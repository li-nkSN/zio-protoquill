package io.getquill

import scala.language.implicitConversions
import scala.reflect.ClassTag

import io.getquill.context.ExecutionType
import io.getquill.context.mirror.MirrorSession
import io.getquill.context.mirror.Row

/** Regression sentinels for quat-driven SQL generation of dynamic (infix)
  * entity queries.
  *
  * Background: QuatMaking caches were object-level and keyed by TypeRepr,
  * whose equality/hashing is only defined within a single macro expansion.
  * A stale/colliding entry could resolve a Product row type to a wrong quat,
  * collapsing the SELECT projection of a dynamic infix query to positional
  * form; rows then decoded against physical table column order (observed in
  * production as an enum decoder receiving a neighboring column's value).
  * The poisoning depends on compiler-session state and cannot be triggered
  * hermetically here; these tests lock the user-visible contract that the
  * fix protects.
  */
object DynamicProjectionFixture {
  // single-column case class with its own Specific decoder (like a PG range type)
  case class Period(start: Int, end: Int)

  // two different enum-like types decoded via one polymorphic implicit def
  // (mirrors enumeratum's `enumDecoder[A <: EnumEntry](implicit e: Enum[A])`)
  sealed trait SourceE
  object SourceE {
    case object BOOKING extends SourceE
    val values: List[SourceE] = List(BOOKING)
  }
  sealed trait OccupancyE
  object OccupancyE {
    case object OCCUPIED extends OccupancyE
    val values: List[OccupancyE] = List(OCCUPIED)
  }
  trait EnumComp[A] { def withName(s: String): A }
  object EnumComp {
    given EnumComp[SourceE] = (s: String) =>
      SourceE.values.find(_.toString == s).getOrElse(throw new NoSuchElementException(s"$s is not a member of SourceE"))
    given EnumComp[OccupancyE] = (s: String) =>
      OccupancyE.values.find(_.toString == s).getOrElse(throw new NoSuchElementException(s"$s is not a member of OccupancyE"))
  }

  case class Block(
      id: String,
      period: Period,
      reason: String,
      source: SourceE,
      sourceId: Option[String],
      occupancy: OccupancyE,
      deleted: Boolean
  )
}

class QuatDynamicProjectionRegressionSpec extends Spec {
  import DynamicProjectionFixture._

  val ctx = new MirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal)
  import ctx.{_, given}

  implicit val periodDecoder: Decoder[Period] = decoder[Period]
  implicit def enumDecoder[A](implicit comp: EnumComp[A], ct: ClassTag[A]): Decoder[A] =
    MirrorDecoder((index, row, session) => comp.withName(row[String](index)))

  val s = MirrorSession.default

  "dynamic infix entity query keeps the explicit field projection" in {
    // runtime splice => the query cannot be compiled statically
    val order = "ASC"
    inline def q = quote {
      sql"""SELECT b.* FROM blocks b ORDER BY b.id #$order""".as[Query[Block]]
    }
    val result = ctx.run(q)

    result.info.executionType mustEqual ExecutionType.Dynamic
    // a Product quat must expand to its field list, never a positional `x.*`
    result.string must include("x.id")
    result.string must include("x.period")
    result.string must include("x.source")
    result.string must include("x.sourceId")
    result.string must include("x.occupancy")
    result.string must not include ("x.*")
  }

  "extractor of the dynamic infix query decodes fields by declared order" in {
    val order = "ASC"
    inline def q = quote {
      sql"""SELECT b.* FROM blocks b ORDER BY b.id #$order""".as[Query[Block]]
    }
    val result = ctx.run(q)

    val row = Row(
      "id" -> "abc",
      "period" -> Period(1, 2),
      "reason" -> "why-not",
      "source" -> "BOOKING",
      "sourceId" -> "sid",
      "occupancy" -> "OCCUPIED",
      "deleted" -> false
    )
    result.extractor(row, s) mustEqual Block(
      "abc",
      Period(1, 2),
      "why-not",
      SourceE.BOOKING,
      Some("sid"),
      OccupancyE.OCCUPIED,
      false
    )
  }

  "static entity query also keeps the explicit projection" in {
    inline def q = quote { query[Block] }
    val result = ctx.run(q)
    result.string must include("x.id")
    result.string must include("x.occupancy")
    result.string must not include ("x.*")
  }
}
