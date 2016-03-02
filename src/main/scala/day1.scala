import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.ClassTag

object day1 {
  object case1 {
    import scalaz._
    import scalaz.Scalaz._

    sealed trait TrafficLight {}

    case object Red extends TrafficLight
    case object Yellow extends TrafficLight
    case object Green extends TrafficLight

    implicit val eqTL: Equal[TrafficLight] = Equal.equal(_ == _)

    val x: TrafficLight = Red

    x === Yellow.asInstanceOf[TrafficLight]
    x === Red
    x === Yellow

    implicit def TraitToSuper(self: TrafficLight)(implicit F: Equal[TrafficLight]) = F

    Red.asInstanceOf[TrafficLight] === Yellow
    // NOPE Red === Yellow
  }

  object case2 {
    import scalaz._
    import scalaz.Scalaz._

    sealed class TrafficLight

    class Red extends TrafficLight
    class Yellow extends TrafficLight
    class Green extends TrafficLight

    val r = new Red
    val y = new Yellow
    val g = new Green

    implicit val eqTL: Equal[TrafficLight] = Equal.equal(_ == _)
    implicit def RedToTL(self: Red): TrafficLight = self

    implicit class Red2(val self: Red)(implicit F: Equal[TrafficLight]) { def x = 1 }
    implicit val eqTL2: Equal[Red2] = Equal.equal(_ == _)

    // NOT INT implicit val eqTL3: Equal[Red] = Equal.equal(_ == _)
    // NOT INT r === r

    r.x

    // STILL NOPE r === r
    // Why it is failing: Red -> (class) Red2 -> (val) Equal[Red2] -> EqualOps[Red2]
  }

  private[this] object case3 {
    // http://docs.scala-lang.org/tutorials/FAQ/finding-implicits

    // http://docs.scala-lang.org/tutorials/FAQ/chaining-implicits.html
    // Scala does not allow two such implicit conversions taking place, however, so one cannot got from A to C using an implicit A to B and another implicit B to C. Is there a way around this restriction?
    // However, if an implicit definition requires an implicit parameter itself, Scala will look for additional implicit values for as long as needed.

    sealed trait A
    case object AA extends A
    trait B[F] { def x = 1 }
    trait C[F] { def y = 2 }

    implicit class ToB(val self: A) extends B[A]

    implicit def toC[F](a: B[F]): C[F] = new C[F] {}
    implicit class ToC[F](val self: B[F]) extends C[F]
    implicit class Z(val self: B[A]) { val z = 3}
    implicit val vToC: C[A] = new C[A] {}

    // AA.y (facepalm)
    // AA.z (facepalm)
  }

  object case4 {
    import scalaz._
    import scalaz.Scalaz._

    sealed trait TrafficLight {}

    case class Red() extends TrafficLight
    case class Yellow() extends TrafficLight
    case class Green() extends TrafficLight

    implicit val eqTL: Equal[TrafficLight] = Equal.equal(_ == _)

    final class EqualOps[F] (val self: F)(implicit val F: Equal[F]) {
      final def ====(other: F): Boolean = F.equal(self, other)
    }

    // DO NOT HELP: implicit def ToEqualOps[F, F0 >: F](v: F)(implicit F0: Equal[F0]): EqualOps[F0] = new EqualOps[F0](v)
    implicit def ToEqualOps1(v: Red) = new EqualOps[TrafficLight](v)

    Red() ==== Yellow()
  }

  object case5 {
    sealed trait A
    case object AA extends A

    trait B[F] { def t(implicit T: ClassTag[F]) = T.toString() }

    implicit class ToB(val self: A) extends B[A]

    assert { AA.t == "A" }
  }

  object case6 {
    sealed trait TrafficLight {}

    case class Red() extends TrafficLight
    case class Yellow() extends TrafficLight
    case class Green() extends TrafficLight

    trait Equal[-F] { self =>
      def equal(a1: F, a2: F): Boolean
    }

    // val rEq: Equal[Red] = new Equal[TrafficLight] { override def equal(a1: TrafficLight, a2: TrafficLight): Boolean = ??? }
  }
}
