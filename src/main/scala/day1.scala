import scala.language.implicitConversions

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
}
