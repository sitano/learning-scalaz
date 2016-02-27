object day1 {
  sealed trait TrafficLight {}
  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight

  trait Equal[+F] { self =>
    def equal[A >: F](a1: A, a2: A): Boolean
  }

  object Equal {
    @inline def apply[F](implicit F: Equal[F]): Equal[F] = F
  }

  new Equal[TrafficLight] {
    override def equal[A >: TrafficLight](a1: A, a2: A): Boolean = a1 == a2
  }.equal(Red, Green)
}
