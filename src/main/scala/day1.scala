object day1 {
  sealed trait TrafficLight {}
  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight

  /**
    * Conditional.
    *
    * {{{
    * p q  p --> q
    * 0 0  1
    * 0 1  1
    * 1 0  0
    * 1 1  1
    * }}}
    */
  final def conditional(p: Boolean, q: => Boolean) = !p || q

  // Our EqualCV is contravariant
  trait EqualCV[-F] { self =>
    def equal(a1: F, a2: F): Boolean

    /** @return true, if `equal(f1, f2)` is known to be equivalent to `f1 == f2` */
    def equalIsNatural: Boolean = false

    trait EqualLaw {
      def commutative(f1: F, f2: F): Boolean = equal(f1, f2) == equal(f2, f1)
      def reflexive(f: F): Boolean = equal(f, f)
      def transitive(f1: F, f2: F, f3: F): Boolean = {
        conditional(equal(f1, f2) && equal(f2, f3), equal(f1, f3))
      }
      def naturality(f1: F, f2: F): Boolean = {
        conditional(equalIsNatural, equal(f1, f2) == (f1 == f2))
      }
    }
    def equalLaw = new EqualLaw {}

    // val equalSyntax = new scalaz.syntax.EqualSyntax[F] { def F = EqualCV.this }
  }

  object EqualCV {
    @inline def apply[F](implicit F: EqualCV[F]): EqualCV[F] = F

    ////
    /** Creates an Equal instance based on universal equality, `a1 == a2` */
    def equalA[A]: EqualCV[A] = new EqualCV[A] {
      def equal(a1: A, a2: A): Boolean = a1 == a2
      override def equalIsNatural: Boolean = true
    }

    /** Creates an Equal instance based on reference equality, `a1 eq a2` */
    def equalRef[A <: AnyRef]: EqualCV[A] = new EqualCV[A] {
      def equal(a1: A, a2: A): Boolean = a1 eq a2
    }

    def equal[A](f: (A, A) => Boolean): EqualCV[A] = new EqualCV[A] {
      def equal(a1: A, a2: A) = f(a1, a2)
    }
  }

  implicit val trafficLightEquality: EqualCV[TrafficLight] = EqualCV.equal(_ == _)

  // TODO Red ===== Green via EqualCVOps
}
