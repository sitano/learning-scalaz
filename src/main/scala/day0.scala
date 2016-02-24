import scala.language.implicitConversions

object day0 {
  def head[A](a: List[A]) = a.head

  def head[A](a: Seq[A]) = a.head

  trait CanPlus2[A] {
    def +(a: A, b: A): A
  }

  trait CanPlus1[A] {
    def +(a: A): A
  }

  implicit case object IntPlus2 extends CanPlus2[Int] {
    override def +(a: Int, b: Int): Int = a + b
  }

  implicit object StringPlus2 extends CanPlus2[String] {
    override def +(a: String, b: String): String = a + b
  }

  // Since Scala 2.8, context bounds
  def plus2x[A](a: A, b: A)(implicit e: CanPlus2[A]): A = e.+(a, b)

  // Since Scala 2.8, context bounds
  def plus2y[A : CanPlus2](a: A, b: A) = implicitly[CanPlus2[A]].+(a, b)

  @deprecated /* Since Scala 2.11.0-M2 */
  def plus1x[A <% CanPlus1[A]](a: A, b: A): A = a.+(b)

  def plus1y[A](a: CanPlus1[A], b: A): A = a + b

  implicit def int2plus1(self: Int): CanPlus1[Int] = new CanPlus1[Int] {
    override def +(a: Int): Int = self + a
  }
}