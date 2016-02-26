import scala.language.implicitConversions
import scala.language.higherKinds

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

  @deprecated(message = "deprecated language feature", since = "2.11.0-M2") /* Since Scala 2.11.0-M2 */
  def plus1x[A <% CanPlus1[A]](a: A, b: A): A = a.+(b)

  def plus1y[A](a: CanPlus1[A], b: A): A = a + b

  implicit def int2plus1(self: Int): CanPlus1[Int] = new CanPlus1[Int] {
    override def +(a: Int): Int = self + a
  }

  trait Monoid[A] {
    // mappend mempty x = x
    // mappend x mempty = x
    def mempty: A
    // mappend x (mappend y z) = mappend (mappend x y) z
    def mappend(a: A, b: A): A
    // mconcat = foldr mappend mempty
    // def mconcat(a: Iterable[A]): A = (a foldRight mempty)(mappend)
    def mconcat[B[_] : FoldLeft](a: B[A]) = implicitly[FoldLeft[B]].foldl(mempty)(a)(mappend)
  }

  object Monoid {
    implicit case object MonoidInt extends Monoid[Int] {
      override def mempty = 0

      override def mappend(a: Int, b: Int) = a + b
    }

    implicit case object MonoidString extends Monoid[String] {
      override def mempty = ""

      override def mappend(a: String, b: String) = a + b
    }
  }

  def plusMonoid[A: Monoid](a: A, b: A) = implicitly[Monoid[A]].mappend(a, b)

  trait FoldLeft[F[_]] {
    def foldl[A, B](z: B)(a: F[A])(f: (B, A) => B): B
  }

  object FoldLeft {
    implicit case object FoldLeftIterable extends FoldLeft[Iterable] {
      override def foldl[A, B](z: B)(a: Iterable[A])(f: (B, A) => B) = (a foldLeft z)(f)
    }
  }

  def sumMonoid[A: Monoid](a: Iterable[A]) = implicitly[Monoid[A]] mconcat a

  def sumFoldLeft[A: Monoid, B[_]: FoldLeft](a: B[A]) = {
    val m = implicitly[Monoid[A]]
    implicitly[FoldLeft[B]].foldl(m.mempty)(a)(m.mappend)
  }

  trait MonoidOp[A] {
    val f: Monoid[A]
    val v: A
    def |+|(a: A): A
  }

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    override val f: Monoid[A] = implicitly[Monoid[A]]

    override val v: A = a

    override def |+|(a: A) = f.mappend(this.v, a)
  }
}