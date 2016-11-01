import org.scalatest.FunSpec

class FoldableSpec extends FunSpec {

  trait Monoid[A] {
    def op(a: A, b: A): A

    def zero: A
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  describe("Foldable") {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def op(a: Int, b: Int): Int = a + b

      override def zero: Int = 0
    }
    object ListFoldable extends Foldable[List] {
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
        case List() => mb.zero
        case (x :: xs) => mb.op(f(x), foldMap(xs)(f)(mb))
      }
    }

    it("should have Foldable[List]") {
      assert(ListFoldable.foldLeft(List(1))(0)(_ + _) == 1)
      assert(ListFoldable.foldRight(List(1))(0)(_ + _) == 1)
      assert(ListFoldable.foldMap(List(1, 2, 3))(_ + 0)(intAddition) == 6)
    }

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.size match {
      case l if l > 1 => m.op(foldMapV(v.take(l / 2), m)(f), foldMapV(v.drop(l / 2), m)(f))
      case 1 => f(v.head)
      case 0 => m.zero
    }

    object IndexSeqFoldable extends Foldable[IndexedSeq] {
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
    }

    it("should have Foldable[IndexSeq]") {
      assert(IndexSeqFoldable.foldLeft(Array(1))(0)(_ + _) == 1)
      assert(IndexSeqFoldable.foldRight(Array(1))(0)(_ + _) == 1)
      assert(IndexSeqFoldable.foldMap(Array(1, 2, 3))(_ + 0)(intAddition) == 6)
    }

    object StreamFoldable extends Foldable[Stream] {
      override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
        case Stream() => mb.zero
        case x => mb.op(f(x.head), foldMap(x.tail)(f)(mb))
      }

    }

    it("should have Foldable[Stream]") {
      assert(StreamFoldable.foldLeft(Stream(1))(0)(_ + _) == 1)
      assert(StreamFoldable.foldRight(Stream(1))(0)(_ + _) == 1)
      assert(StreamFoldable.foldMap(Stream(1, 2, 3))(_ + 0)(intAddition) == 6)
    }

  }

}
