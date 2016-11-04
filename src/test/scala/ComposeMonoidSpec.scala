import org.scalatest.FunSpec

class ComposeMonoidSpec extends FunSpec {

  trait Monoid[A] {
    def op(a: A, b: A): A

    def zero: A
  }

  describe("Monoid") {
    it("should compose each other") {
      def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        override def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1, b._1), B.op(a._2, b._2))

        override def zero: (A, B) = (A.zero, B.zero)
      }

      val intAddition: Monoid[Int] = new Monoid[Int] {
        override def op(a: Int, b: Int): Int = a + b

        override def zero: Int = 0
      }

      val intMultiply: Monoid[Int] = new Monoid[Int] {
        override def op(a: Int, b: Int): Int = a * b

        override def zero: Int = 1
      }

      assert(productMonoid(intAddition, intMultiply).op((1, 2), (3, 4)) == (4, 8))

    }

  }

}
