import org.scalatest.FunSpec

class ComposeMonoidSpec extends FunSpec {

  trait Monoid[A] {
    def op(a: A, b: A): A

    def zero: A
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b

    override def zero: Int = 0
  }

  val intMultiply: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b

    override def zero: Int = 1
  }

  describe("Monoid") {
    it("should compose each other") {
      def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        override def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1, b._1), B.op(a._2, b._2))

        override def zero: (A, B) = (A.zero, B.zero)
      }



      assert(productMonoid(intAddition, intMultiply).op((1, 2), (3, 4)) == (4, 8))

    }

    it("should compose function monoid") {

      def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
        override def op(a: (A) => B, b: (A) => B): (A) => B = (p: A) => B.op(a(p), b(p))

        override def zero: (A) => B = (p: A) => B.zero
      }


      val op: (String) => Int = functionMonoid(intAddition).op(
        (a: String) => a.toInt,
        (a: String) => a.toInt + 1
      )

      assert(op("1") == 3)
      assert(op("5") == 11)
    }

  }

}
