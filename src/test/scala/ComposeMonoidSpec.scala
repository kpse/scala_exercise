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
    // 10-16
    it("should compose each other") {
      def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        override def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1, b._1), B.op(a._2, b._2))

        override def zero: (A, B) = (A.zero, B.zero)
      }

      assert(productMonoid(intAddition, intMultiply).op((1, 2), (3, 4)) == (4, 8))
      assert(productMonoid(intAddition, intAddition).op((1, 2), (3, 4)) == (4, 6))
    }

    // 10-17
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

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.size match {
      case l if l > 1 => m.op(foldMapV(v.take(l / 2), m)(f), foldMapV(v.drop(l / 2), m)(f))
      case 1 => f(v.head)
      case 0 => m.zero
    }

    // 10-18
    it("should implement bag") {
      def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
        val M: Monoid[Map[A, Int]] = new Monoid[Map[A, Int]] {
          override def op(a: Map[A, Int], b: Map[A, Int]): Map[A, Int] =
            (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
              acc.updated(k, a.getOrElse(k, 0) + b.getOrElse(k, 0))
            }

          override def zero: Map[A, Int] = Map()
        }
        foldMapV(as, M)((a) => Map(a -> 1))
      }

      assert(bag(Vector("a", "rose", "is", "a", "rose")) ==  Map("a" -> 2, "rose" -> 2, "is" -> 1))
      assert(bag(Vector("a", "rose", "is", "a", "rose", "rose")) ==  Map("a" -> 2, "rose" -> 3, "is" -> 1))
    }

  }

}
