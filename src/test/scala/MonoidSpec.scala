import org.scalatest.FunSpec

class MonoidSpec extends FunSpec {
  describe("Monoid") {
    trait Monoid[A] {
      def op(a: A, b: A): A

      def zero: A
    }
    // 10-1
    it("should implement for Int Addition") {
      val intAddition: Monoid[Int] = new Monoid[Int] {
        override def op(a: Int, b: Int): Int = a + b

        override def zero: Int = 0
      }

      assert(intAddition.op(1, 2) == 3)
      assert(intAddition.zero == 0)
    }

    it("should implement for intMultiplication") {
      val intMultiplication: Monoid[Int] = new Monoid[Int] {
        override def op(a: Int, b: Int): Int = a * b

        override def zero: Int = 1
      }

      assert(intMultiplication.op(1, 2) == 2)
      assert(intMultiplication.zero == 1)
    }

    it("should implement for boolean or") {
      val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a: Boolean, b: Boolean): Boolean = a || b

        override def zero: Boolean = false
      }

      assert(booleanOr.op(true, true) == true)
      assert(booleanOr.op(false, true) == true)
      assert(booleanOr.zero == false)
    }

    it("should implement for boolean and") {
      val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a: Boolean, b: Boolean): Boolean = a && b

        override def zero: Boolean = true
      }

      assert(booleanAnd.op(true, true) == true)
      assert(booleanAnd.op(false, true) == false)
      assert(booleanAnd.zero == true)
    }

    // 10-2
    it("should implement for option") {
      def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
        override def op(a: Option[A], b: Option[A]): Option[A] = a orElse b

        override def zero: Option[A] = None
      }

      assert(optionMonoid.op(None, None).isEmpty)
      assert(optionMonoid.op(Some(1), None).contains(1))
      assert(optionMonoid.zero.isEmpty)
    }

    // 10-3
    it("should implement for endofunctor") {
      def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
        override def op(a: (A) => A, b: (A) => A): (A) => A = a andThen b

        override def zero: (A) => A = identity
      }

      assert(endoMonoid[Int].op(_ + 1, _ - 1)(1) == 1)
      assert(endoMonoid.zero(2) == 2)
    }

    // 10-5
    it("should work with flatMap") {
      def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero) {
        (a: B, b: A) =>
          try m.op(a, f(b))
          catch {
            case e: Exception => a
          }
      }


      val intAddition: Monoid[Int] = new Monoid[Int] {
        override def op(a: Int, b: Int): Int = a + b

        override def zero: Int = 0
      }

      assert(foldMap(List("1"), intAddition)(_.toInt) == 1)
      assert(foldMap(List("1", "a"), intAddition)(_.toInt) == 1)
      assert(foldMap(List("1", "a", "3"), intAddition)(_.toInt) == 4)
    }
  }

}
