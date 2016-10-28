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

      assert(booleanOr.op(true, true))
      assert(booleanOr.op(false, true))
      assert(!booleanOr.zero)
    }

    it("should implement for boolean and") {
      val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a: Boolean, b: Boolean): Boolean = a && b

        override def zero: Boolean = true
      }

      assert(booleanAnd.op(true, true))
      assert(!booleanAnd.op(false, true))
      assert(booleanAnd.zero)
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
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(a: (A) => A, b: (A) => A): (A) => A = a andThen b

      override def zero: (A) => A = identity
    }

    it("should implement for endofunctor") {
      assert(endoMonoid[Int].op(_ + 1, _ - 1)(1) == 1)
      assert(endoMonoid.zero(2) == 2)
    }

    // 10-5
    it("should work with foldMap") {
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

    // 10-6
    it("should implement foldLeft with foldMap") {
      def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as match {
        case List() => m.zero
        case (x :: xs) =>
          try m.op(f(x), foldMap(xs, m)(f))
          catch {
            case e: Exception => m.op(m.zero, foldMap(xs, m)(f))
          }
      }
      def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(m => f(_, m))(z)

      assert(foldLeft(List(1))(0)(_ + _) == 1)
      assert(foldLeft(List(1, 2))(0)(_ + _) == 3)
      assert(foldLeft(List[Int]())(0)(_ + _) == 0)
      assert(foldLeft(List(3, 2))(1)(_ + _) == 6)
    }

    it("should have foldMap for IndexedSeq") {
      def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.size match {
        case l if l > 1 => m.op(foldMapV(v.take(l / 2), m)(f), foldMapV(v.drop(l / 2), m)(f))
        case 1 => f(v.head)
        case 0 => m.zero
      }

      val intAddition: Monoid[Int] = new Monoid[Int] {
        override def op(a: Int, b: Int): Int = a + b

        override def zero: Int = 0
      }

      assert(foldMapV(IndexedSeq(3, 2), intAddition)(identity) == 5)
      assert(foldMapV(IndexedSeq(), intAddition)(identity) == 0)
      assert(foldMapV(IndexedSeq(1, 2, 3, 7, 8), intAddition)(identity) == 21)
    }

    sealed trait WC
    case class Stub(chars: String) extends WC
    case class Part(lStub: String, words: Int, rStub: String) extends WC

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override def op(a: WC, b: WC): WC = (a, b) match {
        case (Stub(x), Stub(y)) => Stub(x + y)
        case (Stub(x), Part(l, w, r)) => Part(x + l, w, r)
        case (Part(l, w, r), Stub(x)) => Part(l, w, r + x)
        case (Part(l, w, r), Part(l1, w1, r1)) if (r + l1).isEmpty => Part(l, w + w1, r1)
        case (Part(l, w, r), Part(l1, w1, r1)) => Part(l, w + w1 + 1, r1)
      }

      override def zero: WC = Stub("")
    }

    it("should implement wcMonoid") {
      assert(wcMonoid.zero == Stub(""))
      assert(wcMonoid.op(wcMonoid.zero, Stub("123")) == wcMonoid.op(Stub("123"), wcMonoid.zero))
      assert(wcMonoid.op(Stub("123"), Stub("123")) == Stub("123123"))
      assert(wcMonoid.op(Stub("123"), Part("", 2, "1")) == Part("123", 2, "1"))
    }

    it("should implement word counting by wcMonoid") {
      def countWord(input: String): Int = input.foldLeft(wcMonoid.zero) {
        (acc, i) => wcMonoid.op(acc, i match {
          case ' ' => Part("", 0, "")
          case x => Stub(s"$x")
        })
      } match {
        case Stub("") => 0
        case Stub(x) => 1
        case Part(l, w, r) => w + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
      }

      assert(countWord("") == 0)
      assert(countWord("123") == 1)
      assert(countWord("123 321") == 2)
      assert(countWord("123 22 321") == 3)
      assert(countWord("1 1 2 3") == 4)

    }
  }

}
