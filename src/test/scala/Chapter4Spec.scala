import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec

class Chapter4Spec extends FunSpec {

  trait Option1[+A] {
    def map[B](f: A => B): Option1[B]

    def flatMap[B](f: A => Option1[B]): Option1[B]

    def getOrElse[B >: A](default: => B): B

    def orElse[B >: A](ob: => Option1[B]): Option1[B]

    def filter(f: A => Boolean): Option1[A]
  }

  case class Some1[A](a: A) extends Option1[A] {
    override def map[B](f: (A) => B): Option1[B] = Some1(f(a))

    override def flatMap[B](f: A => Option1[B]): Option1[B] = f(a)

    override def getOrElse[B >: A](default: => B): B = a

    override def orElse[B >: A](ob: => Option1[B]): Option1[B] = this

    override def filter(f: (A) => Boolean): Option1[A] = if (f(a)) Some1(a) else None1
  }

  class None1[A] extends Option1[A] {
    override def map[B](f: (A) => B): Option1[B] = None1

    override def flatMap[B](f: (A) => Option1[B]): Option1[B] = None1

    override def getOrElse[B >: A](default: => B): B = default

    override def orElse[B >: A](ob: => Option1[B]): Option1[B] = ob

    override def filter(f: (A) => Boolean): Option1[A] = None1

  }

  object None1 extends None1 {

  }

  describe("Option1") {
    // 4-1
    it("should behave like Option") {

      assert(Some1(1).getOrElse(2) == 1)
      assert(None1.getOrElse(2) == 2)

      assert(Some1(1).map(_ + 1) == Some1(2))
      assert((None1: Option1[Int]).map(_ + 1) == None1)

      def optionalFunc(a: Int): Option1[Int] = if (a > 0) Some1(1) else None1

      assert(Some1(1).flatMap(optionalFunc) == Some1(1))
      assert((None1: Option1[Int]).flatMap(optionalFunc) == None1)


      assert(Some1(1).filter(_ > 1) == None1)
      assert(Some1(1).filter(_ > 0) == Some1(1))
      assert((None1: Option1[Int]).filter(_ > 1) == None1)
      assert((None1: Option1[Int]).filter(_ > 0) == None1)

      assert(Some1(1).orElse(None1) == Some1(1))
      assert(None1.orElse(None1) == None1)
      assert(None1.orElse(Some1(99)) == Some1(99))

    }

    // 4-2
    it("should implement variance") {
      implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

      def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)
      def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

      assert(variance(Seq(1, 1, 1)) === Some(0.0))
      assert(variance(Seq(1, 2, 3)) === Some(0.6666666666666666))
    }

    // 4-3
    it("should combine options with map2") {
      def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
        case (Some(x), Some(y)) => Some(f(x, y))
        case _ => None
      }


      assert(map2(Some(1), Some(1))(_ + _) == Some(2))
      assert(map2(Some(1), None)(_ + _) == None)
      assert(map2(None: Option[Int], None)(_ + _) == None)
    }

    // 4-4
    it("should have sequence") {
      def sequence[A](a: List[Option[A]]): Option[List[A]] = if (a.contains(None)) None else Some(a.map { case Some(x) => x })

      assert(sequence(List(None)) == None)
      assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
      assert(sequence(List(Some(1), None)) == None)
    }

    // 4-4
    it("should have traverse") {
      def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
        case (Some(x), Some(y)) => Some(f(x, y))
        case _ => None
      }
      def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case List() => Some(List())
        case x::xs => map2(f(x), traverse(xs)(f))(_ :: _)
      }

      assert(traverse(List(1))(a => if (a>0) Some(a) else None) == Some(List(1)))
      assert(traverse(List(0))(a => if (a>0) Some(a) else None) == None)
      assert(traverse(List(1, 0))(a => if (a>0) Some(a) else None) == None)
      assert(traverse(List(1, 2))(a => if (a>0) Some(a) else None) == Some(List(1, 2)))

    }

  }
}
