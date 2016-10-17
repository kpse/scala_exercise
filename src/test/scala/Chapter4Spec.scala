import org.scalatest.FunSpec

class Chapter4Spec extends FunSpec {
  trait Option1[+A] {
    def map[B](f: A => B) : Option1[B]
    def flatMap[B](f: A => Option1[B]) : Option1[B]
    def getOrElse[B >: A](default: => B) : B
    def orElse[B >: A](ob: => Option1[B]) : Option1[B]
    def filter(f: A => Boolean) : Option1[A]
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
      assert((None1 : Option1[Int]).map(_ + 1) == None1)

      def optionalFunc(a: Int) : Option1[Int] =  if (a > 0) Some1(1) else None1

      assert(Some1(1).flatMap(optionalFunc) == Some1(1))
      assert((None1 : Option1[Int]).flatMap(optionalFunc) == None1)


      assert(Some1(1).filter(_ > 1) == None1)
      assert(Some1(1).filter(_ > 0) == Some1(1))
      assert((None1 : Option1[Int]).filter(_ > 1) == None1)
      assert((None1 : Option1[Int]).filter(_ > 0) == None1)

      assert(Some1(1).orElse(None1) == Some1(1))
      assert(None1.orElse(None1) == None1)
      assert(None1.orElse(Some1(99)) == Some1(99))

    }

  }
}
