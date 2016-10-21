import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec

class EitherSpec extends FunSpec {

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[E, A](e: E) extends Either[E, A] {
    override def map[B](f: (A) => B): Either[E, B] = Left(e)

    override def flatMap[EE >: E, B](f: (A) => Either[EE, B]): Either[EE, B] = Left(e)

    override def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = b

    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = Left(e)
  }

  case class Right[E, A](a: A) extends Either[E, A] {
    override def map[B](f: (A) => B): Either[E, B] = Right(f(a))

    override def flatMap[EE >: E, B](f: (A) => Either[EE, B]): Either[EE, B] = f(a)

    override def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = Right(a)

    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(a, r))

    }
  }

  describe("Either1") {
    // 4-6
    it("should map like Either") {
      assert((Left(1) : Either[Int, String]).map{ a : String => s"${a} is the value"} == Left(1))
      assert((Right("right") : Either[Int, String]).map{ a : String => s"${a} is the value"} == Right("right is the value"))

      assert((Left(1) : Either[Int, String]).flatMap{ a : String => Right(s"${a} is the value") : Either[Int, String]} == Left(1))
      assert((Right("right") : Either[Int, String]).flatMap{ a : String => Right(s"1") : Either[Int, String]} == Right("1"))

      assert((Left(1) : Either[Int, String]).orElse(Right("1")) == Right("1"))
      assert((Right("right") : Either[Int, String]).orElse(Right("1")) == Right("right"))

      assert((Left(1) : Either[Int, String]).map2(Right("1"))(_ + _) == Left(1))
      assert((Right("right") : Either[Int, String]).map2(Right("1"))(_ + _) == Right("right1"))
      assert((Right("right") : Either[Int, String]).map2(Left(2))(_ + _) == Left(2))

    }



  }
}
