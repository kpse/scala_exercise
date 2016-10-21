import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec

import scala.util.Try

class EitherSpec extends FunSpec {

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](e: E) extends Either[E, Nothing] {
    override def map[B](f: (Nothing) => B): Either[E, B] = Left(e)

    override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = Left(e)

    override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = Left(e)
  }

  case class Right[+A](a: A) extends Either[Nothing, A] {
    override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(a))

    override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(a)

    override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = Right(a)

    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(a, r))

    }
  }

  describe("Either1") {
    // 4-6
    it("should map like Either") {
      assert(Left(1).map { a: String => s"${a} is the value" } == Left(1))
      assert(Right("right").map { a: String => s"${a} is the value" } == Right("right is the value"))

      assert(Left(1).flatMap { a: String => Right(s"${a} is the value"): Either[Int, String] } == Left(1))
      assert(Right("right").flatMap { a: String => Right(s"1"): Either[Int, String] } == Right("1"))

      assert(Left(1).orElse(Right("1")) == Right("1"))
      assert(Right("right").orElse(Right("1")) == Right("right"))

      assert(Left(1).map2(Right("1"))((a, b) => s"${a}${b}") == Left(1))
      assert(Right("right").map2(Right("1"))(_ + _) == Right("right1"))
      assert(Right("right").map2(Left(2))(_ + _) == Left(2))

    }

    it("should work with try") {
      def Try[A](a: => A): Either[Exception, A] = try Right(a)
      catch {
        case e: Exception => Left(e)
      }

      def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age / numberOfSpeedingTickets
      def parseInsuranceRateQuote(age: String,
                                  numberOfSpeedingTickets: String): Either[Exception, Double] = for {
        a <- Try { age.toInt }
        tickets <- Try { numberOfSpeedingTickets.toInt }
      } yield insuranceRateQuote(a, tickets)

      assert(parseInsuranceRateQuote("10", "2") == Right(5))
    }


  }
}
