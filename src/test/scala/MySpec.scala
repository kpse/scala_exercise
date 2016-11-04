import org.scalatest.FunSpec

import scala.annotation.tailrec
import scala.concurrent.Future

class MySpec extends FunSpec {
  describe("My business") {
    it("should test my stuff") {
      assert(1 == 1)
    }
    it("should determine if the array sorted") {
      def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        as match {
          case arr if arr.length < 2 => true
          case Array(head, second, _*) => ordered(head, second) && isSorted(as.tail, ordered)
        }
      }

      @tailrec
      def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

        as match {
          case arr if arr.length < 2 => true
          case Array(head, second, _*) if !ordered(head, second) => false
          case Array(head, second, _*) => isSorted(as.tail, ordered)
        }
      }



      val sorted: Boolean = isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b)
      assert(sorted)
      val sorted1: Boolean = isSorted(Array(1), (a: Int, b: Int) => a < b)
      assert(sorted1)
      val sorted2: Boolean = isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b)
      assert(!sorted2)
      val sorted21: Boolean = isSorted(Array(3, 2, 1), (a: Int, b: Int) => a > b)
      assert(sorted21)
      val sorted3: Boolean = isSorted(Array(1, 3, 2, 1), (a: Int, b: Int) => a < b)
      assert(!sorted3)
      val sorted4: Boolean = isSorted(Array(1, 3, 2, 1), (a: Int, b: Int) => a > b)
      assert(!sorted4)
    }

    it("should curry function") {
      def curry0[A, B, C](f: (A, B) => C): A => (B => C) = f.curried.apply
      def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

      val curry1: (Int) => (Int) => Int = curry((a: Int, b: Int) => a + b)
      assert(curry1(1)(2) == 3)
    }

    it("should uncurry functions") {
      def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

      val curry1: (Int, Int) => Int = uncurry((a: Int) => (b: Int) => a + b)
      assert(curry1(1, 2) == 3)
      val curry2 = uncurry((a: String) => (b: String) => s"$a-$b")
      assert(curry2("some", "other") == "some-other")
    }

    it("should compose functions") {
      def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

      assert(compose((a: Int) => a + 1, (a: Int) => a - 1)(100) == 100)

      assert(compose((a: Int) => s"this is ${a}", (a: Int) => a + 1)(100) == "this is 101")
    }
  }
  describe("chapter 3") {
    it("should tail a list") {
      def tail[A](l: List[A]): List[A] = l match {
        case List() => List()
        case x :: xs => xs
      }

      assert(tail(List()) == List())
      assert(tail(List(1)) == List(1).tail)
      assert(tail(List(1, 2)) == List(1, 2).tail)
    }

    it("should setHead to a list") {
      def setHead[A](l: List[A], newHead: A): List[A] = l match {
        case List() => List(newHead)
        case x :: xs => newHead :: xs
      }

      assert(setHead(List(), 0) == List(0))
      assert(setHead(List(1), 0) == List(0))
      assert(setHead(List(1, 2), 0) == List(0, 2))
    }

    //3.4
    it("should drop number of element from a list") {
      @tailrec
      def drop[A](l: List[A], n: Int): List[A] = l match {
        case List() => List()
        case x :: xs if n == 1 => xs
        case x :: xs => drop(xs, n - 1)

      }

      assert(drop(List(1, 2, 4), 3) == List())
      assert(drop(List(1, 2, 4), 1) == List(2, 4))
      assert(drop(List(1, 2, 4), 10) == List())
    }

    //3.5
    it("should dropWhile considering given condition from a list") {
      @tailrec
      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case List() => List()
        case x :: xs if !f(x) => x :: xs
        case x :: xs => dropWhile(xs, f)

      }

      assert(dropWhile(List(1, 2, 4), (a: Int) => a > 3) == List(1, 2, 4).dropWhile((a: Int) => a > 3))
      assert(dropWhile(List(1, 2, 4), (a: Int) => a > 0) == List(1, 2, 4).dropWhile((a: Int) => a > 0))
      assert(dropWhile(List(1, 2, 4), (a: Int) => a > 2) == List(1, 2, 4).dropWhile((a: Int) => a > 2))
    }

    //3.9

    it("should compute length by foldRight") {
      def length[A](l: List[A]): Int = l.foldRight(0)((_, x) => x + 1)

      assert(length(List()) == List().length)
      assert(length(List(1)) == List(1).length)
      assert(length(List(111, 111, 111, 111)) == List(1, 2, 3, 4).length)
    }

    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(op: (B, A) => B): B = l match {
      case List() => z
      case x :: xs => foldLeft(xs, op(z, x))(op)
    }

    //3.10
    it("should tail recursive foldLeft") {

      assert(foldLeft(List(), 0)((a: Int, b: Int) => b + a) == 0)
      assert(foldLeft(List(1), 0)((a: Int, b: Int) => b + a) == 1)
      assert(foldLeft(List(111, 111, 111, 111), 0)((a: Int, b: Int) => b + a) == 444)
    }

    //3.11
    it("should implement sum, length, product by foldLeft") {

      def length[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)
      def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
      def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

      assert(sum(List()) == 0)
      assert(length(List()) == 0)
      assert(product(List()) == 1)
      assert(product(List(2)) == 2)
      assert(product(List(2, 3)) == 6)
      assert(length(List(2, 3)) == 2)
      assert(sum(List(2, 3)) == 5)

    }

    //3.12
    it("should reverse list by fold") {


      def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((a, b) => b :: a)

      assert(reverse(List()) == List())
      assert(reverse(List(1)) == List(1))
      assert(reverse(List(1, 2)) == List(2, 1))
      assert(reverse(List(1, 2, 3)) == List(3, 2, 1))

    }

    def foldRight[A, B](l: List[A], z: B)(op: (A, B) => B): B = {
      @tailrec
      def foldLeft[A, B](l: List[A], z: B)(op: (B, A) => B): B = l match {
        case List() => z
        case x :: xs => foldLeft(xs, op(z, x))(op)
      }
      def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((a, b) => b :: a)
      foldLeft(reverse(l), z)((acc, i) => op(i, acc))
    }

    //3.14
    it("should implement foldRight by foldLeft") {
      assert(foldRight(List[Int](), 0)(_ + _) == 0)
      assert(foldRight(List[Int](1, 2, 3), 0)(_ + _) == 6)
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)((a, b) => b :: a)
    //3.15
    it("should append by foldLeft") {

      assert(append(List(), List()) == List())
      assert(append(List(1), List()) == List(1))
      assert(append(List(1), List(2)) == List(1, 2))
      assert(append(List(2), List(1)) == List(2, 1))

    }

    it("should flatten list of lists") {
      def flatten[A](list: List[List[A]]): List[A] = foldRight(list, List[A]()) (_ ::: _)

      assert(flatten(List(List(), List())) == List())
      assert(flatten(List(List(1), List())) == List(1))
      assert(flatten(List(List(1), List(2))) == List(1, 2))
      assert(flatten(List(List(2), List(1))) == List(2, 1))
      assert(flatten(List(List(2))) == List(2))
    }

    import scala.concurrent.ExecutionContext.Implicits.global

    it("should use foreach") {
      def find(criteria: String): Option[String] = Some("criteria")
      def printResult(input: String) = println(input)

      def append1(s: Option[String]): Option[String] = Some(s"append a ${s}")
      def append2(s: Option[String]): Option[String] = Some(s"I have to append another ${s}")

      val composed: (String) => Option[String] = find _ andThen append1 andThen append2
      composed("123") foreach printResult

      def liftOption[A, B](f: A => B): Option[A] => Option[B] = _.map(f(_))
    }

    // 3.16
    it("should add 1 to list items repetitively") {
      def add1ToList(input: List[Int]): List[Int] = input match {
        case x :: xs => (x + 1) :: add1ToList(xs)
        case Nil => List()

      }
      val input: List[Int] = List(1, 2, 3, 4, 5)

      assert(add1ToList(input) == List(2, 3, 4, 5, 6))

    }

    // 3.17
    it("should convert double to string repetitively") {
      def doubleToString(input: List[Double]): List[String] = input match {
        case x :: xs => x.toString :: doubleToString(xs)
        case Nil => List()

      }
      val input: List[Double] = List(1.1, 2.2, 3.3, 4.4, 5.0)

      assert(doubleToString(input) == List("1.1", "2.2", "3.3", "4.4", "5.0"))

    }

    // 3.18
    it("should convert every item by map") {
      def map[A, B](as: List[A])(f: A => B): List[B] = as match {
        case x :: xs => f(x) :: map(xs)(f)
        case Nil => List()

      }
      val input: List[Int] = List(1, 2, 3)

      assert(map(input)(_ + 1) == List(2, 3, 4))

    }

    // 3.19
    it("should remove certain item by filter") {
      def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
        case Nil => List()
        case x :: xs if f(x) => x :: filter(xs)(f)
        case x :: xs => filter(xs)(f)

      }
      val input: List[Int] = List(1, 2, 3)

      assert(filter(input)(_ % 2 == 0) == List(2))

    }

    // 3.20
    it("should implement flatMap") {
      def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
        case Nil => List()
        case x :: xs => f(x) ++ flatMap(xs)(f)

      }
      val input: List[Int] = List(1, 2, 3)

      assert(flatMap(input)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

    }

    // 3.21
    it("should implement filter by flatMap") {
      def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
        case Nil => List()
        case x :: xs => f(x) ++ flatMap(xs)(f)

      }
      def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else List())
      val input: List[Int] = List(1, 2, 3)

      assert(filter(input)(_ % 2 == 0) == List(2))

    }

    // 3.22
    it("should add up two lists") {
      def addUp(input1: List[Int], input2: List[Int]): List[Int] = (input1, input2) match {
        case (Nil, Nil) => List()
        case (_, Nil) => List()
        case (Nil, _) => List()
        case (x :: xs, x2 :: xs2) => x + x2 :: addUp(xs, xs2)
      }

      val input: List[Int] = List(1, 2, 3)
      val input2: List[Int] = List(3, 2, 1)

      assert(addUp(input, input2) == List(4, 4, 4))

    }

    // 3.23
    it("should implement zipWith") {
      def zipWith[A](as: List[A], as2: List[A])(f: (A, A) => A): List[A] = (as, as2) match {
        case (Nil, Nil) => List()
        case (_, Nil) => List()
        case (Nil, _) => List()
        case (x :: xs, x2 :: xs2) => f(x, x2) :: zipWith(xs, xs2)(f)
      }

      val input: List[Int] = List(1, 2, 3)
      val input2: List[Int] = List(3, 2, 2)

      assert(zipWith(input, input2)(_ + _) == List(4, 4, 5))

    }

    // 3.24
    it("should implement hasSubsequnce") {
      def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case tooShort if sup.length < sub.length => false
        case m :: xs if m == sub.head && sub.length == 1 => true
        case x :: xs if x == sub.head => hasSubsequence(xs, sub.tail) || hasSubsequence(xs, sub)
        case x :: xs => hasSubsequence(xs, sub)
      }

      val input: List[Int] = List(1, 2, 3)
      val input2: List[Int] = List(1, 2)

      assert(hasSubsequence(input, input2))
      assert(hasSubsequence(input, List(2, 3)))
      assert(hasSubsequence(input, List(3)))
      assert(!hasSubsequence(input, List(4)))

    }

  }
}
