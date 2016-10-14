import org.scalatest.FunSpec

import scala.annotation.tailrec

class MyTreeSpec extends FunSpec {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  describe("My Tree") {
    it("should count its node") {
      def size[A](tree: Tree[A]): Int = tree match {
        case Branch(l, r) => size(l) + size(r)
        case Leaf(v) => 1
      }
      assert(size(Leaf("a")) == 1)
      assert(size(Branch(Leaf("a"), Leaf("b"))) == 2)
      assert(size(Branch(Leaf("a"), Branch(Leaf("c"), Leaf("d")))) == 3)
    }

    it("should count its maximum node") {
      def maximum(tree: Tree[Int]): Int = tree match {
        case Branch(l, r) => maximum(l) max maximum(r)
        case Leaf(v) => v
      }
      assert(maximum(Leaf(1)) == 1)
      assert(maximum(Branch(Leaf(2), Leaf(3))) == 3)
      assert(maximum(Branch(Leaf(4), Branch(Leaf(1), Leaf(9)))) == 9)
    }
  }
}
