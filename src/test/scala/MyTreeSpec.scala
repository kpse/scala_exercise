import org.scalatest.FunSpec

import scala.annotation.tailrec

class MyTreeSpec extends FunSpec {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  describe("My Tree") {
    it("should count its node") {
      def size[A](tree: Tree[A]): Int = tree match {
        case Branch(l,r) => size(l) + size(r)
        case Leaf(v) => 1
      }
      assert(size(Leaf("a")) == 1)
      assert(size(Branch(Leaf("a"), Leaf("b"))) == 2)
      assert(size(Branch(Leaf("a"), Branch(Leaf("c"), Leaf("d")))) == 3)
    }
  }
}
