import org.scalatest.FunSpec

class MonadSpec extends FunSpec {
  describe("Monad") {
    it("should implement Option Monad") {
      val monad: Monad[Option] = Monad.optionMonad

      assert(monad.flatMap(Some(1))(a => Some(a + 1)).contains(2))
      assert(monad.map(Some(1))(_ + 1).contains(2))
    }
  }
}
