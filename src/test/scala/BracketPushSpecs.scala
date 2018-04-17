import org.scalatest.{Matchers, WordSpec}
import bracketpush._

class BracketPushSpecs  extends WordSpec with Matchers {
  "A string containing no brackets" should {
    "return false" in {
      val string = ""
      BracketPush.bracketsArePaired(string) shouldBe false
    }
  }

  "A string containing a pair of brackets" should {
    "return true" in {
      val string = "[]"
      BracketPush.bracketsArePaired(string) shouldBe true
    }
  }

  "A string containing all brackets in a correctly nested order" should {
    "return true" in {
      val string = "{()}[]"
      BracketPush.bracketsArePaired(string) shouldBe true
    }
  }

  "A string containing all brackets in an incorrectly nested" should {
    "return false" in {
      val string = "{[}]()"
      BracketPush.bracketsArePaired(string) shouldBe false
    }
  }

  "A string with unclosed brackets" should {
    "return false" in {
      val string = "{{{{}}"
      BracketPush.bracketsArePaired(string) shouldBe false
    }
  }

  "A string with unopened brackets" should {
    "return false" in {
      val string = "()))"
      BracketPush.bracketsArePaired(string) shouldBe false
    }
  }

  "A string with valid pairing and " should {
    "return true" in {
      val string = "({[]({})})"
      BracketPush.checkValidity(string) shouldBe true
    }
  }
}
