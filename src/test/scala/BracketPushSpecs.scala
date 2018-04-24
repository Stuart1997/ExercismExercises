import org.scalatest.{Matchers, WordSpec}
import bracketpush._

class BracketPushSpecs extends WordSpec with Matchers {
  "The bracketsArePaired regex method" should {
    "return false when a string containing no brackets is passed in" in {
      val string = ""
      BracketPush.bracketsArePaired(string) shouldBe false
    }

    "return true when a string containing a pair of brackets is passed in" in {
      val string = "[]"
      BracketPush.bracketsArePaired(string) shouldBe true
    }

    "return true when a string containing some brackets in correctly nested order is passed in" in {
      val string = "{{{[[[]]]{}}}}"
      BracketPush.bracketsArePaired(string) shouldBe true
    }

    "return true when a string containing all brackets in a correctly nested order is passed in" in {
      val string = "{()}[]"
      BracketPush.bracketsArePaired(string) shouldBe true
    }

    "return false when a string containing all brackets in an incorrectly nested order is passed in" in {
      val string = "{[}]()"
      BracketPush.bracketsArePaired(string) shouldBe false
    }

    "return false when a string with unclosed brackets is passed in" in {
      val string = "{{{{}}"
      BracketPush.bracketsArePaired(string) shouldBe false
    }

    "return false when a string with unopened brackets is passed in" in {
      val string = "()))"
      BracketPush.bracketsArePaired(string) shouldBe false
    }
  }
}
