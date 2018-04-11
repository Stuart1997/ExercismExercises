import org.scalatest.{Matchers, WordSpec}
import isogram._

class IsogramSpecs extends WordSpec with Matchers {
  "A string without repeating letters" should {
    "return true" in {
      val string = "Lumberjacks"
      Isogram.isAnIsogram(string) shouldBe true
    }
  }

  "A string without repeating letters but with multiple hyphens" should {
    "return true" in {
      val string = "a-b-c-d-e"
      Isogram.isAnIsogram(string) shouldBe true
    }
  }

  "A string without repeating letters but with numbers" should {
    "return true" in {
      val string = "a1 b2 c3 d4 e5"
      Isogram.isAnIsogram(string) shouldBe true
    }
  }

  "A string with repeating letters" should {
    "return false" in {
      val string = "Hello"
      Isogram.isAnIsogram(string) shouldBe false
    }
  }

  "An empty string" should {
    "return false" in {
      val string = ""
      Isogram.isAnIsogram(string) shouldBe false
    }
  }

  "A string containing only spaces" should {
    "return false" in {
      val string = "   "
      Isogram.isAnIsogram(string) shouldBe false
    }
  }

  "A string containing only hyphens" should {
    "return false" in {
      val string = "------"
      Isogram.isAnIsogram(string) shouldBe false
    }
  }

  "A string containing only non-whitespace/hyphen/a-z characters" should {
    "return false" in {
      val string = "£123,456.78!?"
      Isogram.isAnIsogram(string) shouldBe false
    }
  }
}
