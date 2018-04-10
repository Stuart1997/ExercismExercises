import org.scalatest.{Matchers, WordSpec}
import wordcount._

class WordCountSpecs extends WordSpec with Matchers {
  "A string containing only unique words" should {
    "return each word mapped with 1" in {
      val string = "My name is Stuart"
      val expectedResult = Some(Map("my" -> 1, "name" -> 1, "is" -> 1, "stuart" -> 1))
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }

  "A string containing duplicate words (only appearing twice)" should {
    "return each word that is duplicated mapped with 2, and the others with 1" in {
      val string = "My name is Stuart, what is your name?"
      val expectedResult = Some(Map("name" -> 2, "is" -> 2, "my" -> 1, "stuart" -> 1, "what" -> 1, "your" -> 1))
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }

  "A string containing only one word 10 times" should {
    "return only that word mapped with 10" in {
      val string = "Mine mine mine mine mine mine mine mine mine mine"
      val expectedResult = Some(Map("mine" -> 10))
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }

  "A string in which there is more than a single space" should {
    "reduce that whitespace to a single space" in {
      val string = "My  name   is    Stuart"
      val expectedResult = Some(Map("my" -> 1, "name" -> 1, "is" -> 1, "stuart" -> 1))
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }

  "An empty string" should {
    "return none" in {
      val string = ""
      val expectedResult = None
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }

  "A string containing only whitespace" should {
    "return none" in {
      val string = "    "
      val expectedResult = None
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }

  "A string containing only non-a-z characters" should {
    "return none" in {
      val string = "£123,456.78!?"
      val expectedResult = None
      WordCount.CountWords(string) shouldBe expectedResult
    }
  }
}
