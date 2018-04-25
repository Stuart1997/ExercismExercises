import org.scalatest.{Matchers, WordSpec}

class SayInScalaSpecs extends WordSpec with Matchers {
  "0" should {
    "return 'zero'" in {
      val number = 0
      SayInScala.readNumber(number) shouldBe "zero"
    }
  }

  "1" should {
    "return 'one'" in {
      val number = 1
      SayInScala.readNumber(number) shouldBe "one"
    }
  }

  "15" should {
    "return 'fifteen'" in {
      val number = 15
      SayInScala.readNumber(number) shouldBe "fifteen"
    }
  }

  "73" should {
    "return 'seventy three'" in {
      val number = 73
      SayInScala.readNumber(number) shouldBe "seventy three"
    }
  }

  "100" should {
    "return 'one hundred'" in {
      val number = 100
      SayInScala.readNumber(number) shouldBe "one hundred"
    }
  }

  "859" should {
    "return 'eight hundred and fifty nine'" in {
      val number = 859
      SayInScala.readNumber(number) shouldBe "eight hundred and fifty nine"
    }
  }
}
