import org.scalatest.{Matchers, WordSpec}
import sayinscala.SayInScala

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
    "return 'eight hundred fifty nine'" in {
      val number = 859
      SayInScala.readNumber(number) shouldBe "eight hundred fifty nine"
    }
  }

  "1000" should {
    "return 'one thousand'" in {
      val number = 1000
      SayInScala.readNumber(number) shouldBe "one thousand"
    }
  }

  "1100" should {
    "return 'one thousand one hundred'" in {
      val number = 1100
      SayInScala.readNumber(number) shouldBe "one thousand one hundred"
    }
  }

  "1110" should {
    "return 'one thousand one hundred ten'" in {
      val number = 1110
      SayInScala.readNumber(number) shouldBe "one thousand one hundred ten"
    }
  }

  "1111" should {
    "return 'one thousand one hundred eleven'" in {
      val number = 1111
      SayInScala.readNumber(number) shouldBe "one thousand one hundred eleven"
    }
  }

  "1001" should {
    "return 'one thousand one'" in {
      val number = 1001
      SayInScala.readNumber(number) shouldBe "one thousand one"
    }
  }

  "1010" should {
    "return 'one thousand ten'" in {
      val number = 1010
      SayInScala.readNumber(number) shouldBe "one thousand ten"
    }
  }

  "1011" should {
    "return 'one thousand eleven'" in {
      val number = 1011
      SayInScala.readNumber(number) shouldBe "one thousand eleven"
    }
  }

  "10000" should {
    "return 'ten thousand'" in {
      val number = 10000
      SayInScala.readNumber(number) shouldBe "ten thousand"
    }
  }

  "11000" should {
    "return 'eleven thousand'" in {
      val number = 11000
      SayInScala.readNumber(number) shouldBe "eleven thousand"
    }
  }

  "12300" should {
    "return 'twelve thousand three hundred'" in {
      val number = 12300
      SayInScala.readNumber(number) shouldBe "twelve thousand three hundred"
    }
  }

  "12340" should {
    "return 'twelve thousand three hundred forty'" in {
      val number = 12340
      SayInScala.readNumber(number) shouldBe "twelve thousand three hundred forty"
    }
  }

  "12345" should {
    "return 'twelve thousand three hundred forty five'" in {
      val number = 12345
      SayInScala.readNumber(number) shouldBe "twelve thousand three hundred forty five"
    }
  }
}
