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

  "40" should {
    "return 'forty'" in {
      val number = 40
      SayInScala.readNumber(number) shouldBe "forty"
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

  "123" should {
    "return 'one hundred twenty three'" in {
      val number = 123
      SayInScala.readNumber(number) shouldBe "one hundred twenty three"
    }
  }

  "1234" should {
    "return 'one thousand two hundred thirty four'" in {
      val number = 1234
      SayInScala.readNumber(number) shouldBe "one thousand two hundred thirty four"
    }
  }

  "12345" should {
    "return 'twelve thousand three hundred forty five'" in {
      val number = 12345
      SayInScala.readNumber(number) shouldBe "twelve thousand three hundred forty five"
    }
  }

  "123456" should {
    "return 'one hundred twenty three thousand four hundred fifty six'" in {
      val number = 123456
      SayInScala.readNumber(number) shouldBe "one hundred twenty three thousand four hundred fifty six"
    }
  }

  "100006" should {
    "return 'one hundred thousand six'" in {
      val number = 100006
      SayInScala.readNumber(number) shouldBe "one hundred thousand six"
    }
  }

  "1234567" should {
    "return 'one million two hundred thirty four thousand five hundred sixty seven'" in {
      val number = 1234567
      SayInScala.readNumber(number) shouldBe "one million two hundred thirty four thousand five hundred sixty seven"
    }
  }

  "12345678" should {
    "return 'twelve million three hundred forty five thousand six hundred seventy eight'" in {
      val number = 12345678
      SayInScala.readNumber(number) shouldBe "twelve million three hundred forty five thousand six hundred seventy eight"
    }
  }

  "123456789" should {
    "return 'one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine'" in {
      val number = 123456789
      SayInScala.readNumber(number) shouldBe "one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine"
    }
  }

  "100000009" should {
    "return 'one hundred million nine'" in {
      val number = 100000009
      SayInScala.readNumber(number) shouldBe "one hundred million nine"
    }
  }

  "100050009" should {
    "return 'one hundred million fifty thousand nine'" in {
      val number = 100050009
      SayInScala.readNumber(number) shouldBe "one hundred million fifty thousand nine"
    }
  }
}
