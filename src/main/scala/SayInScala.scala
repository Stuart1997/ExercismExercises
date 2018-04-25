
object SayInScala extends App {

  def readNumber(number: Int): String = {
    val oneToNine = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val tenToNineteen = List("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
    val multiplesOf10 = List("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    val higherThan100 = List("hundred", "thousand")

    val digits = number.toString.length
    val numberAsString = number.toString

    val result = digits match {
      case oneDigit if oneDigit == 1 => {
        if (number == 0) "zero"
        else oneToNine(number - 1)
      }

      case twoDigits if twoDigits == 2 => {
        if (number >= 10 && number < 20) tenToNineteen(numberAsString.tail.toInt)
        else {
          if (numberAsString.endsWith("0")) multiplesOf10(numberAsString.head.toInt - 1)
          else multiplesOf10(numberAsString.head.toString.toInt - 2) + " " + oneToNine(numberAsString.tail.toString.toInt - 1)
        }
      }

      case threeDigits if threeDigits == 3 => {
        if (numberAsString.endsWith("00")) oneToNine(numberAsString.head.toString.toInt - 1) + " " + higherThan100.head
        else
          oneToNine(numberAsString.head.toString.toInt - 1) + " " + higherThan100.head + " and " +
          multiplesOf10(numberAsString.charAt(1).toString.toInt - 2) + " " + oneToNine(numberAsString.last.toString.toInt - 1)
      }

      case _ => "Invalid number"
    }

    result
  }
}
