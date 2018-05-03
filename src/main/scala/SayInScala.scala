package sayinscala

object SayInScala extends App {

  def readNumber(number: Int): String = {
    val oneToNine = Map(
      0 -> "",
      1 -> "one", 2 -> "two", 3 -> "three",
      4 -> "four", 5 -> "five", 6 -> "six",
      7 -> "seven", 8 -> "eight", 9 -> "nine")

    val tenToNineteen = Map(
      10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen",
      15 -> "fifteen", 16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen")

    val multiplesOfTen = Map(
      20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty",
      60 -> "sixty", 70 -> "seventy", 80 -> "eighty", 90 -> "ninety")

    val zeroes = StringBuilder.newBuilder                                 //Makes a mutable list
    val intAsStringList = number.toString.toList                          //Puts the number passed in into a list of strings (chars)
    val toNine = 9 - intAsStringList.size                                 //Works out how many 0s need to be put in front of the number
    for (x <- 1 to toNine) zeroes.append("0")                             //Add that many 0s to a list
    val nineDigits = zeroes.toList ::: intAsStringList                    //Combine those lists so that it becomes e.g. 0000xxxxx
    val firstNonZeroIndex = nineDigits.indexWhere(index => index != '0')  //Finds the first position in which the number actually begins
    val digitGroup = nineDigits.grouped(3).toList                         //Splits the number into 3 groups of 3 digits

    def block3(group: Int, amount: String): String = {
      val ones = digitGroup(group)(2).toString.toInt
      val tens =
        if (digitGroup(group)(1).toString == "1") {
          digitGroup(group)(1).toString + digitGroup(group)(2).toString
        } else if (digitGroup(group)(1).toString == "0") {
          "0"
        } else {
          digitGroup(group)(1).toString + "0"
        }
      val hundred = digitGroup(group).head.toString.toInt
      val hundredWord = if (hundred > 0) oneToNine(hundred) + s" $amount " else ""
      val tensWord =
        if (tens.head == '0') {
          ""
        }
        else if (tens.head == '1') {
          tenToNineteen(tens.toInt)
        }
        else {
          multiplesOfTen(tens.toInt)
        }
      val onesWord = if (tensWord.contains("ten") || tensWord.contains("eleven") || tensWord.contains("twelve") || tensWord.contains("teen")) "" else oneToNine(ones)
      val sentence = s"$hundredWord $tensWord $onesWord"
      sentence
    }

    val result = firstNonZeroIndex match {
      case zero if number == 0 => "zero"
      case millions if firstNonZeroIndex < 3 && firstNonZeroIndex >= 0 => "million" //block1(0, "million")
      case thousands if firstNonZeroIndex < 6 && firstNonZeroIndex >= 3 => "thousand" //block2(1, "thousand")
      case hundreds if firstNonZeroIndex < 9 && firstNonZeroIndex >= 6 => block3(2, "hundred")
    }

    val noMultiSpaces = result.replaceAll("""([ ]{2,})""", " ")      //Removes any instances of 2 or more spaces and makes them a single space
    val noLeadingSpace = noMultiSpaces.replaceAll("""\A\s+""", "")   //Removes any leading whitespace at the beginning of the string
    val noTrailingSpace = noLeadingSpace.replaceAll("""\s+\Z""", "")   //Removes any leading whitespace at the beginning of the string
    noTrailingSpace
  }

  println(readNumber(50))
}
