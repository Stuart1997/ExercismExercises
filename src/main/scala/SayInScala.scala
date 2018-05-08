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

    def block1(group: Int, amount: String): String = {                                    //Beginning with the group of 3 digits for millions
      val millions = digitGroup(group)(2).toString.toInt                                  //In 123,000,000 this would be the 3
      val tensMillions =                                                                  //In 123,000,000 this would be the 2
        if (digitGroup(group)(1).toString == "1")
          digitGroup(group)(1).toString + digitGroup(group)(2).toString                   //If the tens million number is 1, it will be 10-19, so add the millions number to it
         else if (digitGroup(group)(1).toString == "0") "0"                               //If the tens million number is 0, return that
         else digitGroup(group)(1).toString + "0"                                         //If the tens million number > 1 then it will be a 20/30/40, etc, so add the 0 to it

      val hundredMillions = digitGroup(group).head.toString.toInt                         //In 123,000,000 this would be the 1

      val hundredMillionsWord =
        if (hundredMillions > 0)
          if (tensMillions.head.asDigit == 0 && millions == 0)
            oneToNine(hundredMillions) + s" hundred $amount"                              //If the number is X00,XXX,XXX then this group is "x hundred million"
          else oneToNine(hundredMillions) + s" hundred "                                  //Otherwise it will be "x hundred x million"
        else ""

      val tensMillionsWord =
        if (tensMillions.head == '0') ""                                                  //The tens million can be 0 so return a blank string
        else if (tensMillions.head == '1')
          if (hundredMillions == 0) s"${tenToNineteen(tensMillions.toInt)} $amount"       //If it's an 8 digit number, return "10-19 million"
          else ""
        else
          if (millions == 0) s"${multiplesOfTen(tensMillions.toInt)} $amount"             //If the number is XX0,XXX,XXX, return "x million"
          else multiplesOfTen(tensMillions.toInt)                                         //Otherwise, return x

      val millionsWord =
        if (tensMillionsWord.contains("ten") || tensMillionsWord.contains("eleven") ||
            tensMillionsWord.contains("twelve") || tensMillionsWord.contains("teen")) ""  //If x is 10-19, the word is already made so return an empty string
        else
        if (millions == 0) "" else s"${oneToNine(millions)} $amount"                      //If x is 20/30/40 etc, the word is already made as well, otherwise return "1-9 million"

      val sentence = s"$hundredMillionsWord $tensMillionsWord $millionsWord"              //Construct the number in words by combining each string together
      sentence
    }

    def block2(group: Int, amount: String): String = {
      val thousands = digitGroup(group)(2).toString.toInt
      val tensThousands =
        if (digitGroup(group)(1).toString == "1") digitGroup(group)(1).toString + digitGroup(group)(2).toString
        else if (digitGroup(group)(1).toString == "0") "0"
        else digitGroup(group)(1).toString + "0"

      val hundredThousands = digitGroup(group).head.toString.toInt

      val hundredThousandsWord =
        if (hundredThousands > 0)
          if (tensThousands.head.asDigit == 0 && thousands == 0) oneToNine(hundredThousands) + s" hundred $amount"
          else oneToNine(hundredThousands) + s" hundred "
        else ""

      val tensThousandsWord =
        if (tensThousands.head == '0') ""
        else if (tensThousands.head == '1')
          if (hundredThousands == 0) s"${tenToNineteen(tensThousands.toInt)} $amount"
          else s"${tenToNineteen(tensThousands.toInt)} $amount"
        else
          if (thousands == 0) s"${multiplesOfTen(tensThousands.toInt)} $amount"
          else multiplesOfTen(tensThousands.toInt)

      val thousandsWord =
        if (tensThousandsWord.contains("ten") || tensThousandsWord.contains("eleven") ||
          tensThousandsWord.contains("twelve") || tensThousandsWord.contains("teen")) ""
        else
          if (thousands == 0) ""
          else s"${oneToNine(thousands)} $amount"
      val sentence = s"$hundredThousandsWord $tensThousandsWord $thousandsWord"
      sentence
    }

    def block3(group: Int, amount: String): String = {
      val ones = digitGroup(group)(2).toString.toInt
      val tens =
        if (digitGroup(group)(1).toString == "1") digitGroup(group)(1).toString + digitGroup(group)(2).toString
         else if (digitGroup(group)(1).toString == "0") "0"
         else digitGroup(group)(1).toString + "0"

      val hundred = digitGroup(group).head.toString.toInt
      val hundredWord = if (hundred > 0) oneToNine(hundred) + s" $amount " else ""
      val tensWord =
        if (tens.head == '0') ""
        else if (tens.head == '1') tenToNineteen(tens.toInt)
        else multiplesOfTen(tens.toInt)

      val onesWord =
        if (tensWord.contains("ten") || tensWord.contains("eleven") ||
        tensWord.contains("twelve") || tensWord.contains("teen")) ""
      else oneToNine(ones)

      val sentence = s"$hundredWord $tensWord $onesWord"
      sentence
    }

    val result = firstNonZeroIndex match {
      case zero if number == 0 => "zero"
      case millions if firstNonZeroIndex < 3 && firstNonZeroIndex >= 0 => s"${block1(0, "million")} ${block2(1, "thousand")} ${block3(2, "hundred")}"
      case thousands if firstNonZeroIndex < 6 && firstNonZeroIndex >= 3 => s"${block2(1, "thousand")} ${block3(2, "hundred")}"
      case hundreds if firstNonZeroIndex < 9 && firstNonZeroIndex >= 6 => block3(2, "hundred")
    }

    val noMultiSpaces = result.replaceAll("""([ ]{2,})""", " ")        //Removes any instances of 2 or more spaces and makes them a single space
    val noLeadingSpace = noMultiSpaces.replaceAll("""\A\s+""", "")     //Removes any leading whitespace at the beginning of the string
    val noTrailingSpace = noLeadingSpace.replaceAll("""\s+\Z""", "")   //Removes any trailing whitespace at the end of the string
    noTrailingSpace
  }
}
