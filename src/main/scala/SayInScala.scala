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

    val digitWords = List("million", "thousand", "")
    val smallIndex = 2
    val mediumIndex = 1

    val zeroes = StringBuilder.newBuilder                                 //Makes a mutable list
    val intAsStringList = number.toString.toList                          //Puts the number passed in into a list of strings (chars)
    val toNine = 9 - intAsStringList.size                                 //Works out how many 0s need to be put in front of the number
    for (x <- 1 to toNine) zeroes.append("0")                             //Add that many 0s to a list
    val nineDigits = zeroes.toList ::: intAsStringList                    //Combine those lists so that it becomes e.g. 0000xxxxx
    val firstNonZeroIndex = nineDigits.indexWhere(index => index != '0')  //Finds the first position in which the number actually begins
    val digitGroup = nineDigits.grouped(3).toList                         //Splits the number into 3 groups of 3 digits

    def allBlocks(blocks: List[List[Char]], FNZI: Int): String = {
      if (number < 0 || number > 999999999) "Invalid number"
      else if (number == 0) "zero"
      else {
        val wordConstruction = for (currentGroup <- 0 to 2 by 1) yield {            //Loops through each group of 3
          val smallNumber = blocks(currentGroup)(smallIndex).asDigit                //Within each group of 'XYZ', smallNumber is Z
          val mediumNumber =                                                        //Within each group of 'XYZ', mediumNumber is Y
            if (blocks(currentGroup)(mediumIndex).toString == "1")                  //mediumNumber is 1, therefore this block ends with 10-19, so concatenate smallNumber to it to make a tenToNineteen
              blocks(currentGroup)(mediumIndex).toString +
                blocks(currentGroup)(smallIndex).toString
            else if (blocks(currentGroup)(mediumIndex).toString == "0") "0"         //mediumNumber is 0, therefore leave it as it is
            else blocks(currentGroup)(mediumIndex).toString + "0"                   //mediumNumber is 2-9, therefore it will require a word from multiplesOfTen
          val largeNumber = blocks(currentGroup).head.asDigit                       //Within each group of 'XYZ', largeNumber is X

          val largeWord =
            if (largeNumber > 0)
              if (mediumNumber.head.asDigit == 0 && smallNumber == 0)
                oneToNine(largeNumber) + s" hundred ${digitWords(currentGroup)}"    //This block will just be a multiple of 100, e.g. one hundred million
              else oneToNine(largeNumber) + s" hundred"                             //This block will be more than 100 but will require more words, e.g. one hundred fifty million
            else ""                                                                 //Skips largeNumber when it's 0

          val mediumWord =
            if (mediumNumber.head == '0') ""                                        //Skips mediumNumber when it's 0
            else if (mediumNumber.head == '1')
              s"${tenToNineteen(mediumNumber.toInt)} ${digitWords(currentGroup)}"   //This block only contains a number from tenToNineteen, e.g. fifteen million
            else if (smallNumber == 0)
              s"${multiplesOfTen(mediumNumber.toInt)} ${digitWords(currentGroup)}"  //This block only contains a number from multiplesOfTen, e.g. thirty million
            else multiplesOfTen(mediumNumber.toInt)                                 //Construct just the word before adding the last digit of the block, e.g. thirty

          val smallWord =
            if (mediumWord.contains("ten") || mediumWord.contains("eleven") ||
              mediumWord.contains("twelve") || mediumWord.contains("teen")) ""      //10-19 numbers are already handled in mediumWord, therefore ignore these
            else if (smallNumber == 0) ""                                           //Prevents the digitWords suffix being added to a block that is 000, e.g. 123456 would begin with "million" otherwise
            else if (currentGroup == 2) oneToNine(smallNumber)                      //If it's in the last group of 3, don't add the digitWord, or else passing 2 into the method would return "two hundred"
            else s"${oneToNine(smallNumber)} ${digitWords(currentGroup)}"           //End the block with the last number and its digitWord, e.g. five million

          s"$largeWord $mediumWord $smallWord".trim                                 //Construct a string using each of the words and trim the string
        }
        wordConstruction.mkString(" ")                                              //Take each element of wordConstruction and form it as a string
      }
    }

    val result = allBlocks(digitGroup, firstNonZeroIndex)
    val noMultiSpaces = result.replaceAll("""([ ]{2,})""", " ")                     //Removes any instances of 2 or more spaces and makes them a single space
    val noLeadingSpace = noMultiSpaces.replaceAll("""\A\s+""", "")                  //Removes any leading whitespace at the beginning of the string
    val noTrailingSpace = noLeadingSpace.replaceAll("""\s+\Z""", "")                //Removes any trailing whitespace at the end of the string
    noTrailingSpace
  }
}