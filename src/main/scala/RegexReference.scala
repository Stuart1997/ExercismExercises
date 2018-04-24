object RegexReference extends App {

  val test = "ABC123()!!"

  val replace = test.replaceAll("""([ABC])""", "")  //Replace the ABC with nothing (i.e. remove it)
  val matchCases = """([\A\b])"""                   //The exact character matches are separated with the \ (A, b)

  val anyNumber =   """([!]{*})"""                  //Any number of !
  val onceOrNone =  """([!]{?})"""                  //Exactly none or 1 !
  val exactlyTwo =  """([!]{2})"""                  //Exactly 2 !
  val fiveToTen =   """([!]{5,10})"""               //Between 5 and 10 ! (including 5 & 10)
  val noMultiples = """([!]{10,})"""                //10 or more !

  val allButSpecified = """([^a-z\s])"""            //Only lower case a-z and whitespace, no other characters
  val orOperator = """(\[\]|\(\)|\{\})"""           //[] or () or {}
}
