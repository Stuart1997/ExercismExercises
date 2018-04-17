package bracketpush

import scala.util.matching.Regex

object BracketPush extends App {

  def bracketsArePaired(string: String): Boolean = {
    val onlyBrackets = string.replaceAll("""([^{\[\(\)\]\}])""", "") //Removes everything that aren't brackets

    onlyBrackets match {
      case a
        if a.startsWith("}") || a.startsWith(")") || a.startsWith("]") => false   //Invalidates any string beginning with }, ) or ]
      case b
        if b.endsWith("{") || b.endsWith("(") || b.endsWith("[") => false         //Invalidates any string ending with {, ( or [
      case c
        if !c.contains("{") && !c.contains("(") && !c.contains("[") => false      //Invalidates any string that does not contain any of {, ( or [
      case _ => {
        if (onlyBrackets.length % 2 == 0) {                                       //Checks the string in question contains a multiple of 2
          checkValidity(onlyBrackets)                                             //Calls this method to see whether the pairs are valid
        } else false
      }
    }
  }

  def checkValidity(testString: String): Boolean = {
    def go(decrementer: String): String = {                       //This method's decrementer will get progressively smaller with each cycle until there is no more valid pairs left
      val bracketPattern = """(\[\]|\(\)|\{\})"""                 //[] or () or {}
      val regex: Regex = s"""(.*?)$bracketPattern(.*?)""".r       //Checks if one of the sets of brackets is found more than once
      val removed: String = decrementer match {
        case regex(_, _, _) =>                                    //Checks if the regex pattern contains 3 groups: (.*?) - the bracket matches - (.*?)
          go(decrementer.replaceAll(bracketPattern, ""))          //Removes the valid bracket pair in this loop, calls go again so that it can return the new string to recycle the loop
        case x => x                                               //Returns either the empty string or the string at the point at which it is fully validated
      }
      removed                                                     //Returns the outcome of case x => x
    }
    go(testString).isEmpty                                        //Begins the loop by providing the decrementer, also returns whether the validity is true or false
  }
}