package isogram

import scala.collection.immutable.ListMap

object Isogram extends App {
  def isAnIsogram(string: String): Boolean = {
    val lowerCaseString = string.toLowerCase()                                    //Removes case issues (e.g. H and h will no longer be seen as different characters)
    val noSpaces = lowerCaseString.replaceAll("""([ ]{1,})""", "")                //Prevents spaces from being mapped as they don't count towards an isogram
    val noHyphensOrSpaces = noSpaces.replaceAll("""([-]{1,})""", "")              //Prevents hyphens from being mapped as they don't count towards an isogram
    val organisedString = noHyphensOrSpaces.replaceAll("""([^a-z])""", "")        //Removes everything that isn't the letters a-z

    if (organisedString != "") {
      val eachLetter = organisedString.toList                                     //Creates a list of only letters
      val mappedLetters = eachLetter.groupBy(identity).mapValues(_.size)          //Maps each letter with how many times it is found in the list
      val orderedLetters = ListMap(mappedLetters.toSeq.sortWith(_._2 > _._2):_*)  //Orders the letters by how many times they appear (highest to lowest)

      if (orderedLetters.head._2 == 1) true else false                            //When ordered, if the highest number is 1 then every letter appears once, therefore it is an isogram
    } else false
  }
}
