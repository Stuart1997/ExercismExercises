package wordcount

import scala.collection.immutable.ListMap

object WordCount extends App {
  def CountWords(sentence: String): Option[Map[String, Int]] = {
    val lowerCaseSentence = sentence.toLowerCase()                              //Removes case issues (e.g. Hello and hello will no longer be seen as different words)
    val noMultiSpaces = lowerCaseSentence.replaceAll("""([ ]{2,})""", " ")      //Removes any instances of 2 or more spaces and makes them a single space
    val noSpecialCharacters = noMultiSpaces.replaceAll("""([^a-z\s])""", "")    //Removes everything that isn't the letters a-z or whitespace

    if (noSpecialCharacters != "" && noSpecialCharacters != " ") {
      val sentenceAsList = noSpecialCharacters.split(" ").toList                //Adds each individual word to a list
      val mappedWords = sentenceAsList.groupBy(identity).mapValues(_.size)      //Maps each word with how many times it is found in the list
      Some(ListMap(mappedWords.toSeq.sortWith(_._2 > _._2):_*))                 //Orders the words by how many times they appear (highest to lowest)
    } else {
      None
    }
  }
}
