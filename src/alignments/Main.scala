package alignments;

import scala.io.Source

object Main {

  def main(args: Array[String]) {

    val n = getN()
    val t = initializeT(n)
    
    println(t.size)
    println("number of f|e combinations:" + t.foldLeft(0) ((acc, kv) => acc + kv._2.size))

  }

  def getN() = {

    val lang1Lines = Source.fromFile("corpus.en", "utf-8").getLines
    val lang2Lines = Source.fromFile("corpus.es", "utf-8").getLines

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    for ((line1, line2) <- lang1Lines zip lang2Lines) {
      if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
        line1 split " " foreach (word1 =>
          if (n.contains(word1))
            n(word1) ++= line2.split(" ")
          else
            n(word1) = collection.mutable.Set[String]())
      }
    }

    n("NULL") = collection.mutable.Set[String]()
    val lang2Lines2 = Source.fromFile("corpus.es", "utf-8").getLines
    lang2Lines2 foreach (line2 => if (!line2.trim.isEmpty) n("NULL") ++= line2.split(" "))

    n
  }

  def initializeT(n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {

    val translationParams = collection.mutable.Map[String, collection.mutable.Map[String, Double]]()

    val lang1Lines = Source.fromFile("corpus.en", "utf-8").getLines
    val lang2Lines = Source.fromFile("corpus.es", "utf-8").getLines

    val transParamEst = new TranslationParamEstimator

    for (((line1, line2), index) <- lang1Lines zip lang2Lines zipWithIndex) {
      println("line#:" + (index+1))
      if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
        line1 split " " foreach (word1 =>
          line2 split " " foreach (word2 =>
            if (translationParams.contains(word1))
              translationParams(word1) += (word2 -> transParamEst.estimate(word2, word1, n))
            else
              translationParams(word1) = collection.mutable.Map[String, Double]()))
      }
    }

    translationParams("NULL") = collection.mutable.Map[String, Double]()
    n("NULL") foreach (word2 => translationParams("NULL") += (word2 -> transParamEst.estimate(word2, "NULL", n)))

    translationParams
  }

}

class TranslationParamEstimator {

  val cache = collection.mutable.Map[String, Double]()

  def estimate(word2: String, word1: String, n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {
    cache.getOrElseUpdate(word1, (1.0 / n(word1).size))
  }
}