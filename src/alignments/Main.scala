package alignments;

import scala.Array.canBuildFrom
import scala.compat.Platform
import scala.io.Source

object Main {

  val NULL = "NULL"

  def main(args: Array[String]) {

    val start = Platform.currentTime

    val n = getN()
    val t = initializeT(n)

    println("number of e words in t:" + t.size)
    println("number of f|e combinations in t:" + t.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val startEm = Platform.currentTime
    
    // TODO refactor code using method to concurrently loop through files
    // TODO move EM algorithm into separate method

    val temp = collection.mutable.Set[(String, String)]()
    t foreach {
      case (word1, map) =>
        map foreach {
          case (word2, _) =>
            temp += (word1 -> word2)
        }
    }

    println("also number of f|e combinations in t:" + temp.size)
    //System.exit(0)

    val S = 5
    1 to S foreach { iter =>
      println("Starting iteration #" + iter)

      val lang1Lines = Source.fromFile("corpus.en", "utf-8").getLines
      val lang2Lines = Source.fromFile("corpus.es", "utf-8").getLines

      val c1 = collection.mutable.Map[String, Double]()
      val c2 = collection.mutable.Map[(String, String), Double]()

      for (((line1, line2), index) <- lang1Lines zip lang2Lines zipWithIndex) {
        println("line#:" + (index + 1))
        if (!line1.trim.isEmpty && !line2.trim.isEmpty) {

          line2 split " " foreach { word2 =>

            val denom = ((line1 split " ") :+ NULL).foldLeft(0.0)((acc, word1) => acc + t(word1)(word2))
            (line1 split " ") :+ NULL foreach { word1 =>
              val delta = t(word1)(word2) / denom
              c2((word1, word2)) = c2.getOrElse((word1, word2), 0.0) + delta
              c1(word1) = c1.getOrElse(word1, 0.0) + delta
            }

          }

        }
      }

      temp foreach {
        case (word1, word2) =>
          t(word1)(word2) = c2((word1, word2)) / c1(word1)
      }

      println("Finished iteration #" + iter)
    }

    println("number of e words in t:" + t.size)
    println("number of f|e combinations in t:" + t.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val end = Platform.currentTime

    println("Total time=" + (end - start) / 1000.0 + "s")
    println("EM time=" + (end - startEm) / 1000.0 + "s")

  }

  def getN() = {

    val lang1Lines = Source.fromFile("corpus.en", "utf-8").getLines
    val lang2Lines = Source.fromFile("corpus.es", "utf-8").getLines

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    for ((line1, line2) <- lang1Lines zip lang2Lines) {
      if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
        (line1 split " ") :+ NULL foreach (word1 =>
          if (n.contains(word1))
            n(word1) ++= line2.split(" ")
          else
            n(word1) = collection.mutable.Set[String]() ++= line2.split(" "))
      }
    }

    n
  }

  def initializeT(n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {

    val translationParams = collection.mutable.Map[String, collection.mutable.Map[String, Double]]()

    val lang1Lines = Source.fromFile("corpus.en", "utf-8").getLines
    val lang2Lines = Source.fromFile("corpus.es", "utf-8").getLines

    val transParamEst = new TranslationParamEstimator

    for (((line1, line2), index) <- lang1Lines zip lang2Lines zipWithIndex) {
      println("line#:" + (index + 1))
      if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
        (line1 split " ") :+ NULL foreach (word1 =>
          line2 split " " foreach (word2 =>
            if (translationParams.contains(word1))
              translationParams(word1) += (word2 -> transParamEst.estimate(word2, word1, n))
            else
              translationParams(word1) = collection.mutable.Map[String, Double](word2 -> transParamEst.estimate(word2, word1, n))))
      }
    }

    translationParams
  }

}

class TranslationParamEstimator {

  val cache = collection.mutable.Map[String, Double]()

  def estimate(word2: String, word1: String, n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {
    cache.getOrElseUpdate(word1, (1.0 / n(word1).size))
  }
}