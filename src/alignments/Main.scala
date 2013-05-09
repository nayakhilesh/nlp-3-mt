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

    // TODO make classes: MachineTranslator, IbmModel1, IbmModel2

    // TODO move EM algorithm into separate method

    val temp = t.map {
      case (word1, map) =>
        map.foldLeft(List[(String, String)]()) { case (list, (word2, _)) => (word1, word2) +: list }
    } flatten

    println("also number of f|e combinations in t:" + temp.size)

    val S = 5
    1 to S foreach { iter =>
      println("Starting iteration #" + iter)

      val c1 = collection.mutable.Map[String, Double]()
      val c2 = collection.mutable.Map[(String, String), Double]()

      loopThroughFiles("corpus.en", "corpus.es")((line1: String, line2: String, index: Int) => {

        line2 split " " foreach { word2 =>

          val denom = ((line1 split " ") :+ NULL).foldLeft(0.0)((acc, word1) => acc + t(word1)(word2))
          (line1 split " ") :+ NULL foreach { word1 =>
            val delta = t(word1)(word2) / denom
            c2((word1, word2)) = c2.getOrElse((word1, word2), 0.0) + delta
            c1(word1) = c1.getOrElse(word1, 0.0) + delta
          }

        }

      })

      temp foreach {
        case (word1, word2) =>
          t(word1)(word2) = c2((word1, word2)) / c1(word1)
      }

      println("Finished iteration #" + iter)
    }

    println("number of e words in t:" + t.size)
    println("number of f|e combinations in t:" + t.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val endEm = Platform.currentTime

    // Writing dev alignments out
    val outputFile = new java.io.FileWriter("dev.out")

    loopThroughFiles("dev.en", "dev.es")((line1: String, line2: String, index: Int) => {

      for ((word2, index2) <- line2 split " " zipWithIndex) {

        val (_, maxIndex) = ((NULL +: (line1 split " ")) zipWithIndex).maxBy { case (word1, index1) => t(word1)(word2) }
        outputFile.write((index + 1) + " " + maxIndex + " " + index2 + "\n")

      }

    })

    outputFile.close()

    val end = Platform.currentTime

    println("EM time=" + (endEm - startEm) / 1000.0 + "s")
    println("Total time=" + (end - start) / 1000.0 + "s")

  }

  def getN() = {

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    loopThroughFiles("corpus.en", "corpus.es")((line1: String, line2: String, index: Int) => {

      (line1 split " ") :+ NULL foreach (word1 =>
        if (n.contains(word1))
          n(word1) ++= line2.split(" ")
        else
          n(word1) = collection.mutable.Set[String]() ++= line2.split(" "))

    })

    n
  }

  def initializeT(n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {

    val translationParams = collection.mutable.Map[String, collection.mutable.Map[String, Double]]()
    val transParamEst = new TranslationParamEstimator

    loopThroughFiles("corpus.en", "corpus.es")((line1: String, line2: String, index: Int) => {

      (line1 split " ") :+ NULL foreach (word1 =>
        line2 split " " foreach (word2 =>
          if (translationParams.contains(word1))
            translationParams(word1) += (word2 -> transParamEst.estimate(word2, word1, n))
          else
            translationParams(word1) = collection.mutable.Map[String, Double](word2 -> transParamEst.estimate(word2, word1, n))))

    })

    translationParams
  }

  def loopThroughFiles(file1: String, file2: String)(funcToPerform: (String, String, Int) => _) {

    val file1Lines = Source.fromFile(file1, "utf-8").getLines
    val file2Lines = Source.fromFile(file2, "utf-8").getLines

    for (((line1, line2), index) <- file1Lines zip file2Lines zipWithIndex) {
      if ((index + 1) % 200 == 0) println("line#:" + (index + 1))
      if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
        funcToPerform(line1, line2, index)
      }
    }

  }

}

class TranslationParamEstimator {

  val cache = collection.mutable.Map[String, Double]()

  def estimate(word2: String, word1: String, n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {
    cache.getOrElseUpdate(word1, (1.0 / n(word1).size))
  }
}