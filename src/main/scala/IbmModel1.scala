package main.scala

import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter

import scala.Array.canBuildFrom
import scala.compat.Platform
import scala.io.Source

import Utils.NULL
import Utils.TranslationParameters
import Utils.loopThroughFiles

class IbmModel1 extends IbmModelLike with DefaultTranslationParams {

  var translationParams: TranslationParameters = _

  def computeParams(lang1FilePath: String, lang2FilePath: String, numIterations: Int,
    initialTranslationParams: TranslationParameters = null) {

    if (initialTranslationParams == null)
      translationParams = getDefaultTranslationParams(lang1FilePath, lang2FilePath)
    else
      translationParams = initialTranslationParams

    val startEm = Platform.currentTime

    val temp = translationParams.toSeq.flatMap {
      case (word1, map) =>
        map.foldLeft(List[(String, String)]()) { case (list, (word2, _)) => (word1, word2) +: list }
    }

    println("also number of lang2|lang1 combinations in translationParams:" + temp.size)

    1 to numIterations foreach { iter =>
      println("Starting iteration #" + iter)

      val c1 = collection.mutable.Map[String, Double]()
      val c2 = collection.mutable.Map[(String, String), Double]()

      loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

        line2 split " " foreach { word2 =>

          val denom = (NULL +: (line1 split " ")).foldLeft(0.0)((acc, word1) => acc + translationParams(word1)(word2))
          NULL +: (line1 split " ") foreach { word1 =>
            val delta = translationParams(word1)(word2) / denom
            c2((word1, word2)) = c2.getOrElse((word1, word2), 0.0) + delta
            c1(word1) = c1.getOrElse(word1, 0.0) + delta
          }

        }

      })

      temp foreach {
        case (word1, word2) =>
          translationParams(word1)(word2) = c2((word1, word2)) / c1(word1)
      }

      println("Finished iteration #" + iter)
    }

    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val endEm = Platform.currentTime

    println("EM time=" + (endEm - startEm) / 1000.0 + "s")

  }

  override def extractAlignments(line1: String, line2: String) = {

    val list = collection.mutable.ListBuffer[Int]()
    line2 split " " foreach { word2 =>

      val (_, maxIndex) = ((NULL +: (line1 split " ")) zipWithIndex).maxBy {
        case (word1, index1) => translationParams(word1)(word2)
      }
      list += maxIndex

    }

    list.toList
  }

  override def writeParams(outputFilePath: String) {

    println("Writing params to file")

    val out = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(new File(outputFilePath)), "UTF8"));

    translationParams.foreach {
      case (word1, map) =>
        map.foreach { case (word2, prob) => out.write(word1 + " " + word2 + " " + prob + "\n") }
    }

    out.flush();
    out.close();

    println("Done Writing params to file")

  }

  override def readParams(filePath: String) {

    translationParams = new TranslationParameters

    println("Reading params from file:")

    val fileLines = Source.fromFile(filePath, "utf-8").getLines

    (fileLines zipWithIndex) foreach {
      case (line, index) =>
        if ((index + 1) % 20000 == 0) println("line#:" + (index + 1))
        if (!line.trim.isEmpty) {
          val tokens = line split " "
          val word1 = tokens(0) intern
          val word2 = tokens(1) intern
          val prob = tokens(2).toDouble
          if (translationParams.contains(word1))
            translationParams(word1) += (word2 -> prob)
          else
            translationParams(word1) = collection.mutable.Map(word2 -> prob)
        }
    }

    println("Done Reading params from file")
    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

  }

}