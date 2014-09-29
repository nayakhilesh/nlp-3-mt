package main.scala

import scala.Array.canBuildFrom

import main.scala.Utils.{MutableTranslationParameters, NULL, loopThroughFiles}

object DefaultTranslationParams {

  def getDefaultTranslationParams(lang1FilePath: String,
                                  lang2FilePath: String): MutableTranslationParameters = {

    val n = getN(lang1FilePath, lang2FilePath)
    getUniformlyDistributedTranslationParams(lang1FilePath, lang2FilePath, n)

  }

  private[this] def getN(lang1FilePath: String, lang2FilePath: String) = {

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    println("Initializing 'n':")

    loopThroughFiles(lang1FilePath, lang2FilePath) {
      (line1, line2, index) =>
        NULL +: (line1 split " ") foreach (word1 =>
          if (n.contains(word1))
            n(word1) ++= line2.split(" ")
          else
            n(word1) = collection.mutable.Set[String]() ++= line2.split(" "))
    }

    println("Done Initializing 'n'")

    n
  }

  private[this] def getUniformlyDistributedTranslationParams(lang1FilePath: String, lang2FilePath: String,
                                                             n: collection.mutable.Map[String, collection.mutable.Set[String]]): MutableTranslationParameters = {

    val translationParams = new collection.mutable.HashMap[String, collection.mutable.Map[String, Double]]
    val transParamEst = new TranslationParamEstimator

    println("Initializing translationParams:")

    loopThroughFiles(lang1FilePath, lang2FilePath) {
      (line1, line2, index) =>
        NULL +: (line1 split " ") foreach (word1 =>
          line2 split " " foreach (word2 =>
            if (translationParams.contains(word1))
              translationParams(word1) += (word2 -> transParamEst.estimate(word2, word1, n))
            else
              translationParams(word1) = collection.mutable.Map(word2 -> transParamEst.estimate(word2, word1, n))))
    }

    println("Done Initializing translationParams")

    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) {
        case (acc, (_, map)) => acc + map.size
      })

    translationParams
  }

  class TranslationParamEstimator {

    private[this] val cache = collection.mutable.Map[String, Double]()

    def estimate(word2: String, word1: String,
                 n: collection.mutable.Map[String, collection.mutable.Set[String]]): Double = {
      cache.getOrElseUpdate(word1, (1.0 / n(word1).size))
    }
  }

}