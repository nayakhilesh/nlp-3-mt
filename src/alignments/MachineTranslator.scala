package alignments

import scala.Array.canBuildFrom

class MachineTranslator {

  def initialize(lang1FilePath: String, lang2FilePath: String) {

    val n = getN(lang1FilePath, lang2FilePath)
    val translationParams = getInitialTranslationParams(lang1FilePath, lang2FilePath, n)

    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val ibm1 = new IbmModel1(translationParams)
    ibm1.initialize(lang1FilePath, lang2FilePath, 5)
    ibm1.writeAlignments("test.en", "test.es", "test.out")

  }

  def getN(lang1FilePath: String, lang2FilePath: String) = {

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    println("Initializing 'n':")

    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      (line1 split " ") :+ NULL foreach (word1 =>
        if (n.contains(word1))
          n(word1) ++= line2.split(" ")
        else
          n(word1) = collection.mutable.Set[String]() ++= line2.split(" "))

    })

    println("Done Initializing 'n'")

    n
  }

  def getInitialTranslationParams(lang1FilePath: String, lang2FilePath: String,
    n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {

    val translationParams = collection.mutable.Map[String, collection.mutable.Map[String, Double]]()
    val transParamEst = new TranslationParamEstimator

    println("Initializing translationParams:")

    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      (line1 split " ") :+ NULL foreach (word1 =>
        line2 split " " foreach (word2 =>
          if (translationParams.contains(word1))
            translationParams(word1) += (word2 -> transParamEst.estimate(word2, word1, n))
          else
            translationParams(word1) = collection.mutable.Map(word2 -> transParamEst.estimate(word2, word1, n))))

    })

    println("Done Initializing translationParams")

    translationParams
  }

}

class TranslationParamEstimator {

  val cache = collection.mutable.Map[String, Double]()

  def estimate(word2: String, word1: String, n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {
    cache.getOrElseUpdate(word1, (1.0 / n(word1).size))
  }
}