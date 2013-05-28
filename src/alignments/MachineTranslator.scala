package alignments

import scala.Array.canBuildFrom

import com.typesafe.config.Config

class MachineTranslator {

  def initialize(conf: Config, lang1FilePath: String, lang2FilePath: String) {

    val n = getN(lang1FilePath, lang2FilePath)
    val translationParams = getInitialTranslationParams(lang1FilePath, lang2FilePath, n)

    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val ibm1 = new IbmModel1
    val model1ParamsReadFile = conf.getString("machine-translator.model1.params-read-file")
    val model1ParamsWriteFile = conf.getString("machine-translator.model1.params-write-file")
    if (model1ParamsReadFile.trim.isEmpty)
      ibm1.computeParams(translationParams, lang1FilePath, lang2FilePath, 5)
    else
      ibm1.readParams(model1ParamsReadFile)

    if (!model1ParamsWriteFile.trim.isEmpty)
      ibm1.writeParams(model1ParamsWriteFile)

    ibm1.writeAlignments("dev.en", "dev.es", "dev.model1.out")

    val alignmentParams = getInitialAlignmentParams(lang1FilePath, lang2FilePath)

    println("number of 4-tuples in alignmentParams:" + alignmentParams.size)

    val ibm2 = new IbmModel2
    val model2ParamsReadFile = conf.getString("machine-translator.model2.params-read-file")
    val model2ParamsWriteFile = conf.getString("machine-translator.model2.params-write-file")
    if (model2ParamsReadFile.trim.isEmpty)
      ibm2.computeParams(translationParams, alignmentParams, lang1FilePath, lang2FilePath, 5)
    else
      ibm2.readParams(model2ParamsReadFile)

    if (!model2ParamsWriteFile.trim.isEmpty)
      ibm2.writeParams(model2ParamsWriteFile)

    ibm2.writeAlignments("dev.en", "dev.es", "dev.model2.out")

  }

  def getN(lang1FilePath: String, lang2FilePath: String) = {

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    println("Initializing 'n':")

    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      NULL +: (line1 split " ") foreach (word1 =>
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

      NULL +: (line1 split " ") foreach (word1 =>
        line2 split " " foreach (word2 =>
          if (translationParams.contains(word1))
            translationParams(word1) += (word2 -> transParamEst.estimate(word2, word1, n))
          else
            translationParams(word1) = collection.mutable.Map(word2 -> transParamEst.estimate(word2, word1, n))))

    })

    println("Done Initializing translationParams")

    translationParams
  }

  def getInitialAlignmentParams(lang1FilePath: String, lang2FilePath: String) = {

    val alignmentParams = collection.mutable.Map[(Int, Int, Int, Int), Double]()
    val sentenceLengthPairs = collection.mutable.Set[(Int, Int)]()

    println("Initializing alignmentParams:")

    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      sentenceLengthPairs += ((line1 split " ").size -> (line2 split " ").size)

    })

    sentenceLengthPairs foreach {
      case (l, m) =>
        println(".")
        0 to l foreach (j =>
          1 to m foreach (i =>
            alignmentParams((j, i, l, m)) = 1.0 / (l + 1)))
    }

    println("Done Initializing alignmentParams")

    alignmentParams
  }

}

class TranslationParamEstimator {

  val cache = collection.mutable.Map[String, Double]()

  def estimate(word2: String, word1: String, n: collection.mutable.Map[String, collection.mutable.Set[String]]) = {
    cache.getOrElseUpdate(word1, (1.0 / n(word1).size))
  }
}