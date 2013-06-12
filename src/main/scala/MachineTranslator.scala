package main.scala

import com.typesafe.config.Config
import Utils.loopThroughFiles

class MachineTranslator {

  def initialize(conf: Config, lang1FilePath: String, lang2FilePath: String) {

    val lang1ParamsReadFile = conf.getString("machine-translator.lang1.params-read-file")
    val lang1ParamsWriteFile = conf.getString("machine-translator.lang1.params-write-file")

    //p(f|e); lang1 = e
    val ibm2Lang1 = new IbmModel2
    initializeIbmModel2(lang1FilePath, lang2FilePath, lang1ParamsReadFile, lang1ParamsWriteFile, ibm2Lang1)

    val lang2ParamsReadFile = conf.getString("machine-translator.lang2.params-read-file")
    val lang2ParamsWriteFile = conf.getString("machine-translator.lang2.params-write-file")

    //p(e|f); lang2 = f
    val ibm2Lang2 = new IbmModel2
    initializeIbmModel2(lang2FilePath, lang1FilePath, lang2ParamsReadFile, lang2ParamsWriteFile, ibm2Lang2)

    // TODO buildLexicon
    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      //each position has the index of the lang1 word it aligns to (starting from 0 = NULL)
      val lang2Alignments = ibm2Lang1.extractAlignments(line1, line2)

      val lang1Alignments = ibm2Lang2.extractAlignments(line2, line1)

      val lang2FinalAlignments = new collection.mutable.ArrayBuffer[Seq[Int]](lang2Alignments.size)
      val lang1FinalAlignments = new collection.mutable.ArrayBuffer[Seq[Int]](lang1Alignments.size)

      (lang2Alignments zipWithIndex) foreach {
        case (lang1Index, lang2Index) =>
          if (lang1Index > 0 && lang1Alignments(lang1Index - 1) == lang2Index + 1)
            lang2FinalAlignments += IndexedSeq[Int](lang1Index - 1)
          else lang2FinalAlignments += null
      }

      (lang1Alignments zipWithIndex) foreach {
        case (lang2Index, lang1Index) =>
          if (lang2Index > 0 && lang2Alignments(lang2Index - 1) == lang1Index + 1)
            lang1FinalAlignments += IndexedSeq[Int](lang2Index - 1)
          else lang1FinalAlignments += null
      }

      println(lang2FinalAlignments)
      println(lang1FinalAlignments)

    })

  }

  private[this] def initializeIbmModel2(lang1FilePath: String, lang2FilePath: String,
    paramsReadFilePath: String, paramsWriteFilePath: String, ibm2: IbmModel2) {

    if (paramsReadFilePath.trim.isEmpty) {
      val ibm1 = new IbmModel1
      ibm1.computeParams(lang1FilePath, lang2FilePath, 5)
      ibm2.computeParams(lang1FilePath, lang2FilePath, 5, ibm1.translationParams)
    } else {
      ibm2.readParams(paramsReadFilePath)
    }

    if (!paramsWriteFilePath.trim.isEmpty)
      ibm2.writeParams(paramsWriteFilePath)

  }

}

