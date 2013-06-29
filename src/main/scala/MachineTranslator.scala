package main.scala

import com.typesafe.config.Config

import Utils.loopThroughFiles

class MachineTranslator {

  def initialize(conf: Config, lang1FilePath: String, lang2FilePath: String) {

    val lang1ParamsReadFile = conf.getString("machine-translator.lang1.params-read-file")
    val lang1ParamsWriteFile = conf.getString("machine-translator.lang1.params-write-file")

    //p(f|e)
    //computes prob that word f in lang2 aligns to word e in lang1
    val ibm2Lang1 = new IbmModel2
    initializeIbmModel2(lang1FilePath, lang2FilePath, lang1ParamsReadFile, lang1ParamsWriteFile, ibm2Lang1)

    val lang2ParamsReadFile = conf.getString("machine-translator.lang2.params-read-file")
    val lang2ParamsWriteFile = conf.getString("machine-translator.lang2.params-write-file")

    //p(e|f)
    //computes prob that word e in lang1 aligns to word f in lang2
    val ibm2Lang2 = new IbmModel2
    initializeIbmModel2(lang2FilePath, lang1FilePath, lang2ParamsReadFile, lang2ParamsWriteFile, ibm2Lang2)

    val lexicon = new Lexicon
    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      //each position has the index of the lang1 word it aligns to (starting from 0 <=> NULL)
      val lang2Alignments = ibm2Lang1.extractAlignments(line1, line2)

      //each position has the index of the lang2 word it aligns to (starting from 0 <=> NULL)
      val lang1Alignments = ibm2Lang2.extractAlignments(line2, line1)

      lexicon.add(lang1Alignments, lang2Alignments, line1, line2)

    })

    val lang2Model = new TrigramLanguageModel
    lang2Model initialize lang2FilePath

    val decoder = new Decoder(lexicon, lang2Model, 4, -2.0, 5.0)

    for (lang2Sentence <- io.Source.stdin.getLines)
      println(decoder decode lang2Sentence)

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

