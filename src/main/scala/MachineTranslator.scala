package main.scala

import com.typesafe.config.Config

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
