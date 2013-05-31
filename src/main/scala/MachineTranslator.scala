package main.scala

import com.typesafe.config.Config

class MachineTranslator {

  def initialize(conf: Config, lang1FilePath: String, lang2FilePath: String) {

    val ibm1 = new IbmModel1

    val model1ParamsReadFile = conf.getString("machine-translator.model1.params-read-file")
    val model1ParamsWriteFile = conf.getString("machine-translator.model1.params-write-file")

    if (model1ParamsReadFile.trim.isEmpty)
      ibm1.computeParams(lang1FilePath, lang2FilePath, 5)
    else
      ibm1.readParams(model1ParamsReadFile)

    if (!model1ParamsWriteFile.trim.isEmpty)
      ibm1.writeParams(model1ParamsWriteFile)

    ibm1.writeAlignments("dev.en", "dev.es", "dev.model1.out")

    val ibm2 = new IbmModel2

    val model2ParamsReadFile = conf.getString("machine-translator.model2.params-read-file")
    val model2ParamsWriteFile = conf.getString("machine-translator.model2.params-write-file")

    if (model2ParamsReadFile.trim.isEmpty)
      ibm2.computeParams(lang1FilePath, lang2FilePath, 5, ibm1.translationParams)
    else
      ibm2.readParams(model2ParamsReadFile)

    if (!model2ParamsWriteFile.trim.isEmpty)
      ibm2.writeParams(model2ParamsWriteFile)

    ibm2.writeAlignments("dev.en", "dev.es", "dev.model2.out")

  }

}

