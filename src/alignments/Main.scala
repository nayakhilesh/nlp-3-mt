package alignments;

import scala.compat.Platform

import com.typesafe.config.ConfigFactory

object Main {

  def main(args: Array[String]) {

    //usage: corpus.en, corpus.es

    val start = Platform.currentTime

    val mtConf = ConfigFactory.load()
    //println(mtConf.getString("machine-translator.model1.params-write-file"))

    val translator = new MachineTranslator
    translator.initialize(mtConf, args(0), args(1))

    // TODO translator.translate
    // TODO regression tests
    // TODO logging
    // TODO segregate into packages

    // TODO debug IbmModel2

    val end = Platform.currentTime

    println("Total time=" + (end - start) / 1000.0 + "s")

  }

}
