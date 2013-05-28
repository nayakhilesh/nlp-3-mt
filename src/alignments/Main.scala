package alignments;

import scala.compat.Platform

import com.typesafe.config.ConfigFactory

object Main {

  def main(args: Array[String]) {

    //usage: corpus.en, corpus.es

    val start = Platform.currentTime

    val conf = ConfigFactory.load()

    val translator = new MachineTranslator
    translator.initialize(conf, args(0), args(1))

    // TODO translator.translate 
    // TODO regression tests
    // TODO logging
    // TODO writing/reading params to/from file
    // TODO driven by config/properties file
    // TODO segregate into packages
    // TODO add defaulting of translationParams and alignmentParams into IbmModels

    // TODO debug IbmModel2

    val end = Platform.currentTime

    println("Total time=" + (end - start) / 1000.0 + "s")

  }

}
