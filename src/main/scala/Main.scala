package main.scala

import scala.compat.Platform

import com.typesafe.config.ConfigFactory

object Main {

  def main(args: Array[String]) {

    //usage: corpus.en, corpus.es

    val start = Platform.currentTime

    val mtConf = ConfigFactory.load("mt.conf")

    val translator = new MachineTranslator(mtConf, args(0), args(1))

    val end = Platform.currentTime

    println("Total time=" + (end - start) / 1000.0 + "s")

    println("Enter sentence for translation:")
    for (lang1Sentence <- io.Source.stdin.getLines)
      println(translator translate lang1Sentence)

    // TODO regression tests
    // TODO add to mt.conf:
    // IbmModel(1/2) - numIterations?
    // Lexicon - reading/writing file
    // TrigramLanguageModel - reading/writing file?
    // TODO fix slowness -> see if you can parallelize file access / algorithms
    // TODO fix memory use?

  }

}
