package alignments;

import scala.compat.Platform

object Main {

  def main(args: Array[String]) {

    //usage: corpus.en, corpus.es

    val start = Platform.currentTime

    val translator = new MachineTranslator
    translator.initialize(args(0), args(1))

    // TODO translator.translate 
    // TODO regression tests
    // TODO logging

    val end = Platform.currentTime

    println("Total time=" + (end - start) / 1000.0 + "s")

  }

}
