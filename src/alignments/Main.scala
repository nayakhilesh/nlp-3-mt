package alignments;

import scala.compat.Platform

object Main {

  def main(args: Array[String]) {

    val start = Platform.currentTime

    val translator = new MachineTranslator
    translator.initialize(args(0), args(1))

    val end = Platform.currentTime

    println("Total time=" + (end - start) / 1000.0 + "s")

  }

}
