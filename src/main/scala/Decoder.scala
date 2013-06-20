package main.scala

import scala.collection.mutable.PriorityQueue

class Decoder(val lexicon: Lexicon,
  val languageModel: TrigramLanguageModel,
  val distortionLimit: Int, val distortionParam: Double) {

  // TODO
  def decode(line: String): String = {
    
    val words = line split " "
    val beams = new Array[PriorityQueue[Int]](words.size)
    
    ""
  }
  
}