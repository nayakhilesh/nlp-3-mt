package main.scala

import scala.collection.mutable.PriorityQueue
import java.util.BitSet

class Decoder(val lexicon: Lexicon,
  val languageModel: TrigramLanguageModel,
  val distortionLimit: Int, val distortionPenalty: Double) {

  // TODO
  def decode(line: String): String = {

    val words = line split " "
    val beams = new Array[Beam](words.size)

    ""
  }

  private[this] def ph(q: State, wordsLang1: Array[String]): collection.mutable.Set[Phrase] = {

    val phrases = collection.mutable.Set[Phrase]()
    //(r + 1) - d <= s <= (r + 1) + d
    (q.prevEnd + 1 - distortionLimit) to (q.prevEnd + 1 + distortionLimit) dropWhile (_ < 0) takeWhile (_ < wordsLang1.size) foreach {
      case start =>
        var end = start
        var overlap = false
        while (end < wordsLang1.size && !overlap) {
          val nextSet = q.bitString.nextSetBit(start)
          if (nextSet == -1 || nextSet > end) {
            phrases.add(new Phrase(start + 1, end + 1, wordsLang1.slice(start, end + 1)))
          } else {
            overlap = true
          }
          end += 1
        }
    }

    phrases
  }

  private[this] def next(q: State, p: Phrase, wordsLang1: Array[String]): State = {
    val numWords = p.words.size
    if (numWords == 0 || p.start > p.end ||
      (p.end + 1) - p.start != numWords)
      throw new IllegalArgumentException

    val newWord1 = if (numWords == 1) q.word2 else p.words(numWords - 2)
    val newWord2 = p.words.last
    val newBitString = q.bitString.clone().asInstanceOf[BitSet]
    newBitString.set(p.start - 1, p.end)
    val newPrevEnd = p.end
    val newScore = q.score + lexicon.estimate(wordsLang1, p.words) +
      languageModel.estimate(p.words) + (distortionPenalty * math.abs(q.prevEnd + 1 - p.start))

    new State(newWord1, newWord2, newBitString, newPrevEnd, newScore)
  }

}

class Beam(val width: Double) {

  var max = -1.0
  val map = collection.mutable.Map[State, State]()

  def add(q: State) {

    val existing = map.getOrElse(q, null)
    if (existing == null) {
      map.put(q, q)
    } else if (q.score > existing.score) {
      map.remove(existing)
      map.put(q, q)
    }
    if (q.score > max) {
      max = q.score
      purge()
    }

  }

  private[this] def purge() {
    map.retain((k, v) => k.score >= max - width)
  }
}

//start and end are 1 indexed
//words is end inclusive
class Phrase(val start: Int, val end: Int, val words: Array[String])

class State(val word1: String, val word2: String, val bitString: BitSet,
  val prevEnd: Int, val score: Double) extends Equals {

  def canEqual(other: Any) = {
    other.isInstanceOf[main.scala.State]
  }

  override def equals(other: Any) = {
    other match {
      case that: main.scala.State => that.canEqual(State.this) && word1 == that.word1 && word2 == that.word2 && bitString == that.bitString && prevEnd == that.prevEnd
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime * (prime * (prime + word1.hashCode) + word2.hashCode) + bitString.hashCode) + prevEnd.hashCode
  }
}