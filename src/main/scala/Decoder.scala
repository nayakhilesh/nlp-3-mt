package main.scala

import java.util.BitSet
import collection.mutable.ArrayBuffer
import collection.immutable.Seq

class Decoder(val lexicon: Lexicon, val languageModel: TrigramLanguageModel,
  val distortionLimit: Int, val distortionPenalty: Double, val beamWidth: Double) {

  println("Created decoder with distortionLimit=" + distortionLimit +
    ", distortionPenalty=" + distortionPenalty + ", beamWidth=" + beamWidth)

  def decode(line: String): String = {

    val words = line split " "
    val n = words.size
    val beams = ArrayBuffer.fill(n + 1)(new Beam(beamWidth))

    val q0 = new State(TrigramLanguageModel.BeforeSymbol, TrigramLanguageModel.BeforeSymbol,
      new BitSet(n), 0, 0)
    beams(0).map.put(q0, q0)

    // back-pointers
    val bp = collection.mutable.Map[State, (State, Phrase)]()

    0 to (n - 1) foreach {
      i =>
        beams(i).map.keys foreach {
          q =>
            ph(q, words) foreach {
              p =>
                val q1 = next(q, p, words)
                val j = q1.bitString.cardinality
                add(beams(j), q1, q, p, bp)
            }
        }
    }

    val keys = beams(n).map.keys

    if (keys.size == 0)
      "No translation found"
    else {
      val maxState = keys maxBy (_.score)

      def follow(q: State): Array[String] =
        bp.getOrElse(q, null) match {
          case null => Array.empty[String]
          case (q1, p) =>
            follow(q1) ++ p.lang2Words
        }

      follow(maxState).mkString(" ")
    }
  }

  private[this] def ph(q: State, wordsLang1: Array[String]): collection.Set[Phrase] = {

    val phrases = collection.mutable.Set[Phrase]()
    //(r + 1) - d <= s <= (r + 1) + d
    math.max((q.prevEnd + 1 - distortionLimit), 0) to
      math.min((q.prevEnd + 1 + distortionLimit), wordsLang1.size - 1) foreach {
        start =>
          var end = start
          var overlap = false
          while (end < wordsLang1.size && !overlap) {
            val nextSet = q.bitString.nextSetBit(start)
            if (nextSet == -1 || nextSet > end) {
              val lang2WordsSet = lexicon.getTranslation(wordsLang1.slice(start, end + 1).toIndexedSeq)
              if (lang2WordsSet != null) {
                lang2WordsSet foreach (
                  lang2Words =>
                    phrases.add(new Phrase(start + 1, end + 1, lang2Words)))
              }
            } else {
              overlap = true
            }
            end += 1
          }
      }

    phrases
  }

  private[this] def add(beam: Beam, q1: State, q: State, p: Phrase,
    bp: collection.mutable.Map[State, (State, Phrase)]) {

    val existing = beam.map.getOrElse(q1, null)
    if (existing == null) {
      beam.map += (q1 -> q1)
      bp(q1) = (q -> p)
    } else if (q1.score > existing.score) {
      beam.map -= existing
      beam.map += (q1 -> q1)
      bp(q1) = (q -> p)
    }
    if (q1.score > beam.max) {
      beam.max = q1.score
      beam.purge()
    }
  }

  private[this] def next(q: State, p: Phrase, wordsLang1: Array[String]): State = {
    val numWords = p.lang2Words.size
    if (numWords == 0 || p.lang1Start > p.lang1End)
      throw new IllegalArgumentException

    val newWord1 = if (numWords == 1) q.word2 else p.lang2Words(numWords - 2)
    val newWord2 = p.lang2Words.last
    val newBitString = q.bitString.clone().asInstanceOf[BitSet]
    newBitString.set(p.lang1Start - 1, p.lang1End)
    val newPrevEnd = p.lang1End
    val newScore = q.score + lexicon.estimate(wordsLang1.slice(p.lang1Start - 1, p.lang1End).toIndexedSeq, p.lang2Words) +
      languageModel.estimate(p.lang2Words) + (distortionPenalty * math.abs(q.prevEnd + 1 - p.lang1Start))

    new State(newWord1, newWord2, newBitString, newPrevEnd, newScore)
  }

}

class Beam(val width: Double) {

  var max = -1.0
  val map = collection.mutable.Map[State, State]()

  def purge() {
    map.retain((k, v) => k.score >= max - width)
  }

}

//start and end are 1 indexed
//i.e. start..end (inclusive) in lang1 is translated as lang2Words
class Phrase(val lang1Start: Int, val lang1End: Int, val lang2Words: Seq[String]) extends Equals {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Phrase]

  override def equals(other: Any): Boolean = other match {
    case that: Phrase =>
      (that canEqual this) &&
        lang1Start == that.lang1Start &&
        lang1End == that.lang1End &&
        lang2Words == that.lang2Words
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(lang1Start, lang1End, lang2Words)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class State(val word1: String, val word2: String, val bitString: BitSet,
  val prevEnd: Int, val score: Double) extends Equals {

  def canEqual(other: Any): Boolean = other.isInstanceOf[State]

  override def equals(other: Any): Boolean = other match {
    case that: State =>
      (that canEqual this) &&
        word1 == that.word1 &&
        word2 == that.word2 &&
        bitString == that.bitString &&
        prevEnd == that.prevEnd
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(word1, word2, bitString, prevEnd)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}