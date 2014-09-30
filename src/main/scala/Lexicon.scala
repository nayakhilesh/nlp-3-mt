package main.scala

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Seq

class Lexicon {

  private[this] val translatedPairs = collection.concurrent.TrieMap.empty[Seq[String], collection.mutable.Set[Seq[String]]]
  private[this] val c2 = collection.concurrent.TrieMap.empty[(Seq[String], Seq[String]), Int]
  private[this] val c1 = collection.concurrent.TrieMap.empty[Seq[String], Int]

  def add(lang1Alignments: Seq[Int], lang2Alignments: Seq[Int], line1: String, line2: String) {

    val lang2FinalAlignments = new ArrayBuffer[ArrayBuffer[Int]](lang2Alignments.size)
    val lang1FinalAlignments = new ArrayBuffer[ArrayBuffer[Int]](lang1Alignments.size)

    //find elements in intersection of alignments
    findAlignmentIntersection(lang2Alignments, lang1Alignments, lang2FinalAlignments)
    findAlignmentIntersection(lang1Alignments, lang2Alignments, lang1FinalAlignments)

    //order matters, though which is better?
    growAlignments(lang2FinalAlignments, lang1FinalAlignments, lang2Alignments, lang1Alignments)
    growAlignments(lang1FinalAlignments, lang2FinalAlignments, lang1Alignments, lang2Alignments)

    val words1 = line1 split " "
    val words2 = line2 split " "

    0 until lang2FinalAlignments.size foreach {
      startLang2 =>
        0 until (lang2FinalAlignments.size - startLang2) foreach {
          lengthLang2 =>
            0 until lang1FinalAlignments.size foreach {
              startLang1 =>
                0 until (lang1FinalAlignments.size - startLang1) foreach {
                  lengthLang1 =>

                    if (isConsistent(startLang2, lengthLang2,
                      startLang1, lengthLang1, lang2FinalAlignments) &&
                      isConsistent(startLang1, lengthLang1,
                        startLang2, lengthLang2, lang1FinalAlignments)) {
                      val phraseLang2 = words2.slice(startLang2, startLang2 + lengthLang2 + 1)
                      val phraseLang1 = words1.slice(startLang1, startLang1 + lengthLang1 + 1)
                      //println(arrayToString(phraseLang2) + "|" + arrayToString(phraseLang1))
                      val phraseLang2Seq = phraseLang2.toIndexedSeq
                      val phraseLang1Seq = phraseLang1.toIndexedSeq
                      if (translatedPairs.contains(phraseLang1Seq))
                        translatedPairs(phraseLang1Seq) += phraseLang2Seq
                      else
                        translatedPairs += (phraseLang1Seq -> collection.mutable.Set[Seq[String]](phraseLang2Seq))
                      c2(phraseLang2Seq -> phraseLang1Seq) = c2.getOrElse((phraseLang2Seq -> phraseLang1Seq), 0) + 1
                      c1(phraseLang2Seq) = c1.getOrElse(phraseLang2Seq, 0) + 1
                      //g(e,f) = log c(f,e)/c(f)
                      //e to f translation
                    }

                }
            }
        }
    }

  }

  private[this] def isConsistent(startLang2: Int, lengthLang2: Int,
                                 startLang1: Int, lengthLang1: Int,
                                 lang2FinalAlignments: ArrayBuffer[ArrayBuffer[Int]]): Boolean = {

    (startLang2 to (startLang2 + lengthLang2)) foreach {
      i =>
        val valueSeq = lang2FinalAlignments(i)
        if (valueSeq.size == 0 || valueSeq.exists {
          value => value < startLang1 || value > (startLang1 + lengthLang1)
        })
          return false
    }
    true

  }

  private[this] def findAlignmentIntersection(lang2Alignments: Seq[Int], lang1Alignments: Seq[Int],
                                              lang2FinalAlignments: ArrayBuffer[ArrayBuffer[Int]]) {

    lang2Alignments.iterator.zipWithIndex foreach {
      case (lang1Index, lang2Index) =>
        if (lang1Index > 0 && lang1Alignments(lang1Index - 1) == lang2Index + 1)
          lang2FinalAlignments += (ArrayBuffer[Int]() += lang1Index - 1)
        else lang2FinalAlignments += ArrayBuffer[Int]()
    }

  }

  //g(e,f) = log c(f,e)/c(f)
  //e to f translation
  def estimate(wordsLang1: Seq[String], wordsLang2: Seq[String]): Double = {
    if (!(c2.contains((wordsLang2 -> wordsLang1)) && c1.contains(wordsLang2)))
      0.0
    math.log(c2(wordsLang2 -> wordsLang1).toDouble / c1(wordsLang2))
  }

  def getTranslation(wordsLang1: Seq[String]): collection.Set[Seq[String]] = {
    translatedPairs.getOrElse(wordsLang1, null)
  }

  private[this] def growAlignments(lang2FinalAlignments: IndexedSeq[ArrayBuffer[Int]],
                                   lang1FinalAlignments: IndexedSeq[ArrayBuffer[Int]],
                                   lang2Alignments: Seq[Int], lang1Alignments: Seq[Int]) {

    lang2FinalAlignments.iterator.zipWithIndex foreach {
      case (lang1Seq, index2) =>
        if (lang1Seq.size == 0) {

          var count = -1
          val alignedToLang1Index = lang2Alignments(index2) - 1
          if (alignedToLang1Index >= 0) {
            count = countNeighbours(alignedToLang1Index, index2, lang2FinalAlignments, lang1FinalAlignments)
          }

          var count1 = -1
          var row1 = 0
          lang1Alignments.iterator.zipWithIndex foreach {
            case (lang2Index, index1) =>
              if (lang2Index - 1 == index2) {
                val tempCount = countNeighbours(index1, index2, lang2FinalAlignments, lang1FinalAlignments)
                if (tempCount > count1) {
                  count1 = tempCount
                  row1 = index1
                }
              }
          }

          if (!(count == -1 && count1 == -1)) {
            if (count >= count1) {
              lang2FinalAlignments(index2) += alignedToLang1Index
              lang1FinalAlignments(alignedToLang1Index) += index2
            } else {
              lang2FinalAlignments(index2) += row1
              lang1FinalAlignments(row1) += index2
            }
          }

        }

    }

  }

  private[this] def countNeighbours(row: Int, col: Int,
                                    lang2FinalAlignments: IndexedSeq[collection.mutable.Seq[Int]], lang1FinalAlignments: IndexedSeq[collection.mutable.Seq[Int]]): Int = {

    var count = 0

    (row - 1) to (row + 1) foreach {
      tempRow =>
        (col - 1) to (col + 1) foreach {
          tempCol =>
            if (tempRow >= 0 && tempRow < lang1FinalAlignments.size &&
              tempCol >= 0 && tempCol < lang2FinalAlignments.size &&
              !(tempRow == row && tempCol == col) &&
              lang2FinalAlignments(tempCol).contains(tempRow)) {
              count += 1
            }
        }
    }

    count
  }

}