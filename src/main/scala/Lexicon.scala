package main.scala

import scala.collection.mutable.ArrayBuffer

class Lexicon {

  private[this] val translatedPairs = new collection.mutable.HashSet[(Array[String], Array[String])]
  private[this] val c2 = new collection.mutable.HashMap[(Array[String], Array[String]), Int]
  private[this] val c1 = new collection.mutable.HashMap[Array[String], Int]

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

    var startLang2 = 0
    var lengthLang2 = 0
    var startLang1 = 0
    var lengthLang1 = 0

    while (startLang2 < lang2FinalAlignments.size) {
      lengthLang2 = 0
      while (lengthLang2 < lang2FinalAlignments.size - startLang2) {
        startLang1 = 0
        while (startLang1 < lang1FinalAlignments.size) {
          lengthLang1 = 0
          while (lengthLang1 < lang1FinalAlignments.size - startLang1) {

            if (checkConsistent(startLang2, lengthLang2,
              startLang1, lengthLang1, lang2FinalAlignments) &&
              checkConsistent(startLang1, lengthLang1,
                startLang2, lengthLang2, lang1FinalAlignments)) {
              val phraseLang2 = words2.slice(startLang2, startLang2 + lengthLang2 + 1)
              val phraseLang1 = words1.slice(startLang1, startLang1 + lengthLang1 + 1)
              //println(arrayToString(phraseLang2) + "|" + arrayToString(phraseLang1))
              translatedPairs += (phraseLang1 -> phraseLang2)
              c2(phraseLang2 -> phraseLang1) = c2.getOrElse((phraseLang2 -> phraseLang1), 0) + 1
              c1(phraseLang2) = c1.getOrElse(phraseLang2, 0) + 1
              //g(e,f) = log c(f,e)/c(f)
              //e to f translation
            }

            lengthLang1 += 1
          }
          startLang1 += 1
        }
        lengthLang2 += 1
      }
      startLang2 += 1
    }

  }

  private[this] def checkConsistent(startLang2: Int, lengthLang2: Int,
    startLang1: Int, lengthLang1: Int,
    lang2FinalAlignments: ArrayBuffer[ArrayBuffer[Int]]): Boolean = {

    var temp = startLang2
    while (temp <= startLang2 + lengthLang2) {
      val valueSeq = lang2FinalAlignments(temp)
      if (valueSeq.size == 0 || valueSeq.exists { value => value < startLang1 || value > (startLang1 + lengthLang1) })
        false
      temp += 1
    }
    true

  }

  private[this] def findAlignmentIntersection(lang2Alignments: Seq[Int], lang1Alignments: Seq[Int],
    lang2FinalAlignments: ArrayBuffer[ArrayBuffer[Int]]) {

    (lang2Alignments zipWithIndex) foreach {
      case (lang1Index, lang2Index) =>
        if (lang1Index > 0 && lang1Alignments(lang1Index - 1) == lang2Index + 1)
          lang2FinalAlignments += (ArrayBuffer[Int]() += lang1Index - 1)
        else lang2FinalAlignments += ArrayBuffer[Int]()
    }

  }

  // TODO
  def estimate(wordsLang1: Array[String], wordsLang2: Array[String]): Double = {
    0.0
  }

  private[this] def growAlignments(lang2FinalAlignments: IndexedSeq[ArrayBuffer[Int]],
    lang1FinalAlignments: IndexedSeq[ArrayBuffer[Int]],
    lang2Alignments: Seq[Int], lang1Alignments: Seq[Int]) {

    (lang2FinalAlignments zipWithIndex) foreach {
      case (lang1Seq, index2) =>
        if (lang1Seq.size == 0) {

          var count = -1
          val alignedToLang1Index = lang2Alignments(index2) - 1
          if (alignedToLang1Index >= 0) {
            count = countNeighbours(alignedToLang1Index, index2, lang2FinalAlignments, lang1FinalAlignments)
          }

          var count1 = -1
          var row1 = 0
          (lang1Alignments zipWithIndex) foreach {
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
    lang2FinalAlignments: IndexedSeq[Seq[Int]], lang1FinalAlignments: IndexedSeq[Seq[Int]]): Int = {

    var count = 0

    var tempRow = row - 1
    while (tempRow <= row + 1) {
      var tempCol = col - 1
      while (tempCol <= col + 1) {

        if (tempRow >= 0 && tempRow < lang1FinalAlignments.size &&
          tempCol >= 0 && tempCol < lang2FinalAlignments.size &&
          !(tempRow == row && tempCol == col) &&
          lang2FinalAlignments(tempCol).contains(tempRow)) {
          count += 1
        }

        tempCol += 1
      }
      tempRow += 1
    }

    count
  }

}