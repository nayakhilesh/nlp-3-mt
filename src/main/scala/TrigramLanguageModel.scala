package main.scala

import scala.io.Source
import scala.util.Random

class TrigramLanguageModel {

  val c1 = collection.mutable.Map[String, Int]()
  val c2 = collection.mutable.Map[(String, String), Int]()
  val c3 = collection.mutable.Map[(String, String, String), Int]()
  var totalNumWords = 0

  val beforeSymbol = "*"
  val afterSymbol = "STOP"
  val trainingPercent = 0.95
  val epsilon = 10E-6

  // TODO refactor
  def initialize(filePath: String) {

    val numLines = Source.fromFile(filePath, "utf-8").getLines.size
    val partitionPoint = (numLines * trainingPercent).toInt

    val fileLines = Source.fromFile(filePath, "utf-8").getLines

    val (trainingData, validationData) = (fileLines zipWithIndex) span { case (line, index) => index < partitionPoint }

    trainingData foreach {
      case (line, index) =>
        if ((index + 1) % 200 == 0) println("line#:" + (index + 1))

        val words = line split " "

        var word1: String = beforeSymbol
        var word2: String = beforeSymbol
        var word3: String = beforeSymbol
        words foreach {
          case word =>
            word1 = word2
            word2 = word3
            word3 = word
            c3((word1, word2, word3)) = c3.getOrElse((word1, word2, word3), 0) + 1
            c2((word2, word3)) = c2.getOrElse((word2, word3), 0) + 1
            c1(word3) = c1.getOrElse(word3, 0) + 1
            totalNumWords += 1
        }
        c3((word2, word3, afterSymbol)) = c3.getOrElse((word2, word3, afterSymbol), 0) + 1
        c2((word3, afterSymbol)) = c2.getOrElse((word3, afterSymbol), 0) + 1
        c1(afterSymbol) = c1.getOrElse(afterSymbol, 0) + 1

    }

    val cPrime = collection.mutable.Map[(String, String, String), Int]()

    validationData foreach {
      case (line, index) =>
        if ((index + 1) % 200 == 0) println("line#:" + (index + 1))

        val words = line split " "

        var word1: String = beforeSymbol
        var word2: String = beforeSymbol
        var word3: String = beforeSymbol
        words foreach {
          case word =>
            word1 = word2
            word2 = word3
            word3 = word
            cPrime((word1, word2, word3)) = cPrime.getOrElse((word1, word2, word3), 0) + 1

        }
        cPrime((word2, word3, afterSymbol)) = cPrime.getOrElse((word2, word3, afterSymbol), 0) + 1

    }

    val random = new Random
    var lambda1 = random nextDouble
    var lambda2 = random nextDouble
    var lambda3 = 1 - (lambda1 + lambda2)

    var prevLambda1 = 0.0
    var prevLambda2 = 0.0
    var prevLambda3 = 0.0

    //EM here 

    do {

      var count1 = 0.0
      var count2 = 0.0
      var count3 = 0.0

      prevLambda1 = lambda1
      prevLambda2 = lambda2
      prevLambda3 = lambda3

      cPrime foreach {
        case ((word1, word2, word3), numOccurences) =>

          val part1 = lambda1 * qML(word1, word2, word3)
          val part2 = lambda2 * qML(word2, word3)
          val part3 = lambda3 * qML(word3)
          val denominator = part1 + part2 + part3
          count1 += ((numOccurences * part1) / denominator)
          count2 += ((numOccurences * part2) / denominator)
          count3 += ((numOccurences * part3) / denominator)
      }

      val sum = count1 + count2 + count3
      lambda1 = count1 / sum
      lambda2 = count2 / sum
      lambda3 = count3 / sum

    } while (!(doublesAreEqual(lambda1, prevLambda1) && doublesAreEqual(lambda2, prevLambda2) &&
      doublesAreEqual(lambda3, prevLambda3)))

  }

  private[this] def doublesAreEqual(d1: Double, d2: Double) = {
    math.abs(d1 - d2) < epsilon
  }

  private[this] def qML(word1: String, word2: String, word3: String) = {
    c3((word1, word2, word3)) / c2((word1, word2))
  }

  private[this] def qML(word1: String, word2: String) = {
    c2((word1, word2)) / c1(word1)
  }

  private[this] def qML(word1: String) = {
    c1(word1) / totalNumWords
  }

}