package main.scala

import scala.Array.canBuildFrom
import scala.compat.Platform
import scala.io.Source
import scala.util.Random

object TrigramLanguageModel {

  //constants
  val BeforeSymbol = "*"
  val AfterSymbol = "STOP"
  private val ValidationDataFraction = 0.05
  private val Epsilon = 10E-6
  private val NumPartitions = 4

}

class TrigramLanguageModel {

  //uses Linear Interpolation with history partitions

  //state
  private[this] val c1 = collection.mutable.Map[String, Int]()
  private[this] val c2 = collection.mutable.Map[(String, String), Int]()
  private[this] val c3 = collection.mutable.Map[(String, String, String), Int]()
  private[this] var totalNumWords = 0
  private[this] var lambda1 = new Array[Double](TrigramLanguageModel.NumPartitions)
  private[this] var lambda2 = new Array[Double](TrigramLanguageModel.NumPartitions)
  private[this] var lambda3 = new Array[Double](TrigramLanguageModel.NumPartitions)

  def initialize(filePath: String) {

    val numLines = Source.fromFile(filePath, "utf-8").getLines.size
    val partitionPoint = (numLines * (1.0 - TrigramLanguageModel.ValidationDataFraction)).toInt

    val fileLines = Source.fromFile(filePath, "utf-8").getLines

    val (trainingData, validationData) =
      (fileLines zipWithIndex) span { case (line, index) => index < partitionPoint }

    trainingData foreach {
      case (line, index) =>
        if ((index + 1) % 200 == 0) println("line#:" + (index + 1))

        val words = line split " "
        totalNumWords += words.size
        val newWords = TrigramLanguageModel.BeforeSymbol +: TrigramLanguageModel.BeforeSymbol +:
          words :+ TrigramLanguageModel.AfterSymbol

        newWords sliding (3) foreach {
          case seq =>
            val word1 = seq(0)
            val word2 = seq(1)
            val word3 = seq(2)
            c3((word1, word2, word3)) = c3.getOrElse((word1, word2, word3), 0) + 1
            c2((word2, word3)) = c2.getOrElse((word2, word3), 0) + 1
            c1(word3) = c1.getOrElse(word3, 0) + 1
        }

    }

    val cPrime = collection.mutable.Map[(String, String, String), Int]()

    validationData foreach {
      case (line, index) =>
        if ((index + 1) % 200 == 0) println("line#:" + (index + 1))

        val words = line split " "
        val newWords = TrigramLanguageModel.BeforeSymbol +: TrigramLanguageModel.BeforeSymbol +:
          words :+ TrigramLanguageModel.AfterSymbol

        newWords sliding (3) foreach {
          case seq =>
            val word1 = seq(0)
            val word2 = seq(1)
            val word3 = seq(2)
            cPrime((word1, word2, word3)) = cPrime.getOrElse((word1, word2, word3), 0) + 1
        }

    }

    0 to (TrigramLanguageModel.NumPartitions - 1) foreach {
      case i =>
        val (lambda1bucketI, lambda2bucketI, lambda3bucketI) = emAlgorithm(cPrime, i)
        lambda1(i) = lambda1bucketI
        lambda2(i) = lambda2bucketI
        lambda3(i) = lambda3bucketI
    }

  }

  private[this] def emAlgorithm(cPrime: collection.mutable.Map[(String, String, String), Int],
    bucket: Int): (Double, Double, Double) = {

    val random = new Random
    var lambda1 = random.nextDouble
    var lambda2 = random.nextDouble
    var lambda3 = 1 - (lambda1 + lambda2)

    var prevLambda1 = 0.0
    var prevLambda2 = 0.0
    var prevLambda3 = 0.0

    //EM here
    val startEm = Platform.currentTime

    do {

      var count1 = 0.0
      var count2 = 0.0
      var count3 = 0.0

      prevLambda1 = lambda1
      prevLambda2 = lambda2
      prevLambda3 = lambda3

      cPrime foreach {
        case ((word1, word2, word3), numOccurences) =>

          if (phi(word1, word2) == bucket) {
            val part1 = lambda1 * qML(word1, word2, word3)
            val part2 = lambda2 * qML(word2, word3)
            val part3 = lambda3 * qML(word3)
            val denominator = part1 + part2 + part3
            if (denominator != 0) {
              count1 += ((numOccurences * part1) / denominator)
              count2 += ((numOccurences * part2) / denominator)
              count3 += ((numOccurences * part3) / denominator)
            }
          }
      }

      val sum = count1 + count2 + count3
      lambda1 = count1 / sum
      lambda2 = count2 / sum
      lambda3 = count3 / sum

    } while (!(doublesAreEqual(lambda1, prevLambda1) &&
      doublesAreEqual(lambda2, prevLambda2) &&
      doublesAreEqual(lambda3, prevLambda3)))

    val endEm = Platform.currentTime

    println("TrigramLanguageModel EM time=" + (endEm - startEm) / 1000.0 + "s")

    (lambda1, lambda2, lambda3)
  }

  private[this] def phi(word1: String, word2: String): Int =
    c2.getOrElse((word1, word2), 0) match {
      case 0 => 0
      case 1 | 2 => 1
      case 3 | 4 | 5 => 2
      case _ => 3
    }

  def estimate(words: Seq[String]): Double = {

    val newWords = TrigramLanguageModel.BeforeSymbol +: TrigramLanguageModel.BeforeSymbol +:
      words :+ TrigramLanguageModel.AfterSymbol

    (newWords sliding (3)).foldLeft(0.0) {
      case (acc, seq) =>
        val word1 = seq(0)
        val word2 = seq(1)
        val word3 = seq(2)
        acc + math.log(q(word1, word2, word3))
    }
  }

  private[this] def q(word1: String, word2: String, word3: String) = {
    val bucket = phi(word1, word2)
    (lambda1(bucket) * qML(word1, word2, word3)) + (lambda2(bucket) * qML(word2, word3)) +
      (lambda3(bucket) * qML(word1))
  }

  private[this] def doublesAreEqual(d1: Double, d2: Double) = {
    math.abs(d1 - d2) < TrigramLanguageModel.Epsilon
  }

  private[this] def qML(word1: String, word2: String, word3: String): Double = {
    if (!c2.contains((word1, word2))) {
      0.0
    } else {
      c3.getOrElse((word1, word2, word3), 0).toDouble / c2((word1, word2)).toDouble
    }
  }

  private[this] def qML(word1: String, word2: String): Double = {
    if (!c1.contains(word1)) {
      0.0
    } else {
      c2.getOrElse((word1, word2), 0).toDouble / c1(word1).toDouble
    }
  }

  private[this] def qML(word1: String): Double = {
    if (totalNumWords == 0) {
      0.0
    } else {
      c1.getOrElse(word1, 0).toDouble / totalNumWords.toDouble
    }
  }

}