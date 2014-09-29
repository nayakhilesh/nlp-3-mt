package main.scala

import scala.io.Source
import scala.collection.GenTraversableOnce

object Utils {

  type TranslationParameters = collection.immutable.Map[String, collection.mutable.Map[String, Double]]
  type AlignmentParameters = collection.immutable.Map[(Int, Int, Int, Int), Double]

  type MutableTranslationParameters = collection.mutable.Map[String, collection.mutable.Map[String, Double]]
  type MutableAlignmentParameters = collection.mutable.Map[(Int, Int, Int, Int), Double]

  val NULL = "NULL"

  def loopThroughFiles(file1Path: String, file2Path: String, parallel: Boolean = false)(funcToPerform: (String, String, Int) => Unit) {

    val file1Lines = Source.fromFile(file1Path, "utf-8").getLines
    val file2Lines = Source.fromFile(file2Path, "utf-8").getLines

    val iterator = file1Lines zip file2Lines zipWithIndex

    if (parallel) {
      val chunkSize = 10
      val chunkedIterator = iterator.grouped(chunkSize)
      chunkedIterator.foreach(lines => processLines(lines.par, funcToPerform, false))
    } else {
      processLines(iterator, funcToPerform, true)
    }

  }

  private[this] def processLines(lines: GenTraversableOnce[((String, String), Int)],
                                 funcToPerform: (String, String, Int) => Unit, printLineNum: Boolean) {
    lines foreach {
      case ((line1, line2), index) =>
        if (((index + 1) % 200 == 0) && printLineNum) println("line#:" + (index + 1))
        if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
          funcToPerform(line1, line2, index)
        }
    }
  }

  def arrayToString[T](array: Array[T]): String = {
    val sb = collection.mutable.StringBuilder.newBuilder
    array foreach {
      i =>
        sb append i.toString
        sb append " "
    }
    sb toString
  }

}