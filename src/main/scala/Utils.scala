package main.scala

import scala.io.Source

object Utils {

  type TranslationParameters = collection.mutable.HashMap[String, collection.mutable.Map[String, Double]]
  type AlignmentParameters = collection.mutable.HashMap[(Int, Int, Int, Int), Double]

  val NULL = "NULL"

  def loopThroughFiles(file1Path: String, file2Path: String)(funcToPerform: (String, String, Int) => _) {

    val file1Lines = Source.fromFile(file1Path, "utf-8").getLines
    val file2Lines = Source.fromFile(file2Path, "utf-8").getLines

    for (((line1, line2), index) <- file1Lines zip file2Lines zipWithIndex) {
      if ((index + 1) % 200 == 0) println("line#:" + (index + 1))
      if (!line1.trim.isEmpty && !line2.trim.isEmpty) {
        funcToPerform(line1, line2, index)
      }
    }

  }

}