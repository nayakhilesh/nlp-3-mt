package main.scala

import Utils.loopThroughFiles

trait IbmModelLike {

  // TODO break up into atomic traits?

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String) {

    println("Writing alignments:")

    val outputFile = new java.io.FileWriter(outputFilePath)

    loopThroughFiles(input1FilePath, input2FilePath) {
      (line1, line2, index) =>
        val list = extractAlignments(line1, line2)
        (list zipWithIndex) foreach {
          case (maxIndex, index2) =>
            outputFile.write((index + 1) + " " + maxIndex + " " + (index2 + 1) + "\n")
        }
    }

    outputFile.close()

    println("Done Writing alignments")

  }

  def extractAlignments(line1: String, line2: String): List[Int]

  def writeParams(filePath: String)

  def readParams(filePath: String)

}