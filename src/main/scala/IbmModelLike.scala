package main.scala

import Utils.loopThroughFiles

trait IbmModelLike {

  // TODO break up into atomic traits?
  //computeParams

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String) {

    println("Writing alignments:")

    val outputFile = new java.io.FileWriter(outputFilePath)

    loopThroughFiles(input1FilePath, input2FilePath)((line1: String, line2: String, index: Int) => {

      val list = extractAlignments(line1, line2)
      for ((maxIndex, index2) <- list zipWithIndex) {
        outputFile.write((index + 1) + " " + maxIndex + " " + (index2 + 1) + "\n")
      }

    })

    outputFile.close()

    println("Done Writing alignments")

  }

  def extractAlignments(line1: String, line2: String): List[Int]

  def writeParams(filePath: String)

  def readParams(filePath: String)

}