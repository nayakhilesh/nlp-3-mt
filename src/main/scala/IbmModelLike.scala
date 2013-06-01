package main.scala

trait IbmModelLike {

  // TODO break up into atomic traits?
  //computeParams

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String)

  def writeParams(filePath: String)

  def readParams(filePath: String)

}