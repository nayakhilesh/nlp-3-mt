package alignments

trait IbmModelLike {

  //computeParams

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String)

  def writeParams(filePath: String)

  def readParams(filePath: String)

}