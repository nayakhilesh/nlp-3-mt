package main.scala

import Utils.TranslationParameters

trait IbmModelLike {

  // TODO break up into atomic traits?
  //computeParams

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String)

  def writeParams(filePath: String)

  def readParams(filePath: String)

  // TODO
  def serializeParams(filePath: String)

  def deserializeParams(filePath: String): TranslationParameters

}