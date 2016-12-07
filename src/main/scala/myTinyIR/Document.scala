package myTinyIR

import util.Try

abstract class Document {
  def title  : String 
  def body   : String
  def ID     : String
  def tokens : List[String] = Tokenizer.tokenize(body)
}
