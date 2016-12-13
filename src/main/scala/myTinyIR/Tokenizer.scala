package myTinyIR

//modified to also split words with lowercase letter before capital one
object Tokenizer {
  def tokenize (text: String) : List[String] = {
//    text.toLowerCase.split("[ .,;:?!*&$-+\"\'\t\n\r\f]+").filter(w => w.length >= 3).toList
      val tokens = text.split("[ .,;:?!*&$-+\"\'\t\n\r\f]+").filter(w => w.length > 4).toList
      tokens.flatMap(_.split("(?=\\p{Upper})")).filter(w => w.length > 4).map(_.toLowerCase).toList
  }
}