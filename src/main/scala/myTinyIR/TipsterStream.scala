package myTinyIR

class TipsterStream (path: String, ext: String = "") 
extends ParsedXMLStream(new ZipDirStream(path, "")){
  def stream : Stream[XMLDocument] = unparsed.stream.map(is => new TipsterParse(is))
  def length = unparsed.length 
}
