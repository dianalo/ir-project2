package myTinyIR

import util.Try
import util.Success
import javax.xml.parsers._
import org.w3c.dom.{Document => XMLDoc}
import java.io.InputStream

class TipsterParse(is: InputStream) extends XMLDocument(is) {  
  override def body   : String = read(doc.getElementsByTagName("TEXT"))
  override def ID     : String = read(doc.getElementsByTagName("DOCNO")).filterNot(_.isSpaceChar)
  def dateLine        : String = read(doc.getElementsByTagName("DATELINE"))
}
