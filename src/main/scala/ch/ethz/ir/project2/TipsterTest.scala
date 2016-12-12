package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.indexing._
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator

  object TipsterTest extends App {
  override def main(args: Array[String]){

    val str = new TipsterStream("resources/documentsSmall").stream
    //val iter = new TipsterCorpusIterator("resources/documentMedium").toStream
//    println(str.take(1).head.ID)
//    println(str.take(1).head.body)
//    println(str.take(1).head.title)
//    println(str.take(1).head.name)
//    println(str.take(1).head.date)
    var si = new SimpleIndex(str)
    println(si.results("revelations").mkString("[", " ", "]"))

  }
}