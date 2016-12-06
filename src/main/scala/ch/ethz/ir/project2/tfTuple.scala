package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing._

case class tfTuple (term: String, doc: String, count: Int)

object useTFTuple{
  def tfTuples (iter: TipsterCorpusIterator) : (Stream[tfTuple], Long) = {
    var tfList = Stream[tfTuple]()
    var sum: Long = 0
   	while(iter.hasNext){
    	val doc = iter.next
    	tfList = tfList ++ doc.tokens.groupBy(identity).map{ case (tk,lst) => tfTuple(tk, doc.name, lst.length) }.toList
    	sum += doc.content.split("[ .,;:?!*&$-+\"\'\t\n\r\f]+").size
    }
    (tfList, sum)
  }
}