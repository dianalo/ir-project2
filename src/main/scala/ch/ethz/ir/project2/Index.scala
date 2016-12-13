package ch.ethz.ir.project2

import myTinyIR._
import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer.PorterStemmer

case class DocPreProcessedTokens(id: String, tokens: List[String])
case class IndexTuple(id: String, tfInDoc: Int)

class Index(path: String, var maxDocs: Int, var titleIndex: Map[String, List[String]], var sizeIndex: Map[String, Int], var totalTerms: Long){
     
  //term index
  val index : Map[String,List[IndexTuple]] = {
    var iter = new TipsterCorpusIterator(path)
    
    if(maxDocs == 0){
      maxDocs = 100001
    }
    
    var indexMap = Map[String, List[IndexTuple]]()
    var i=0.0
    
    while(iter.hasNext && i < maxDocs){
      val doc = iter.next()
      
      //store title in title index
      val title = Preprocessing.preprocessQuery(Tokenizer.tokenize(doc.title))
      titleIndex = titleIndex + (doc.ID -> title)
      
      //process doc
      
      //preprocessing
      val preproc = new DocPreProcessedTokens(doc.ID, Preprocessing.preprocessDoc(doc.tokens))
      
      totalTerms += preproc.tokens.length
      
      sizeIndex += (doc.ID -> preproc.tokens.length)
      
      //make posting for each term
      //add termfrequencies per document
      val docWithTF = preproc.tokens.groupBy(identity).mapValues(_.length).toList
                      .map{case (t, l) => (t, new TermDocTuple(t, l, preproc.id))}.toMap
      
      //make IndexTuples
      val tuples = docWithTF.mapValues(p => new IndexTuple(p.id, p.tf))

      for(k <- tuples.keys){
        val oldVal = indexMap.getOrElse(k, List())
        val newVal = (tuples.getOrElse(k, new IndexTuple("",0))::oldVal).reverse
        indexMap = indexMap.updated(k, newVal)
      }
      
      i = i+1
      println(i/maxDocs * 100 + "% done.")
    }
    
    indexMap
  }
  
  
  case class TermDocTuple(term: String, tf: Int, id: String)
    
  def get(key: String) : List[IndexTuple] = {
    index.getOrElse(key, Nil)
  }
  
  def getTitle(key: String) : List[String] = {
    titleIndex.getOrElse(key, Nil)
  }
  
  def getSize(key: String) : Int = {
    sizeIndex.getOrElse(key, 1)
  }
  
}

object Index {
  def main(args: Array[String]){
    
    println("creating index")
    var idx = new Index("resources/documents", 100, Map[String, List[String]](), Map[String, Int](), 0)
    println("created index")
    println(idx.get("revelations").mkString("[", " ", "]"))
  }
}
  