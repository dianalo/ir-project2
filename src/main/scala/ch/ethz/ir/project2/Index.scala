package ch.ethz.ir.project2

import myTinyIR._
import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer.PorterStemmer

//totalTerms + sizeIndex needed for languagemodel
class Index(path: String, maxDocs: Int, var titleIndex: Map[String, List[String]], var sizeIndex: Map[String, Int], var totalTerms: Long){
  
//  case class IndexTuple(id: String, tfInDoc: Int, title: String, docFreq: Int) extends Ordered[IndexTuple]{
//    def compare(that: IndexTuple) : Int = (this.id, this.tfInDoc) compare (that.id, that.tfInDoc)
//  }
  
  case class DocPreProcessedTokens(id: String, tokens: List[String])
  case class IndexTuple(id: String, tfInDoc: Int)
  
  //term index
  val index : Map[String,List[IndexTuple]] = {
    var iter = new TipsterCorpusIterator(path)
    
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
      
      //println(docWithTF.toList.filter{_._2.tf > 1})
      //make IndexTuples
      val tuples = docWithTF.mapValues(p => new IndexTuple(p.id, p.tf))
      //add them to indexMap, take care not to overwrite anything      
      val temp = indexMap.toSeq ++ tuples.mapValues(v => List(v)).toSeq
      indexMap = temp.groupBy(_._1).mapValues(_.flatMap(_._2).toList)
      
      i = i+1
      println(i/maxDocs * 100 + "% done.")
    }
    
    //filter out postings with term only appearing once
    indexMap.mapValues(l => l.filter(_.tfInDoc > 1))
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
    //val ts = new TipsterStream("resources/documentsSmall").stream
    
    println("creating index")
    var idx = new Index("resources/documents", 100, Map[String, List[String]](), Map[String, Int](), 0)
    println("created index")
    println(idx.get("revelations").mkString("[", " ", "]"))
  }
}
  