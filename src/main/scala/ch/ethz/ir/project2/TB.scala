package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._

class TB (trainingDataFolder: String){
  println("Running Term Based")
  
  val streamSize=1000  
    
  def termScore(query:String): Map[String,Double]={
    //input: doc stream
    //output: <doc, query-match-score>
        
    val stream = new TipsterStream (trainingDataFolder)  
    val docNo=stream.length
    println("Total document number: " +docNo )
    var doc_tfidf = collection.Map[String,Int]()
    
    val queryShingled=Tokenizer.tokenize(query)
    println("Query: "+ queryShingled)
    var doc_queryScore=collection.immutable.Map[String,Double]()
    
    for (doc <- stream.stream.take(streamSize)) { 
      
      val docID=doc.ID
      println("Current Doc id: "+docID)
      var localQScore=0.0;
      
      val LTF:Map[String,Double]=TermFrequencies.logtf(Tokenizer.tokenize(doc.body))
     
      var df = collection.immutable.Map[String,Int]()
      for (doc <- stream.stream){
        df ++= doc.tokens.distinct.map(t => t.toLowerCase() -> (1+df.getOrElse(t,0)))
      }
            
      val IDF:Map[String,Double]=TermFrequencies.idf(df,docNo)
      val TFIDF=LTF++IDF.map{case(k,v)=>k->(v*LTF.getOrElse(k,1.0))}
      
      for(q <- queryShingled){
        localQScore+=TFIDF(q)
      }
      
      println(docID+"'s query score: "+localQScore)
      doc_queryScore+=(docID.toString()->localQScore)
    }
    
    
    val qScoreSorted=doc_queryScore.toSeq.sortWith(_._2 > _._2).take(100).toMap
    for ((k,v) <- qScoreSorted) println("key: %s, value: %s\n", k, v)
    
    return qScoreSorted
}
 
 
  /*
  def evaluate(query:String):Double={
    // P, R , F1, AP, AMP
  }
  */
    
}