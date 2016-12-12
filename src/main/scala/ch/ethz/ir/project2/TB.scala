package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import com.github.aztek.porterstemmer.PorterStemmer

//compute tf-idf weigths for documents
class TB (index: Index){  
  val docNo = 100000 
    
  //input: query
  //output: <doc, query-match-score>, sorted by score
  def termScore(query:String): List[(String,Double)]={
    
    var doc_tfidf = collection.Map[String,Int]()
    
    //preprocessing
    val queryPreProc = Preprocessing.preprocessQuery(Tokenizer.tokenize(query))
   
//  println("Query: "+ queryPreProc)
    var doc_queryScore=collection.immutable.Map[String,Double]()
    
    var scores = Map[String, Double]()
    
    for(q <- queryPreProc){
      //fetch posting list
      val qDocs = index.get(q)
      //add documents to overall score map, take care not to add duplicates
      val docScoreMap = qDocs.map(d => (d.id, 0.0)).toMap
      val temp = scores.toSeq ++ docScoreMap.toSeq
      scores = temp.groupBy(_._1).mapValues(_.map(_._2).toList.sum)
      //compute values needed for scoring
      val df = qDocs.length
      val idf = Math.log(docNo / (df+1))
      //some values are doc specific (local TF)
      for(qDoc <- qDocs){
        val ltf = Math.log(qDoc.tfInDoc+1)
        
        val tfidf = ltf*idf
        
        //update score for this doc
        val oldV = scores.getOrElse(qDoc.id, 0.0) //each doc should be in map     
        scores = scores.updated(qDoc.id, oldV + tfidf)
      }
      
    }
      //output top 100 sorted scores
      val scoresL = scores.toList.sorted
      scoresL.take(100)
}
 
 
  /*
  def evaluate(query:String):Double={
    // P, R , F1, AP, AMP
  }
  */
    
}