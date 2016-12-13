package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import com.github.aztek.porterstemmer.PorterStemmer

//compute tf-idf weigths for documents
class TB (index: Index, var scores: Map[String, Double]){  
  val docNo = 100001
  val wTitle = 100
    
  //input: query
  //output: <doc, query-match-score>, 100 documents with best scores, sorted by score
  def termScore(query:String): List[(String,Double)]={
    
    //reset map
    scores = Map[String, Double]()
        
    //preprocessing
    val queryPreProc = Preprocessing.preprocessQuery(Tokenizer.tokenize(query))
               
     for(q<-queryPreProc){
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
        
        //look at title of interesting doc, no log, as only few appearances
        val title = index.getTitle(qDoc.id)
        val tfTitle = Preprocessing.preprocessQuery(title).groupBy(identity).mapValues(_.length).getOrElse(q, 0)
        
        //reward term frequency in title
        val tfidf = (ltf+wTitle*tfTitle)*idf
        //without title rewarding
        //val tfidf = ltf*idf
        
        //update score for this doc
        val oldV = scores.getOrElse(qDoc.id, 0.0) //each doc should be in map     
        scores = scores.updated(qDoc.id, oldV + tfidf)
      }
      
    }
      //output top 100 sorted scores
      scores.toList.sortWith{case((a1, b1), (a2, b2)) => b1 > b2}.take(100)
}
    
}