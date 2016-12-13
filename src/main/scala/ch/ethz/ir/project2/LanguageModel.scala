package ch.ethz.ir.project2

import myTinyIR._

class LanguageModel (index: Index, var log_p_qd: Map[String, Double]) {
    
  def computeProbabilities(query: String): List[(String, Double)] = {
    
    val numDocs = 100001
    
    val lambda = 0.15
    
    //reset map
    log_p_qd = Map[String, Double]()
    
    val preprocQuery = Preprocessing.preprocessQuery(Tokenizer.tokenize(query))
    
    // get the total number of different words in the query
    //collection frequencies
    var cfqs = Map[String, Int]()
    var sum = 0
    //compute collection frequency of each query word
    for(word <- preprocQuery){
      sum = 0
      val pList = index.get(word)
      sum = pList.map(_.tfInDoc).sum
      cfqs += (word -> sum)
    }
              	
    	var p_wd = 1.0
    	
    	//optimization to avoid passing over all docs again
    	//get set of docs that contain at least one of the query words
    	//assume it contains at least 100 docs for big data set and any query
    	var candidates = List[String]()
    	for(word <- preprocQuery){
    	  candidates = (candidates++index.get(word).map(_.id)).distinct
    	}
    	
    	for(word <- preprocQuery){
    	  //number of terms matching with query term
    	  val docPost = index.get(word)
    	  //docs that contain word
    	  for(d <- docPost){
    	    
    	    //better estimate of p(w|d)
    	    //tf(queryWord)/totalTermsInDoc + cf/totalTerms
    	    p_wd = ((1-lambda) * d.tfInDoc.toDouble / index.getSize(d.id)) + lambda * (cfqs.getOrElse(word, 0).toDouble / index.totalTerms)
    	    //use this p(w|d) to compute query probability given this doc : p(q|d)
    	    log_p_qd = log_p_qd.updated(d.id, log_p_qd.getOrElse(d.id, 0.0) + Math.log(p_wd))
    	  }
    	  //candidate docs that don't contain word
    	  for(id <- candidates){
    	    if(!docPost.map(_.id).contains(id)){
    	      //only get smoothing value
    	      p_wd = lambda * (cfqs.getOrElse(word, 0).toDouble / index.totalTerms)
    	      log_p_qd = log_p_qd.updated(id, log_p_qd.getOrElse(id, 0.0) + Math.log(p_wd))
    	    }
  	    }
    	    
    	}
    	
    	//output best 100, sorted by score
    	log_p_qd.toList.sortWith{case((a1, b1), (a2, b2)) => b1 > b2}.take(100)
  }
}