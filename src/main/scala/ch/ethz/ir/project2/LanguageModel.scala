package ch.ethz.ir.project2

import myTinyIR._

class languageModel (path: String, index: Index, maxDocs: Int) {
  
  def computeProbabilities(query: String, lambda: Double): List[(String, Double)] = {    
    var logDocProb = Map[String, Double]()
    
    val preprocQuery = Preprocessing.preprocessQuery(Tokenizer.tokenize(query))
    
    // get the total number of different words in the query
    //collection frequencies
    var numOfWord = Map[String, Int]()
    var sum = 0
    //compute collection frequency of each query word
    for(word <- preprocQuery){
      sum = 0
      val pList = index.get(word)
      sum = pList.map(_.tfInDoc).sum
      numOfWord += (word -> sum)
    }
      
    //loop again through docs, to get ids
    var iter = new TipsterCorpusIterator(path)
    var i=0
    
   	while(iter.hasNext && i < maxDocs){
    	val doc = iter.next
    	//val wordList = doc.content.toLowerCase.split("[ .,;:?!*&$-+\"\'\t\n\r\f]+")
    	
    	var prob: Double = 1
    	for(word <- preprocQuery){
    	  //number of terms matching with query term
    	  val docPost = index.get(word).filter(_.id == doc.ID)
    	  var nTerms = 0
    	  if(docPost.nonEmpty){
    	    nTerms = docPost.head.tfInDoc
    	  }
    	  var sum = 0

    	  //cf/totalTerms + tf(queryWord)/totalTermsInDoc
    	  prob *= ((1-lambda) * (numOfWord.getOrElse(word, 0).toDouble / index.totalTerms) + lambda * nTerms / index.getSize(doc.ID).toDouble)

    	}
    	logDocProb = logDocProb + (doc.ID -> prob)
    	
    	i=i+1
    }
    logDocProb.toList.sorted.take(100)
  }
}