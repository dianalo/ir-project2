package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing._
import Math.log10

class languageModel (trainingDataFolder: String) {
  def getFQIndex() = {
    var iter = new TipsterCorpusIterator(trainingDataFolder)
    
    // get frequency index
//    val fqIndex = Map[String,List[(String,Int)]]()
    var (temp, totalNum) = useTFTuple.tfTuples(iter)
    val fqIndex = temp.groupBy(_.term)
      .mapValues(_.map(tfT => (tfT.doc, tfT.count)).toList.sorted)
      
    (fqIndex, totalNum)
  }

  def log2 (x : Double) = log10(x)/log10(2.0)
  
  def computeProbabilities(query: List[String], lambda: Double): Map[String, Double] = {
    val (fqIndex, totalNum) = getFQIndex()
//    println(fqIndex)
    
    var logDocProb = Map[String, Double]()
    var iter = new TipsterCorpusIterator(trainingDataFolder)
    
    // get the total number of different words in the query
    var numOfWord = Map[String, Int]()
    var sum = 0
    for(word <- query){
      sum = 0
      fqIndex.get(word)match {
       case Some(i) => i.foreach(sum += _._2)
       case None => sum = 0
      }
      println("there are " + sum + " words of " + word)
      numOfWord += (word -> sum)
    }
    
    
    
   	while(iter.hasNext){
    	val doc = iter.next
    	val wordList = doc.content.toLowerCase.split("[ .,;:?!*&$-+\"\'\t\n\r\f]+")
    	var prob: Double = 1
    	for(word <- query){
    	  val nTerms = wordList.filter { _ == word }.size.toDouble
    	  var sum = 0
//    	  println(fqIndex.get(term).size)
    	  prob *= ((1-lambda) * (numOfWord.getOrElse(word, 0).toDouble / totalNum.toDouble) + lambda * nTerms / wordList.size.toDouble)
    	  println("for document " + doc.name + ", the prob is " + prob)    	
    	}
    	logDocProb = logDocProb + (doc.name -> prob)
    }
    logDocProb 
  }
}