package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing._

object languageModelScript extends App {
  override def main(args: Array[String]) : Unit = {
    val index = new Index("resources/documents", 20000, Map[String, List[String]](), Map[String, Int](), 0)
    
    val lm = new languageModel("resources/documents", index, 20000)
    val query = "allows applications"
    val docProbabilities = lm.computeProbabilities(query, 0.5)
    println(docProbabilities)
    
    // get the top 100 results
    val result = docProbabilities.toSeq.sortBy(_._2).take(100).map(i => i._1).toList
    println("The result is " + result)
    
  }
}