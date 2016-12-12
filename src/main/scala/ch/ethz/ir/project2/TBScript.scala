package ch.ethz.ir.project2

object TBScript {
    def main(args: Array[String]) : Unit = {
  	val path="resources/documents"  	  	
  	var query="Cruelty to Animals Prevention Legislation Passed"
  	
  	val index = new Index(path, 100, Map[String, List[String]](), Map[String, Int](), 0)
  	
  	val tb = new TB(index)    
	  val ranking = tb.termScore(query)
	  
	  var i=1
	  for(d <- ranking){
	    println(i + ") Document\t" + d._1 + "\tScore\t" + d._2)
	  }
  }
}