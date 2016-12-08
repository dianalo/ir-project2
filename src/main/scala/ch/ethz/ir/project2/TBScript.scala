package ch.ethz.ir.project2

object TBScript {
    override def main(args: Array[String]) : Unit = {
  	val path="~/Document/documents_small/"
  	val tb = new TB(path)
  	
  	var query="Cruelty to Animals Prevention Legislation Passed" 	
        
	  tb.termScore(path,query)
    
  }
}