package ch.ethz.ir.project2

object TBScript {
    def main(args: Array[String]) : Unit = {
  	val path="~/Document/documents_small/"  	  	
  	var query="Cruelty to Animals Prevention Legislation Passed"
  	
  	val tb = new TB(path)    
	  tb.termScore(query)
    
  }
}