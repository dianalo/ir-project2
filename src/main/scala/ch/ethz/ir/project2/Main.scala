package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing._

object Main extends App {
  override def main(args: Array[String]) : Unit = {
     val trainingDataFolder = "resources/documents/zips/subzips"
     var iter = new TipsterCorpusIterator(trainingDataFolder)
     
     var count = 0
     	while(iter.hasNext){
	    	val doc = iter.next
	    	println(doc.content)

	    	count += 1
	    }
     println("there are " + count + " zip files in this directory")
  }
}