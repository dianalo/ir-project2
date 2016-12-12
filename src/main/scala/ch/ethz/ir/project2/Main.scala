package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing._
import java.io.BufferedReader
import java.io.FileReader
import java.util.regex.Pattern
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

object main {
  def main(args: Array[String]) : Unit = {    
     val trainingDataFolder = "resources/documents"
     val maxDocs = 5000
     
     println("constructing index")
     val ind = new Index(trainingDataFolder, maxDocs, Map[String, List[String]](), Map[String, Int](), 0)
          
     //println(test.mkString("[", ", ", "]"))
           
     //initialize termbased model
     println("scoring...")
     var tb = new TB(ind)
     
     //initialize language model
     var lm = new languageModel(trainingDataFolder, ind, 5000)
     
     
    if(args(0) == "-training"){
      
      println("training mode")
     
      //read in queries
     println("reading in queries")
     var fr = new FileReader("resources/topic_descriptions.txt")
     var br = new BufferedReader(fr)
     var queries = List[(String, Int)]()
     var line = br.readLine()
     val patQ = Pattern.compile("<title> Topic:  [\\w\\s]+")
     val patN = Pattern.compile("<num> Number:  [0-9]+")
     
     var n = 0 //to save current query number
     
     while(line != null){
     val mQ = patQ.matcher(line)
     val mN = patN.matcher(line)
       
       if(mQ.find()){
         val q = mQ.group().substring(16)
         queries = (q,n)::queries
       }
       else if(mN.find()){
         n = mN.group().substring(16).toInt //strip the 0
       }
       line = br.readLine()
     }
     queries = queries.reverse
     
     br.close()
     fr.close()
     //println(queries.mkString("[", ", ", "]")) 
      
      
      //read in qrels
       println("reading in qrels")
       val fr2 = new FileReader("resources/qrels.txt")
       val br2 = new BufferedReader(fr2)
       
       //map with qrels
       var docRelevance = Map[(String, Int), Boolean]()
       
       var line2 = br2.readLine()
       while(line2 != null){
         val subs = line2.split("\\s")
         val r = subs(3) == "1"
         docRelevance = docRelevance + ((subs(2), subs(0).toInt) -> r)
         line2 = br2.readLine()
       }
       
       fr2.close()
       br2.close()
       
       //println(docRelevance.mkString("[", ", ", "]"))
       
       
       
       var AP_term = Map[Int, Double]()
       var AP_lang = Map[Int, Double]()
       
       var i_q = 1.0
       for(q <- queries){
         println("scoring query " + q._2)
         //termbased model
         val topDocsTerm = tb.termScore(q._1)
         //language model
         val topDocsLang = lm.computeProbabilities(q._1, 0.3)
         
         
         //compute AP for term based model
         var P_term = 0.0
         var P_rels_term = new Array[Double](100)
         var n_rels_term = 0.0
         var i_term = 1
         var iter_term = topDocsTerm.iterator
         while(iter_term.hasNext){
           val d = iter_term.next
           //combine key for relevance map
           val relev = docRelevance.getOrElse((d._1, q._2), false)
           if(relev){
             //true positive
             n_rels_term = n_rels_term+1
             P_term = n_rels_term/i_term
             P_rels_term(i_term-1) = P_term
           }
           else{
             //false positive
             P_term = n_rels_term/i_term
           }
           
           i_term = i_term+1
         }
  
         //add AP for term model to map       
         if(n_rels_term < 1) n_rels_term = 1.0
         AP_term = AP_term.updated(q._2, P_rels_term.sum/n_rels_term) 
         
         
         //compute AP for language model
         var P_lang = 0.0
         var P_rels_lang = new Array[Double](100)
         var n_rels_lang = 0.0
         var i_lang = 1
         var iter_lang = topDocsLang.iterator
         while(iter_lang.hasNext){
           val d = iter_lang.next
           //combine key for relevance map
           val relev = docRelevance.getOrElse((d._1, q._2), false)
           if(relev){
             //true positive
             n_rels_lang = n_rels_lang+1
             P_lang = n_rels_lang/i_lang
             P_rels_lang(i_lang-1) = P_lang
           }
           else{
             //false positive
             P_lang = n_rels_lang/i_lang
           }
           
           i_lang = i_lang+1
         }
         
         //add AP for language model to map       
         if(n_rels_lang < 1) n_rels_lang = 1.0
         AP_lang = AP_lang.updated(q._2, P_rels_lang.sum/n_rels_lang) 
         
         
         println(i_q/queries.length * 100 + "% done")
         i_q = i_q+1
       }
       
       //println(AP)
       
       //compute MAP
       val MAP_term = AP_term.values.sum/queries.length
       val MAP_lang = AP_lang.values.sum/queries.length
       
       println("MAP scores:")
       println("term model:\t" + MAP_term)
       println("language model:\t" + MAP_lang)
       
    }
    else if(args(0) == "-test"){
     println("test mode")
      
     //read in test questions
     println("reading in test questions")
     var fr = new FileReader("resources/test-questions.txt")
     var br = new BufferedReader(fr)
     var queries = List[(String, Int)]()
     var line = br.readLine()
     val patQ = Pattern.compile("<title> Topic: [\\w\\s]+")
     val patN = Pattern.compile("<num> Number: [0-9]+")
     
     var n = 0 //to save current query number
     
     while(line != null){
     val mQ = patQ.matcher(line)
     val mN = patN.matcher(line)
       
       if(mQ.find()){
         val q = mQ.group().substring(14)
         queries = (q,n)::queries
       }
       else if(mN.find()){
         n = mN.group().substring(15).toInt //strip the 0
       }
       line = br.readLine()
     }
     queries = queries.reverse
     
     br.close()
     fr.close()
     
     val ts = System.currentTimeMillis()
     
     val fileTerm = new File("testoutput/outTerm"+ts+".txt")
     val fileLang = new File("testoutput/outLang"+ts+".txt")
     
     if(!fileTerm.createNewFile()){
       throw new Exception("Failed to open file.")
     }
     if(!fileLang.createNewFile()){
       throw new Exception("Failed to open file.")
     }
     
     val fwTerm = new FileWriter("testoutput/outTerm"+ts+".txt")
     val bwTerm = new BufferedWriter(fwTerm)
     
     val fwLang = new FileWriter("testoutput/outLang"+ts+".txt")
     val bwLang = new BufferedWriter(fwLang)
     
     for(q <- queries){
         println("scoring query " + q._2)
         //termbased model
         val topDocsTerm = tb.termScore(q._1)
         //language model
         val topDocsLang = lm.computeProbabilities(q._1, 0.3)
         
         //output best term model results
         var rank=1
         for(dTerm <- topDocsTerm){
           bwTerm.write(q._2 + " " + rank + " " + dTerm._1)
           bwTerm.newLine()
           rank = rank + 1
         }
         
         //output best lang model results
         rank = 1
         for(dLang <- topDocsLang){
           bwLang.write(q._2 + " " + rank + " " + dLang._1)
           bwLang.newLine()
           rank = rank + 1
         }
     
     }
     
     fwTerm.close()
     fwLang.close()
     bwTerm.close()
     bwLang.close()
     
    }
     
  }
}