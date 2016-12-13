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
        
    //SPECIFY DATA LOCATION HERE
    val trainingDataFolder = "resources/documents"
    val trainingQueries = "resources/topic_descriptions.txt"
    val qrels = "resources/qrels.txt"
    val testQueries = "resources/test-questions.txt"
    //has to be created before hand
    val testResultsoutputFolder = "testoutput/"
    
     val maxDocs = 0 //0=inf
     
     println("constructing index")
     val ind = new Index(trainingDataFolder, maxDocs, Map[String, List[String]](), Map[String, Int](), 0)
                     
     //initialize termbased model
     println("scoring...")
     var tb = new TB(ind, Map[String, Double]())
     
     //initialize language model
     var lm = new LanguageModel(ind, Map[String, Double]())
          
     
    if(args(0) == "-training"){
      
     println("training mode")
     
      //read in queries
     println("reading in queries")
     var fr = new FileReader(trainingQueries)
     var br = new BufferedReader(fr)
     var queries = List[(String, Int)]()
     var line = br.readLine()
     val patQ = Pattern.compile("(<title>\\s*Topic:\\s*[^\n]*)")
     val patN = Pattern.compile("<num>\\s*Number:\\s*0[0-9]+")
     val patN2 = Pattern.compile("[0-9]+")
     
     var n = 0 //to save current query number
     
     while(line != null){
     val mQ = patQ.matcher(line)
     val mN = patN.matcher(line)
       
       if(mQ.find()){
         val q = mQ.group().substring(16)
         queries = (q,n)::queries
       }
       else if(mN.find()){
         var s = mN.group()
         val mN2 = patN2.matcher(s)
         while(mN2.find()){
            n = Integer.parseInt(mN2.group())
         }
       }
       line = br.readLine()
     }
     queries = queries.reverse
     
     br.close()
     fr.close()      
      
      //read in qrels
       println("reading in qrels")
       val fr2 = new FileReader(qrels)
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
              
       val res = Evaluation.evaluateAll(queries, docRelevance, tb, lm)
       
       //measurements for term based model
       //bad P scores
       println("EVALUATION TERM MODEL")
       println("queries with bad P: " + res.term.P.toList.filter{case (q, p) => p == 0.0}.map(_._1))     
       println("average P: " + res.term.P.toList.map(_._2).sum / queries.length)
       println("average R: " + res.term.R.toList.map(_._2).sum / queries.length)
       println("average F1: " + res.term.F1.toList.map(_._2).sum / queries.length)
       println("MAP: " + res.term.MAP)
       println("====================================")
       println("EVALUATION LANG MODEL")
       println("queries with bad P: " + res.lang.P.toList.filter{case (q, p) => p == 0.0}.map(_._1))     
       println("average P: " + res.lang.P.toList.map(_._2).sum / queries.length)
       println("average R: " + res.lang.R.toList.map(_._2).sum / queries.length)
       println("average F1: " + res.lang.F1.toList.map(_._2).sum / queries.length)
       println("MAP: " + res.lang.MAP)
       
    }
    else if(args(0) == "-test"){
     println("test mode")
      
     //read in test questions
     println("reading in test questions")
     var fr = new FileReader(testQueries)
     var br = new BufferedReader(fr)
     var queries = List[(String, Int)]()
     var line = br.readLine()
     val patQ = Pattern.compile("(<title>\\s*Topic:\\s*[^\n]*)")
     val patN = Pattern.compile("<num>\\s*Number:\\s*0[0-9]+")
     val patN2 = Pattern.compile("[0-9]+")
     
     var n = 0 //to save current query number
     
     while(line != null){
     val mQ = patQ.matcher(line)
     val mN = patN.matcher(line)
       
       if(mQ.find()){
         val q = mQ.group().substring(14)
         queries = (q,n)::queries
       }
       else if(mN.find()){
         var s = mN.group()
         val mN2 = patN2.matcher(s)
         while(mN2.find()){
            n = Integer.parseInt(mN2.group())
         }
       }
       line = br.readLine()
     }
     queries = queries.reverse
     
     br.close()
     fr.close()
     
     val ts = System.currentTimeMillis()
     
     val fileTerm = new File(testResultsoutputFolder+"outTerm"+ts+".txt")
     val fileLang = new File(testResultsoutputFolder+"outLang"+ts+".txt")
     
     if(!fileTerm.createNewFile()){
       throw new Exception("Failed to open file.")
     }
     if(!fileLang.createNewFile()){
       throw new Exception("Failed to open file.")
     }
     
     val fwTerm = new FileWriter(testResultsoutputFolder+"outTerm"+ts+".txt")
     var bwTerm = new BufferedWriter(fwTerm)
     
     val fwLang = new FileWriter(testResultsoutputFolder+"outLang"+ts+".txt")
     var bwLang = new BufferedWriter(fwLang)
    
     
     for(q <- queries){
         println("scoring query " + q._2)
         //termbased model
         val topDocsTerm = tb.termScore(q._1)
         //language model
         val topDocsLang = lm.computeProbabilities(q._1)
         
         //output best term model results
         var rank=1
         for(dTerm <- topDocsTerm){
           bwTerm.write(q._2 + " " + rank + " " + dTerm._1)
           bwTerm.newLine()
           rank = rank + 1
         }
         
         bwTerm.flush()
         
         //output best lang model results
         rank = 1
         for(dLang <- topDocsLang){
           bwLang.append(q._2 + " " + rank + " " + dLang._1)
           bwLang.newLine()
           rank = rank + 1
         }
         
         bwLang.flush()
     
     }
     
     fwTerm.close()
     fwLang.close()
     bwTerm.close()
     bwLang.close()
     
    }
     
  }
}