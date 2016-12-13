package ch.ethz.ir.project2

case class SingleEvaluationResult(P: Map[Int, Double], R: Map[Int, Double], 
    F1: Map[Int, Double], MAP: Double)
    
case class FullEvaluationResult(term: SingleEvaluationResult, lang: SingleEvaluationResult)


object Evaluation{
  
  def evaluateAll(queries: List[(String, Int)], docRelevance: Map[(String,Int), Boolean], tb: TB, lm: LanguageModel) : FullEvaluationResult = {
    val resTerm = evaluateSingleModel(queries, docRelevance, tb, "term")
    val resLang = evaluateSingleModel(queries, docRelevance, lm, "lang")
  
    new FullEvaluationResult(resTerm, resLang)
  }
  
  //allow to pass both models via casting
  private def evaluateSingleModel(queries: List[(String, Int)], docRelevance: Map[(String,Int), Boolean], model: Object, modelType: String) : SingleEvaluationResult = {
                 
      //evaluation measures for term based model: P, R, F1
      var relDocs = 0
      var P = 0.0
      var P_map = Map[Int, Double]()
      var R = 0.0
      var R_map = Map[Int, Double]()
      var F1_map = Map[Int, Double]()
     
      var AP = Map[Int, Double]()
       
       for(q <- queries){
         println("scoring query " + q._2)
         
          
         var P_rels = new Array[Double](100)
         var n_rels = 0.0
         
         //cast to used model
         var topDocs = List[(String, Double)]()
         if(modelType == "term"){
           topDocs = model.asInstanceOf[TB].termScore(q._1)
         }
         else if(modelType == "lang"){
           topDocs = model.asInstanceOf[LanguageModel].computeProbabilities(q._1)
         }

         var i = 1
         var iter = topDocs.iterator
         while(iter.hasNext){
           val d = iter.next
           
           //relevant docs for this query according to qrel
           relDocs = docRelevance.toList.filter{case ((d_id, qNum), r) => qNum==q._2 && r}.length
           
           //combine key for relevance map
           val relev = docRelevance.getOrElse((d._1, q._2), false)
           if(relev){
             //true positive
             n_rels = n_rels+1
             P = n_rels/i
             P_rels(i-1) = P
           }
           else{
             //false positive
             P = n_rels/i
           }
           
           i = i+1
         }
  
         //add AP for term model to map       
         if(n_rels < 1) n_rels = 1.0
         AP = AP.updated(q._2, P_rels.sum/n_rels)
         P_map = P_map.updated(q._2, P)
         
         R = n_rels/relDocs
         R_map = R_map.updated(q._2, R)
         F1_map = F1_map.updated(q._2, 2*(P*R)/(P+R))
                  
       }
       
        val MAP = AP.values.sum/queries.length 
      
        new SingleEvaluationResult(P_map, R_map, F1_map, MAP)
  
  }
  
}