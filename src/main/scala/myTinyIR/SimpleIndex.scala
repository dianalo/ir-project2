package myTinyIR

case class SimpleResult(val id: String) extends AnyVal with Result[SimpleResult] {
  def matches(that: SimpleResult) = this.id compare that.id
  def matched(that: SimpleResult) = that
} 

class SimpleIndex (docs: Stream[Document]) 
extends InvertedIndex[SimpleResult] {
  
  case class SimplePosting(val id: String) extends Ordered[SimplePosting] {
    def compare(that: SimplePosting) = this.id compare that.id
    
  }
  type PostList = List[SimplePosting]     
  val index : Map[String,PostList] = {
    val groupedTuples = postings(docs).groupBy(_.term)
    val sortedPL = groupedTuples.mapValues(_.map(p => SimplePosting(p.doc)).distinct.sorted)
    sortedPL.map{case (k, l) => (k, putToDB(k, l))}
  }
  
  def putToDB(term: String, l: PostList) :  PostList = {
    if(l.length < 50){
      DB.put(term.toArray.map(_.toByte), l.mkString(", ").toArray.map(_.toByte))
      return List(new SimplePosting("_DB_")) //means in DB
    }
    return l
  }
  
  
  
  case class IdTuple(term: String, doc: String) 
  private def postings (s: Stream[Document]): List[IdTuple] = 
    s.flatMap( d => d.tokens.map(token => IdTuple(token,d.ID)) ).toList
   
  def results (term: String) : List[SimpleResult] = {
    //index.getOrElse(term,Nil).map( p => SimpleResult(p.id) )
    if(index.contains(term)){
      val entry = index.get(term).get
      if(entry.head.id == "_DB_"){
          val bytes = term.toArray.map(_.toByte)
          return DB.get(bytes).toString().split(", ").toList.map(k => SimpleResult(k))
      }
      else{
        return entry.map( p => SimpleResult(p.id) )
      }
    }
    else{
      return List()
    }
  }
}
  