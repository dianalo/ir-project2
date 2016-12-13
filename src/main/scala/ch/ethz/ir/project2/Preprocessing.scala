package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer._

object Preprocessing {
  def preprocessDoc(tokens: List[String]) : List[String] = {
    //Stop words (no duplicate filtering!! otherwise termfrequency useless...)
    val noSw = StopWords.filterOutSW(tokens).toList
    
    //short words
    val len = noSw.filter(_.length > 4).toList
    val letterOrDigit = len.map(_.filter{_.isLetterOrDigit})
    //stemming
    //val stem = len.map(w => PorterStemmer.stem(w)).toList
    //only take words appearing more than once
    letterOrDigit.diff(letterOrDigit.groupBy(identity).mapValues(_.length).filter(_._2 <= 1).toList.map(_._1))
  }
  
  def preprocessQuery(tokens: List[String]) : List[String] = {
    //Stop words (no duplicate filtering!! otherwise termfrequency useless...)
    val noSw = StopWords.filterOutSW(tokens).toList
    
    //stemming
    //val stem = noSw.map(w => PorterStemmer.stem(w)).toList
    
    //short words
    val len = noSw.filter(_.length > 4).toList
    val letterOrDigit = len.map(_.filter{_.isLetterOrDigit})
    letterOrDigit
  }
}