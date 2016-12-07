package ch.ethz.ir.project2

import myTinyIR._

object Index extends App{
  var si = new SimpleIndex(new TipsterStream("resources/zipped").stream)
  println(si.results("the").mkString("[", " ", "]"))
}