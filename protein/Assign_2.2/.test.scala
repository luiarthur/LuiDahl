import Rho._
import scala.collection.immutable.Vector.empty

object test{
  def main(args:Array[String]){
    val N = 10000; val in = 8
/*
    for ( i <- 1 to (N/200) ) print("#") 
    println()
    def res (i:Int):String={
      if (i % 200 == 0) {print(">")}
      Rho.draw(ko=in) 
    }
    val M = Vector.tabulate(N)(r => res(r))
*/
    val M = Rho.draw(N,ko=in)
    val F = M.distinct.sorted
    println();println()
    
    var sum = 0.0
    F.foreach( f => sum = sum+Rho.prob(f,ko=in) )
    F.foreach( f =>  println("\t"+f+"\t"+M.count(m => m == f).toDouble/M.size+"\t"+
               Rho.prob(f)/sum) )
    println()
  }


}



// To Compile: 
//    First, Compile Rho.scala:
//      scalac -d classes Rho.scala
//    Then, Compile testRho.scala:
//      scalac -d classes -cp classes testRho.scala
// Then, run testRho
//    scala test
