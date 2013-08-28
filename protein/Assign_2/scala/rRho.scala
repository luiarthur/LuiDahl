// USING the non normalized rnorm: Just drawing from continuous normal
import scala.collection.immutable.Vector.empty
import Gamma.Gamma._
import scala.io.Source
import java.io.File
import scala.math._
import util.Random

object rRho{
  // SET the value of k //////////////////////
  val ko = 8; val d = 4; val k: Int = ko/.62//
  ////////////////////////////////////////////
  val theta = Vector(.170207, .224021, .178348, .427424)

  def rnbinom(mu:Double, size:Double):Int={
  }
  def rLamGivenEta (eta: Char):Int= {
    if (eta=="H"){5+rnbinom(1.885880,6.953392)}
    else if (eta=="E"){3+rnbinom(2.521091,2.899121)}
    else if (eta=="T"){3+rnbinom(0.839557,0.728294)}
    else {1+rnbinom(0.990796,3.725501)}
  }
  def getRho (EL: Vector[(Char,Int)]):String= {
    var rv = Vector.fill(0)("a") 
    for (i <- 0 to EL.size-1){
      for (j <- 1 to EL(i)._2){
        rv = rv :+ E[i]
      }
    } 
    rv
  } 
  def rmultinom(n: Int, Vector[Double]):Vector[Int]{
    
  } 
  def main(args: Array[String]){
    val m = new Random().nextGaussian*(5.526382/d) + 2.347253+.154154*k
    val mv = rMultinom(m-2,theta)
    var valid = false
    while (!valid){
      valid = true
      val tempEL = Vector( ('H',mv(0)), ('E',mv(1)), ('T',mv(2)), ('C',mv(3)) )
      val tempRho = getRho(tempEL)
      val eta = "C" :+ Random.shuffle(tempRho.toSeq).mkstring :+ "C"
      for (i <- 1 to (eta.length-2) ){
        if (eta.charAt(i)==eta.chatAt(i-1)){
          for (j <- 1 to (eta.length-2) ){
            if ( (eta.charAt
          } 
        } 
      }
    }
  }
}
