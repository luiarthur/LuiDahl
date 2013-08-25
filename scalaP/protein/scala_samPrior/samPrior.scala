import scala.collection.immutable.Vector.empty
import Gamma.Gamma._
import scala.io.Source
import java.io.File
import scala.math._
import util.Random

object rRho{
  // SET the value of k ////
  val k = 8/.62; val d = 4//
  //////////////////////////

  def logLamGivenEta (eta: Char){
    if (eta=="H"){5+rnbinom(1.885880,6.953392)}
    else if (eta="E"){3+rbinom(2.521091,2.899121)}
    else if (eta="T"){3+rbinom(0.839557,0.728294)}
    else {1+rbinom(0.990796,3.725501)}
  }
  val theta = Array(.170207, .224021, .178348, .427424)
  def logf (x){
    val mu = 2.347253+.154154*k
    val sig = 5.526382/d
    .5*log(2*Pi*sig*sig)-pow((x-mu)/sig,2)/2
  }
  def getRho (ETA: Vector[Char], LAM: Vector[Int]){
    var rv = Vector.fill(0)("a"); var k=0
    for (i <- 0 to Lam.size-1){
      for (j <- 1 to (Lam[i])){
        rv[k] = E[i]
        k += 1
      }
    } 
  }
  def sample 
}
