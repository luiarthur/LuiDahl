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

  def logLamGivenEta (eta: Char):Int= {
    if (eta=="H"){5+rnbinom(1.885880,6.953392)}
    else if (eta=="E"){3+rnbinom(2.521091,2.899121)}
    else if (eta=="T"){3+rnbinom(0.839557,0.728294)}
    else {1+rnbinom(0.990796,3.725501)}
  }
  val theta = Array(.170207, .224021, .178348, .427424)
  def logf (x):Double= {
    val mu = 2.347253+.154154*k
    val sig = 5.526382/d
    5*log(2*Pi*sig*sig)-pow((x-mu)/sig,2)/2
  }
  def getRho (ETA: Vector[Char], LAM: Vector[Int]):Vector[Char]= {
    var rv = Vector.fill(0)("a"); var k=0
    for (i <- 0 to Lam.size-1){
      for (j <- 1 to (Lam[i])){
        rv[k] = E[i]
        k += 1
      }
    } 
  }
  def sampleStr (vc: Vector[Char], probVec: Vector[Double]):Char= {
    var prob = probVec
    val pSum = prob.sum
    prob = prob updated (0, prob(0)/pSum) 
    if (pSum != 1) {
      for (i <- 1 to prob.size-1){
        prob = prob updated (i, prob(i-1)+prob(i)/pSum)
      }
    } 
    val r = new Random().nextDouble
    for (i <- 1 to (prob.size-1) ){
      if ( (prob(i-1) <= r) && (r <= prob(i)) ){
        vc(i) 
      } 
    } 
  }
  def SampleInt (vc: Vector[Char], probVec: Vector[Double]):Char= {
    var prob = probVec
    val pSum = prob.sum
    prob = prob updated (0, prob(0)/pSum)
    if (pSum != 1) {
      for (i <- 1 to prob.size-1){
        prob = prob updated (i, prob(i-1)+prob(i)/pSum)
      }
    } 
    val r = new Random().nextDouble
    for (i <- 1 to (prob.size-1) ){
      if ( (prob(i-1) <= r) && (r <= prob(i)) ){
        vc(i)
      }
    }
  }   
  def rmultinom(m: Int, Vector[Double]):Vector[Int]{
    
  } 
}
