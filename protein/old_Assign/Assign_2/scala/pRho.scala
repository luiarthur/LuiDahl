//This will use strings instead of Vectors for rho
import scala.collection.immutable.Vector.empty
import scala.io.Source
import java.io.File
import math._
import util.Random
import Gamma.Gamma._

object pRho{
  val ko = 8; val d = 4;
  val k = ko/.62
  val theta = Array(.170207, .224021, .178348, .427424)
  def countEta (r:String ,s:Char):Int={
    var cnt = 0; if (r.charAt(0)==s) {cnt = 1}
    for (i <- 1 to (r.length-1)){
      if ( r.charAt(i-1) != r.charAt(i) ) {
        if (r.charAt(i)==s){ cnt+=1 }
      }
    } 
    cnt
  } 
  def inEta (rv:String):Array[Int]={
    val mv = Array(countEta(rv,'H'),countEta(rv,'E'),
                   countEta(rv,'T'),countEta(rv,'C')) 
    mv
  }
  def getEL (rv:String):Vector[(Char,Int)]={
    val rv2 = rv:+'A'
    val l = rv2.length; var i = 0
    var el = Vector.fill(0)('A',0)
    while (i < (l-1)){
      var j = 0
      while ( (rv2(i+j)==rv2(i+j+1)) & (i+j+2 < l) ){ j += 1 }
      el = el :+ (rv2(i),j+1)
      while ( (rv2.charAt(i+j)==rv2.charAt(i+j+1)) & (i+j+2 < l) ){ j += 1 }
      el = el :+ (rv2.charAt(i),j+1)
      i += j+1
    }
    el
  }
  def logNegBinom(x:Double, mu:Double, size:Double):Double={
    val p = size/(size+mu) 
    val n = size
    logGamma(x+n)-logGamma(n)-logGamma(x+1)+n*log(p)+x*log(1-p)
  }
  def sumLogLamGivenEta (EL:Vector[(Char,Int)]):Double={
    def logLamGivenEta (el: (Char,Int)):Double={
      if ((el._1=='H')&(el._2>=5)){logNegBinom(el._2-5,mu=1.885880,size=6.953392)}
      else if ((el._1=='E')&(el._2>=3)){logNegBinom(el._2-3,mu=2.521091,size=2.899121)}
      else if ((el._1=='T')&(el._2>=3)){logNegBinom(el._2-3,mu=0.839557,size=0.728294)}
      else if ((el._1=='C')&(el._2>=1)){logNegBinom(el._2-1,mu=0.990796,size=3.725501)}
      else {0}
    }
    var sumLog = 0.0
    for (i <- 0 to EL.size-1){
      sumLog = sumLog+logLamGivenEta(EL(i))
    }
    sumLog
  } 
  def logNorm(x:Double,mu:Double,sig:Double):Double={
    -.5*(log(2*Pi*sig*sig)+pow((x-mu)/sig,2))
  } 
  def logMult(mv:Array[Int],prob:Array[Double]):Double={
    var sum = 0.0
    for (i <- 0 to prob.size-1){
      sum = sum - logGamma(mv(i)+1) + mv(i)*log(prob(i))
    }
    sum+logGamma(1+mv.sum)
  }

  def main(args: Array[String]){
    val rho = "CEEETTTC" 
    val m = inEta(rho).sum
    val mv = inEta(rho).dropRight(1):+(inEta(rho)(3)-2)
    val EL = getEL(rho)
    val ans = exp(logNorm(m,2.347253+.154154*k, 5.526382/d)+logMult(mv,theta)+sumLogLamGivenEta(EL))
    println(ans)
  }

}
//To Compile: mkdir classes
//To Compile: scalac -d classes Gamma.scala
//To Compile:  scalac -d classes pRho.scala 
//To Execute:  scala -cp classes pRho 
