// USING the non normalized rnorm: Just drawing from continuous normal
// IMPORTANT NOTE ABOUT NegativeBinomial from breeze:
//   The parameters r & p refer to the number of FAILURES before stopping & 
//   the probability of SUCCESS respectively.
//   THINK CLEARLY before implementing.
// PROBLEM TO FIX: 
//   It appears that in my samples of rho length = 8, 
//   sequences like CEEETTTC and CTTTEEEC appear less frequently than they
//   ought to. That leads me to think there is a problem with my
//   draw function.
// Creating too many objects. Especially when randomly draw from distributions.
// Condsider making  only one object to draw from.

package Rho

import scala.collection.immutable.Vector.empty
import org.apache.commons.math3.special.Gamma._
import breeze.stats.distributions.{NegativeBinomial,Binomial,Gaussian}
import scala.io.Source
import java.io.File
import scala.math.{log,pow,Pi,exp}
import util.Random

class Rho{
//rRho//////////////////////////////////////////////////

  // make my RANDOMS all at the beginning. Run loops in here. 
  // Don't create objects every time! Takes much more time.

  val theta = Vector(.170207, .224021, .178348, .427424)
  
    
  val ran = new Random() //JUST ADDED
  def rMult(n: Int, v: Vector[Double]):Vector[Int]={
    var N = n; var x = Vector.fill(0)(0)
    val p = Vector.tabulate(v.size)(i => v(i)/(1-v.dropRight(v.size-i).sum))
    for (i <- 0 to v.size-1){
      if ( N <= 0 ){x = x :+ 0}
      else {x = x :+ (new Binomial(N,p(i)).draw) }
      N = N - x(i)
    } 
    x
  }
  def rLamGivenEta (eta: Char):Int= {
    def rnbinom(mean: Double, size: Double):Int={
      new NegativeBinomial(size,mean/(size+mean)).draw
    }
    if (eta=='H'){5+rnbinom(1.885880,6.953392)}
    else if (eta=='E'){3+rnbinom(2.521091,2.899121)}
    else if (eta=='T'){3+rnbinom(0.839557,0.728294)}
    else {1+rnbinom(0.990796,3.725501)}
  }
  def getRho (EL: Vector[(Char,Int)]):String= {
    var rv = ""
    for (i <- 0 to EL.size-1){
      for (j <- 1 to EL(i)._2){
        rv = rv :+ EL(i)._1
      }
    } 
    rv
  } 
  def mSamp (ko:Int):Int={
    val k = ko/.62; val d = 4
    //val k = ko; val d =4
    //val r = new Random().nextDouble*
    def f(x:Double):Double={
      new Gaussian(2.347253+.154154*k, 5.526382/d).pdf(x) 
    }
    // -3 because must have at least 3 blocks. 
    // -2 because must have < k-2 blocks.

    var vf= Vector.tabulate(ko-3+(8-ko-2))(i => f(i+3) )
    for (i <- 1 to (vf.size-1) ){
      vf = vf updated ( i, (vf(i-1)+vf(i))/vf.sum )
    }
    vf = vf:+1.0
    var i = 0
    while (ran.nextDouble > vf(i)) i += 1
    i+3
  }  
  //////////////////////////////////////////////////////////
  def draw(N: Int, ko:Int=8, d:Int=4):Array[String]={
    var arRho = new Array[String] (N)
    for ( i <- 1 to (N/200) ) print("#"); println()

    for ( n <- 0 to (N-1) ){
	    var rho = ""
	    var valid = false
	    var etas = ""
	    while (!valid){
	      valid = true
	      rho = ""; etas = ""
	      //val m = (new Random().nextGaussian*(5.526382/d) + 2.347253+.154154*k).toInt
	      val m = mSamp(ko)
	      val mv = rMult(m-2,theta)
	      val tempEL = Vector( ('H',mv(0)), ('E',mv(1)), ('T',mv(2)), ('C',mv(3)) )
	      val tempRho = getRho(tempEL)
	      etas = "C".concat(Random.shuffle(tempRho.toSeq).mkString :+ 'C')

       // NOT SURE IF THIS CHUNK IS USEFUL
        for (i <- 1 to (etas.length-2) )
        {
          if (etas(i)==etas(i-1))
          {
            for (j <- i to (etas.length-2) )
            {
              if ( (etas.charAt(j)!=etas(i-1)) & (etas(j)!=etas(i+1)) &
                   (etas.charAt(i)!=etas(j-1)) & (etas(i)!=etas(j+1)) )
              {
                val t = etas.charAt(j) 
                etas = etas updated (j, etas(i)) 
                etas = etas updated (i, t)
              } 
            } 
          } 
        }// END OF THIS CHUNK

        for (i <- 1 to (etas.length-1)){
          if (etas(i-1)==etas(i)){valid=false}
        }
        val EL = Vector.tabulate(etas.length)( i => (etas(i),rLamGivenEta(etas(i))) )
        rho = getRho(EL)
        if (rho.length != ko){valid=false}
      }
      arRho.update(n,rho)
      //if ( n % 200 == 0 ) print(">")
    }
    //println()
    arRho
  }
//pRho////////////////////////////////////////////////////////////////
  // checked countEta
  def countEta (r:String ,s:Char):Int={
    var cnt = 0; if (r(0)==s) {cnt = 1}
    for (i <- 1 to (r.length-1)){
      if ( r(i-1) != r(i) ) {
        if (r(i)==s){ cnt+=1 }
      }
    } 
    cnt
  } 

  // checked inEta
  def inEta (rv:String):Vector[Int]={
    val mv = Vector(countEta(rv,'H'),countEta(rv,'E'),
                    countEta(rv,'T'),countEta(rv,'C')) 
    mv
  }

  // checked getEL
  def getEL (rv:String):Vector[(Char,Int)]={
    val rv2 = rv:+'A'
    val l = rv2.length; var i = 0
    var el = Vector.fill(0)('A',0)
    while (i < (l-1)){
      var j = 0
      while ( (rv2(i+j)==rv2(i+j+1)) & (i+j+2 < l) ){ j += 1 }
      el = el :+ (rv2(i),j+1)
      i += j+1
    }
    el
  }
  def logNegBinom(x:Double, mean:Double, size:Double):Double={
    val p = mean/(size+mean) 
    val r = size 
    new NegativeBinomial(r,p).logProbabilityOf(x.toInt) 
  }
  def sumLogLamGivenEta (EL:Vector[(Char,Int)]):Double={
    def logLamGivenEta (el: (Char,Int)):Double={
      if ((el._1=='H')&(el._2>=5)){logNegBinom(el._2-5,mean=1.885880,size=6.953392)}
      else if ((el._1=='E')&(el._2>=3)){logNegBinom(el._2-3,mean=2.521091,size=2.899121)}
      else if ((el._1=='T')&(el._2>=3)){logNegBinom(el._2-3,mean=0.839557,size=0.728294)}
      else if ((el._1=='C')&(el._2>=1)){logNegBinom(el._2-1,mean=0.990796,size=3.725501)}
      else {0}
    }
    var sumLog = 0.0; var bad = false
    //EL.foreach( el => (sumLog = sumLog+logLamGivenEta(el)))
    for ( i <- 0 to (EL.size-1) ){
      if (logLamGivenEta(EL(i))==0) bad = true
      sumLog = sumLog + logLamGivenEta(EL(i)) 
    }
    if (bad) 0 else sumLog
  } 
  def logNorm(x:Double,mu:Double,sig:Double):Double={
    -.5*(log(2*Pi*sig*sig)+pow((x-mu)/sig,2))
  } 

  // logMult is correct
  def logMult(x:Vector[Int],prob:Vector[Double]):Double={
    var sum = 0.0
    for ( i <- 0 to x.size-1) (sum = sum - logGamma(x(i)+1) + x(i)*log(prob(i)))
    sum+logGamma(1+x.sum)
  }

  def prob(rho:String,ko:Int=8,d:Int=4):Double={
    val k = ko/.62
    val m = inEta(rho).sum
    val mv = inEta(rho).dropRight(1):+(inEta(rho)(3)-2)
    val EL = getEL(rho)
    val ans = exp(logNorm(m,2.347253+.154154*k, 5.526382/d)+logMult(mv,theta)+sumLogLamGivenEta(EL)) 
    if (sumLogLamGivenEta(EL) == 0) 0 else ans
  }
}
object Rho extends Rho

//To Compile rRho:  scalac -d classes Rho.scala
//To run rRho:      scala -cp classes Rho
//TO Compile ALL:   scalac -d classes -cp classes *.scala
//To run test:      scala -cp classes test
