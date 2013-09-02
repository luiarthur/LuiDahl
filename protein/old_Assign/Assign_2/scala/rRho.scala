// USING the non normalized rnorm: Just drawing from continuous normal
// import breeze.stats.distributions._
//import Gamma.Gamma._  //allows me to use gamma & logGamma
import scala.collection.immutable.Vector.empty
import org.apache.commons.math3.special.Gamma._
import breeze.stats.distributions._
import scala.io.Source
import java.io.File
import scala.math._
import util.Random

object rRho{
  // SET the value of k ////////////////////////////
  val ko = 8.0; val d = 4.0; val k = round(ko/.62)//
  //////////////////////////////////////////////////
  val theta = Vector(.170207, .224021, .178348, .427424)
/*
  def rnbinom(mean:Double, size:Double):Int={
    def f(x: Int):Double={
      val p = size/(size+mean); val n = size
      logGamma(x+n)-logGamma(n)-logGamma(x+1)+n*log(p)+x*log(1-p) 
    }
    var i = 0; val r = log(new Random().nextDouble)
    while (!((f(i) <= r) & (r < f(i+1)))){i += 1}
    i
  }
*/
  def rnbinom(mean: Double, size: Double):Int={
    val p = size/(mean+size) 
    val x = new NegativeBinomial(size,p).draw
    x
  }
  def rmultinom(n: Int, v: Vector[Double]):Vector[Int]={
/*
    def rbinom(n: Int, p:Double):Int={
      def logfact(x:Int):Double={
        if (x==0) {0}
        else {log(x) + logfact(x-1)}
      }
      def f(x:Int,n:Int,p:Double):Double={
         exp(logfact(n)-(logfact(x)+logfact(n-x))) * pow(p,x) * pow(1-p,n-x)
      }
      var i = 0; val r = new Random().nextDouble
      while (!((f(i,n,p) <= r) & (r < f(i+1,n,p)))){i += 1} 
      i
    }  
*/
    
    var N = n; var x = Vector.fill(0)(0)
    val p = Vector.tabulate(v.size)(i => v(i)/(1-v.dropRight(v.size-i).sum))
    for (i <- 0 to v.size-1){
      //x = x :+ rbinom(N,p(i))
      x = x :+ (new Binomial(N,p(i)).draw)
      N = N - x(i)
    } 
    x
  }
  def rLamGivenEta (eta: Char):Int= {
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
   
  def main(args: Array[String]){
    var rho = ""
    var valid = false
    var ii = 0
    var etas = ""
    while (!valid){
      valid = true
      rho = ""
      val m = (new Random().nextGaussian*(5.526382/d) + 2.347253+.154154*k).toInt
println("This is the value of m: "+m)
      //val mv = rmultinom(m-2,theta)
      val mv = Vector(0,1,1,0)  

      val tempEL = Vector( ('H',mv(0)), ('E',mv(1)), ('T',mv(2)), ('C',mv(3)) )
println(tempEL.toString)
      val tempRho = getRho(tempEL)
println(tempRho)
      etas = "C".concat(Random.shuffle(tempRho.toSeq).mkString :+ 'C')
println(etas) 
      for (i <- 1 to (etas.length-2) ){
        if (etas.charAt(i)==etas.charAt(i-1)){
          for (j <- i to (etas.length-2) ){
            if ( (etas.charAt(j)!=etas.charAt(i-1)) & (etas.charAt(j)!=etas.charAt(i+1)) &
                 (etas.charAt(i)!=etas.charAt(j-1)) & (etas.charAt(i)!=etas.charAt(j+1)) ){
              val t = etas.charAt(j); 
              etas = etas updated (j, etas.charAt(i)) 
              etas = etas updated (i, t)
println(etas)
            } 
          } 
        } 
      }
      for (i <- 1 to (etas.length-1)){
        if (etas.charAt(i-1)==etas.charAt(i)){valid=false}
      }
println("here1 "+etas)
      val EL = Vector.tabulate(etas.length)( i => (etas.charAt(i),rLamGivenEta(etas.charAt(i))) )
//val EL = Vector( ('C',1),('E',3),('T',3),('C',1) )
println("This is EL: "+EL)
      rho = getRho(EL)
println("This is rho: "+rho)
      if (rho.length != ko){valid=false}
    }
    println(rho)
  }
}


//To Compile Gamma: scalac -d classes Gamma.scala
//But make sure the directory classes exists first!
//To Compile rRho:  scalac -cp classes -d classes rRho.scala
//To run rRho:      scla -cp classes rRho
