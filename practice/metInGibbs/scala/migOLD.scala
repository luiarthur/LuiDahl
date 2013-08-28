import scala.collection.immutable.Vector.empty
import scala.io.Source
import java.io.File
import scala.math._
import util.Random
import Gamma.Gamma._

object mig{

  def writeToFile(file: String, M: Vector[ Vector[Double] ]) {
    val pw = new java.io.PrintWriter(new File(file))
    val N = M.size
    try { 
      var i = 0    
      while (i < N ){
        pw.write( M(i).mkString(" ") + s"\n")
        i = i + 1
      }
    } 
    finally {
      pw.close()
    }
  }
  def rnorm(mu: Double, sig: Double): Double={
    var r = new Random().nextGaussian()
    r*sig+mu
  }
  def la(x: Vector[Double], a: Double, b: Double): Double = {
    val n = x.size
    var slx = 0.0
    for (i <- 0 to n-1){ slx += log( x(i) ) }
    4 * log(a) - a/20 + n * (logGamma(a+b)-logGamma(a)) + (a-1) * slx
  }
  def lb(x: Vector[Double], a: Double, b: Double): Double = {
    val n = x.size
    var slx = 0.0
    for (i <- 0 to n-1){  slx = slx + log( 1-x(i) )  }
    7 * log(b) - b/30 + n * (logGamma(a+b)-logGamma(b)) + (b-1) * slx
  }
  
  def main(args: Array[String]){

    val x = Vector.fill(1)(.81) :+.83 :+.79 :+.75 :+.8
    val csa = 50; val csb = 20; val N:Double = 1000000
    var cnta = 0; var cntb = 0; var i = 1; var r = 0.0
    var canda = 0.1; var candb = 0.1; var ran = new Random()

    var m = Vector.fill(1)(100.0); m = m:+ 240.0
    var M = Vector.fill(1)(m); M = M :+ m;

    while (i < N){

      canda = rnorm( (M(i)(0)), csa)
      if (canda > 0) {
        r = la(x,canda,M(i)(1)) - la(x,M(i)(0),M(i)(1));
        if (r > log( ran.nextDouble ) ){
          M = M updated (i, ( M(i) updated(0, canda ) ) )
          cnta += 1
        }
      } 

      candb = rnorm(M(i)(1), csb)
      if (candb > 0){
        r = lb(x,M(i)(0),candb)-lb(x,M(i)(0),M(i)(1))
        if (r > log( ran.nextDouble ) ){
          M = M updated (i, ( M(i) updated(1, candb ) ) )
          cntb += 1
        }
      }

      M = M:+M(i)
      i += 1
    }

    println(cnta/N+" "+cntb/N)
    M = M.drop(5000);
    writeToFile("sout.txt", M)
  }
}

//To Compile: mkdir classes
//To Compile: scalac -d classes Gamma.scala
//To Compile:  scalac -d classes mig.scala
//To Execute:  scala -cp classes mig

