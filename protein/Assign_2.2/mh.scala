import Rho._ 
import util.Random
import java.io.File
import scala.io.Source

object mh { 

  val ran = new Random()

  def outData(m: Array[String], dest:String):Int={
    val pw = new java.io.PrintWriter(new File(dest))
    val N = m.size
    try { 
      var i = 0    
      while (i < N ){
        pw.write( m(i) + s"\n")
        i = i + 1
      }
    } 
    finally {
      pw.close()
    }
    0
  }

  // Questions:
  //   How do I write the probabilty for merge and split?
  //   How do I write merge function?

  // NEED TO ADD Rho. before each function of Rho
  class Proposal {

    def draw(a:String,t:Double):String={
      val m = inEta(a).sum
      var g = ran.nextInt(m-2) + 1
      var blks =Array('H','E','T','C')

      def switch():String= { // switch function
        for (i <- 0 to (blks.length-1)){
          if ( (a(g-1)==blks(i)) || (a(g)==blks(i)) || (a(g+1)==blks(i)) ){
            blks.update(i,'Z')
          }
        }
        blks = blks.filter (s => s!='Z')
        val r = ran.nextInt(blks.size) 
        val newBlk = (blks(r),getEL(a)(g)._2)
        (getRho(getEL(a) updated (g, newBlk)), 1, 1)
      }

      def changeBound():String= { // change boundary position function
        val z = getEL(a)(g-1)._2+getEL(a)(g)._2-1
        val p = ran.nextInt(z)+1
        (getRho ((getEL(a) updated (g-1, (getEL(a)(g)._1,p) ) ) updated
                (m, (getEL(a)(g)._1,z-p) )), 1, 1 )
      }

      def split(from:String, to:String):String= { // split function
        val z = getEL(a)(g)._2 - 1
        val p = ran.nextInt(z) + 1
        for (i <- 0 to (blks.length-1)){
          if ( (a(g-1)==blks(i)) || (a(g)==blks(i)) ){ blks.update(i,'Z') }
        }
        blks = blks.filter (s => s!='Z')
        val r = ran.nextInt(blks.size)
        var tempEL = getEL(a) updated (g, (getEL(a)(g)._1,z)) 
        tempEL = (tempEL.dropRight(m-g) :+ (blks(r),z-p) ) ++ tempEL.drop(g)        
        val b = getRho(tempEL)
        (b,.25/(m-2)/(inEta(a)._g-1)/(blks.length), merge(from=b, to=a)._2)
      }

      def merge(from:String, to:String):String={ // merge function is very hard.
        var tempEL = getEL (a)
        // m-3 & + 2 because I don't want to merge into the 0th element (C)
	      while (tempEL(g-1)._1==tempEL(g+1)._1){g = ran.nextInt(m-3) + 2} 
        tempEL = tempEL updated (g+1, (tempEL(g+1)._1,
                                 tempEL(g)._2+tempEL(g+1)._2))
        tempEL = tempEL.splitAt(g)._1 ++ tempEL.splitAt(g).drop(1)
        val b = getRho(tempEL)
        (b, .5*.25/(m-3), split(b,a)._2)
      }
      
      if (t <= .25) { switch()}
      else if (t <= .5) { changeBound() }
      else if (t <= .75) { split( from =_, to =_) }
      else { merge( from =_, to =_) }

    }
  }

  def main(args:Array[String]){
    
    //  INITIALIZE:
    val N = 15000; val koIn = 8; val dIn = 4; var cnt = 0
    var M = new Array[String](N)
    val myFile = "~/Documents/LuiDahl/protein/Assign2.2/mhResults.txt"
    M.update(0,Rho.draw(ko=koIn,d=dIn))
    //  END INITIALIZE

    for (i <- 1 to (N-1)){
      M.update(i, M(i-1) )
      val t = ran.nextDouble
      val cpp = Proposal.draw(M(i),t)
      cand = cpp._1; cand2Curr = cpp._2; curr2Cand = cpp._3
      val r = Rho.prob(cand,koIn,dIn) / Rho.prob(M(i),koIn,dIn) * 
              cand2Curr / curr2Cand
      if (r > ran.nextDouble){
        M(i) = cand
        cnt += 1
      }
    } 
    println(cnt)
    outData(M.drop(5000),myFile)
  }
}
