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

    def draw(a:String,t:Double):(String, Double, Double)={
      val m = Rho.inEta(a).sum
      var g = ran.nextInt(m-2) + 1
      var blks =Array('H','E','T','C')

      def switch():(String, Double, Double)=
      { // switch function
        for (i <- 0 to (blks.length-1)){
          if ( (a(g-1)==blks(i)) || (a(g)==blks(i)) || (a(g+1)==blks(i)) ){
            blks.update(i,'Z')
          }
        }
        blks = blks.filter (s => s!='Z')
        val r = ran.nextInt(blks.size) 
        val newBlk = (blks(r),Rho.getEL(a)(g)._2)
        (Rho.getRho(Rho.getEL(a) updated (g, newBlk)), 1.0, 1.0)
      }
      

      // NEEDS FIXING. OutOfBound ERROR:
      def changeBound():(String, Double, Double) =
      { // change boundary position function
        val z = Rho.getEL(a)(g-1)._2 + Rho.getEL(a)(g)._2
        val p = ran.nextInt(z-1) + 1
        //println("DID A CHANGEBOUND "+m+" "+g+" "+p+" "+z+" "+Rho.getEL(a))
        (Rho.getRho((Rho.getEL(a) updated (g-1, (Rho.getEL(a)(g-1)._1,p) ) ) updated
                (g, (Rho.getEL(a)(g)._1,z-p) )), 1.0, 1.0 )
      }


      def split(from:String, to:String):(String, Double, Double) = 
      { // split function
        val z = Rho.getEL(a)(g)._2 - 1
        val p = ran.nextInt(z) + 1
        for (i <- 0 to (blks.length-1)){
          if ( (a(g-1)==blks(i)) || (a(g)==blks(i)) ){ blks.update(i,'Z') }
        }
        blks = blks.filter (s => s!='Z')
        val r = ran.nextInt(blks.size)
        var tempEL = Rho.getEL(a) updated (g, (Rho.getEL(a)(g)._1,p)) 
        tempEL = (tempEL.dropRight(m-g) :+ (blks(r),z-p)) ++ tempEL.drop(g)        
        val b = Rho.getRho(tempEL)
        //(b,.25/(m-2)/(Rho.inEta(a)._g-1)/(blks.length), merge(from=b, to=a)._2)
        val c = if (to == "") {merge(from=b, to=a)._2} else {1}
        (b, .25/(m-2)/z/(blks.length), c)
      }

      def merge(from:String, to:String):(String, Double, Double)=
      { // merge function is very hard.
        var tempEL = Rho.getEL(a)
        if (m <= 3) { (from,1,1) }
        else
        {
          // m-3 & + 2 because I don't want to merge into the 0th element (C)
          while (tempEL(g-1)._1==tempEL(g+1)._1){g = ran.nextInt(m-3) + 2} 
          tempEL = tempEL updated (g+1, (tempEL(g+1)._1,
                                   tempEL(g)._2+tempEL(g+1)._2))
          tempEL = tempEL.splitAt(g)._1 ++ tempEL.splitAt(g+1)._2
          val b = Rho.getRho(tempEL)
          val c = if (to == "") {split(b,a)._2} else {1}
          (b, .5*.25/(m-3), c)
        }
      }
      
      if (t <= .25) { switch()}
      else if (t <= .5) { changeBound() }
      else if (t <= .75) { split( from =a, to ="" ) }
      else { merge( from =a, to ="" ) }

    }
  }

  def main(args:Array[String]){
    
    //  INITIALIZE:
    val N = 150000; val koIn = 8; val dIn = 4; var cnt = 0
    var M = new Array[String](N)
    val myFile = "mhResults.txt"
    M.update(0,Rho.draw(N=1,ko=koIn,d=dIn)(0))
    val proposal = new Proposal
    for (i <- 1 to N/2000) {print("#")}; println()
    //  END INITIALIZE

    for (i <- 1 to (N-1))
    {
      if ( (i-1)%2000 == 0 ) {print(">")}
      M.update(i, M(i-1) )
      val t = ran.nextDouble
      val cpp = proposal.draw(M(i),t)
      val cand = cpp._1; val cand2Curr = cpp._2; val curr2Cand = cpp._3
      val r = Rho.prob(cand,koIn,dIn) / Rho.prob(M(i),koIn,dIn) * 
              cand2Curr / curr2Cand
      if ((r > ran.nextDouble) & (cand.length==koIn)){
        M(i) = cand
        cnt += 1
      }
      //println(M(i)+" "+cpp._1+" "+r)
    } 
    println()
    println(cnt*1.0/N)
    outData(M.drop(5000),myFile)
  }
}
