import Rho._ 
import util.Random
import java.io.File
import scala.io.Source

object mh { 

  val ran = new Random()

  def outData(m: Array[String], dest: String): Int = 
  {
    val pw = new java.io.PrintWriter(new File(dest))
    val N = m.size
    try 
    { 
      var i = 0; while (i < N)
      { pw.write( m(i) + s"\n"); i = i + 1 }
    } 
    finally { pw.close() }
    0
  }

  // Questions:
  //   How do I write the probabilty for merge and split?
  //   How do I write merge function?

  // NEED TO ADD Rho. before each function of Rho
  class Proposal {

    def draw(a: String, t: Double): (String, Double, Double) =
    {
      val m = Rho.inEta(a).sum
      var g = ran.nextInt(m-1) + 1 // picks the block to change
                                   // draws from positions 1 to m-1
                                   // of the amino acid sequence.
                                   // i.e. If the a.a. has length m,
                                   //      I can make changes at 
                                   //      every position except for
                                   //      the very first one.
      var blks =Array('H','E','T','C')

      def switch: (String, Double, Double) =
      {
        //println("SWITCH")
        // CANNOT sample an a.a that is going to merge or keep chain unmerged.
        blks = blks.filter( s => ( (a(g-1) != s) & (a(g) != s) & (a(g+1) != s) ) )
        val r = ran.nextInt(blks.size) 
        val newBlk = (blks(r),Rho.getEL(a)(g)._2)
        (Rho.getRho(Rho.getEL(a) updated (g, newBlk)), 1, 1)
      }
      
      // Check for OutOfBound ERROR:
      def changeBound: (String, Double, Double) =
      {
        //println("CHANGEBOUNDARY")
        val z = Rho.getEL(a)(g-1)._2 + Rho.getEL(a)(g)._2
        val p = ran.nextInt(z-1) + 1
        (Rho.getRho((Rho.getEL(a) updated (g-1, (Rho.getEL(a)(g-1)._1,p) ) ) updated
                   (g, (Rho.getEL(a)(g)._1,z-p) )), 1, 1 )
      }

      // Check for OutOfBound ERROR:
      def split(from: String, to: String): (String, Double, Double) = 
      { 
        //println("SPLIT: "+a)
        val z = Rho.getEL(from)(g)._2 
        if ( z > 1 ){
          val p = ran.nextInt(z-1) + 1
          blks = blks.filter( s => ( (s != from(g-1)) & (s != from(g)) ) )
          val r = ran.nextInt(blks.size)
          var tempEL = Rho.getEL(from) updated (g, (Rho.getEL(from)(g)._1,p)) 
              tempEL = (tempEL.dropRight(m-g) :+ (blks(r),z-p)) ++ tempEL.drop(g)
          val b = Rho.getRho(tempEL)
          val c = if (to == "") {merge(from=b, to=a)._2} else {1}
          (b, .25/(m-2)/(z-1)/(blks.length), c)
        }
        else { (from, 1, 1) }
      }

      def merge(from: String, to: String): (String, Double, Double) =
      {
        //println("MERGE: "+a)
        var tempEL = Rho.getEL(from)
        if ( (g > 1) & (m > 3) )
        {
          //println("m is greater than 3")
          if ( g == m-1 ) { (from, 1, 1) } 
          else if ( tempEL(g-1)._1 == tempEL(g+1)._1) { (from, 1, 1) }
          else {
            tempEL = tempEL updated (g+1, (tempEL(g+1)._1,
                                     tempEL(g)._2 + tempEL(g+1)._2))
            tempEL = tempEL.splitAt(g)._1 ++ tempEL.splitAt(g+1)._2
            val b = Rho.getRho(tempEL)
            val c = if (to == "") {split(b,a)._2} else {1}
            (b, .25*.5/(m-2), c)
          }
        }
        else { (from, 1, 1) }
      }
      
      if (t <= .25) { switch}
      else if (t <= .5) { changeBound }
      else if (t <= .75) { split( from =a, to ="" ) }
      else { merge( from =a, to ="" ) }

    }
  }

  def main(args:Array[String]){
    
    //  INITIALIZE:
    val N = 250000; val koIn = 8; val dIn = 4; var cnt = 0
    var M = new Array[String](N)
    val myFile = "mhResults.txt"
    M.update(0,Rho.draw(N=1,ko=koIn,d=dIn)(0))
    val proposal = new Proposal
    for (i <- 1 to N/2500) {print("#")}; println()
    //  END INITIALIZE

    for (i <- 1 to (N-1))
    {
      if ( (i-1)%2500 == 0 ) {print(">")}
      M.update(i, M(i-1) )
      val t = ran.nextDouble
      val cpp = proposal.draw(M(i),t)
      val cand = cpp._1; val cand2Curr = cpp._2; val curr2Cand = cpp._3
      val r = Rho.prob(cand,koIn,dIn) / Rho.prob(M(i),koIn,dIn) * 
              curr2Cand / cand2Curr
      if ((r > ran.nextDouble) & (cand.length==koIn))
      {
        M(i) = cand
        cnt += 1
      }
    } 
    println()
    println("Acceptance: "+cnt*1.0/N+"\t"+"N: "+N+"\t"+"Burn in: "+5000)

    //print data in a useful format
    val dM = M.drop(5000).distinct
    println("Number of Distinct Sequences: "+dM.size)
    println()

    val MM = (Array.tabulate(dM.size)
    {
      i => (dM(i), (M.count(s => s == dM(i)).toDouble/M.size *
                    100000).toInt/100000.0 )
    }).sorted 

    var sumTheorProb = 0.0
    for ( i <- 0 to (dM.size-1) )
    { sumTheorProb = sumTheorProb + Rho.prob(dM(i)) }  

    MM.foreach( s => println(s._1+"\t\t"+s._2+"\t\t"+Rho.prob(s._1)/sumTheorProb) ) 
    println()
    //MM.foreach( s => println(s._1+"\t"+s._2+"\t\t"+Rho.prob(s._1)) ) 
    //outData(MM.drop(5000),myFile)
  }
}
