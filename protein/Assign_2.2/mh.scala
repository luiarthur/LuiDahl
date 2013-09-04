import Rho._ 
  object mh { 

  val ran = new Random()

  def outData(m: Array[String], dest){
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
  }

  // Questions:
  //   How do I write the probabilty for merge and split?
  //   How do I write merge function?

  class q {

    def prob(a:String, b:String, ko:Int, d:Int, t:Int): Double = {
      if (t <=.5){1}
      else if (t <= .75) {}
      else {}
    }

    def draw(a:String,t:Double):String={
      val m = inEta(a).sum
      var g = ran.nextInt(m-2) + 1
      var blks =Array('H','E','T','C')

      if (t <= .25) { // switch function
        for (i <- 0 to blks.length){
          if ( (a(g-1)==blks(i)) || (a(g)==blks(i)) || (a(g+1)==blks(i)) ){
            blks.updated(i,'Z')
          }
        }
        blks = blks.filter (s => s!='Z')
        val r = ran.nextInt(blks.size) 
        val newBlk = (blks(r),getEL(a)(g)._2)
        getRho(getEL(a) updated (g, newBlk))
      }

      else if (t <= .5) { // change bnd. pos. function
        val z = getEL(a)(g-1)._2+getEL(a)(g)._2-1
        val p = ran.nextInt(z)+1
        getRho ((getEL(a) updated (g-1, (getEL(a)(g)._1,p) ) ) updated
                (m, (getEL(a)(g)._1,z-p) ) )
      }

      else if (t <= .75) { // split function
        val z = getEL(a)(g)._2 - 1
        val p = ran.nextInt(z) + 1
        if ( (a(g-1)==blks(i)) || (a(g)==blks(i)) ){
            blks.updated(i,'Z')
        }
        blks = blks.filter (s => s!='Z')
        val r = ran.nextInt(blks.size)
        var tempEL = getEL(a) updated (g, (getEL(a)(g)._1,z)) 
        tempEL = (tempEL.dropRight(m-g) :+ (blks(r),z-p) ) ++ tempEL.drop(g)        
        getRho(tempEL)
      }

      else { // merge function is very hard.
        var tempEL = getEL (a)
        // m-3 & + 2 because I don't want to merge into the 0th element (C)
	while (tempEL(g-1)._1==tempEL(g+1)._1){g = ran.nextInt(m-3) + 2} 
        tempEL = tempEL updated (g+1, (tempEL(g+1)._1,
                                 tempEL(g)._2+tempEL(g+1)._2))
        tempEL = tempEL.splitAt(g)._1 ++ tempEL.splitAt(g).drop(1)
        getRho(tempEL)
      }

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
      val cand = q.draw(M(i),t)
      val r = Rho.prob(cand,koIn,dIn) / Rho.prob(M(i),koIn,dIn) * 
              q.prob(M(i),cand,koIn,dIn,t) / q.prob(cand,M(i),koIn,dIn,t) 
      if (r > ran.nextDouble){
        M(i) = cand
        cnt += 1
      }
    } 
    println(cnt)
    outData(M.drop(5000),myFile)
  }
}
