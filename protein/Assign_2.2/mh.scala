import Rho._


object mh {
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
  class q(t:Double){
    def prob{
    }
    def draw{
    }
  }

  def main(args:Array[String]){
    
    //  INITIALIZE:
    val N = 15000; val koIn = 8; val dIn = 4; var cnt = 0
    var M = new Array[String](15000)
    val myFile = "~/Documents/LuiDahl/protein/Assign2.2/mhResults.txt"
    M.update(0,Rho.draw(ko=koIn,d=dIn))
    //  END INITIALIZE

    for (i <- 1 to (N-1)){
      M.update(i, M(i-1) )
      val t = new Random().nextDouble
      val cand = q.draw(M(i),t)
      val r = Rho.prob(cand,ko=koIn,d=dIn) / Rho.prob(M(i),ko=koIn,d=dIn) * 
              q.prob(M(i),cand,ko=koIn,d=dIn) / q.prob(cand,M(i),ko=koIn,d=dIn) 
      if (r > new Random().nextDouble){
        M(i) = cand
        cnt += 1
      }
    } 
    println(cnt)
    outData(M.drop(5000),myFile)
  }
}
