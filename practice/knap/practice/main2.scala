//ListBuffer is too slow
import scala.collection.immutable.Vector.empty
import scala.io.Source

class Item(vc: Int, wc: Int, tc: Int){
  val v: Int = vc
  val w: Int = wc
  var t: Int = tc
}

object knap2{
  val item = new Item(0,0,0)
  var items = Vector.fill(0)(item)
  var M = Vector.fill(0,0)(0)
  var N = 0; var K = 0

  def O(j: Int, k: Int): Int= {
    val w = items(j).w
    val v = items(j).v 
    if ( (j==0) || (k==0) ) {0}
    else if (w<=k) {
      ( ( M(j-1)(k) ) max ( v + M(j-1)(k-w) ) )
    }
    else { M(j-1)(k) }
  }

  def taken(n: Int, k: Int): Int= {
    if ( (k==0) || (n==0) ){0}
    else if ( M(n)(k) == M(n-1)(k) ) {taken(n-1, k)}
    else{
      val w = items(n).w
      items(n).t = 1
      taken(n-1, k-w)
    }
  }

  def main(args: Array[String]){
    var bN = 0; var bK = 0
    var i = 0

    for (ln <- Source.fromFile(args(0)).getLines()){
      if (i==0) {
        bN = ln.split("\\s+")(0).toInt
        bK = ln.split("\\s+")(1).toInt
        val item = new Item(0,0,0); items = items:+item
        i += 1
      }
      else {
        val item = 
        new Item ( ln.split("\\s+")(0).toInt, ln.split("\\s+")(1).toInt, 0 )
        items = items:+item
      }
    }
    scala.io.Source.fromFile(args(0)).close

  N = bN; K = bK; 
// M = Vector.tabulate(N+1,K+1){(c,r) => O(c,r) }
// M is not updated as the values are filled...

// is this the fastest and most memory efficient?
// how about linked list or vector? 
// 10000 by 1000 is the biggest? then memory leak?
// what should I use for matrices?
  for (c <- 0 to N){
    val m = Vector.tabulate(K+1){ (r) => O(c,r) }
    M = M:+m
  }


  taken(N,K);
  println( M(N)(K) +" "+"0")
  for (c <- 1 to N){print(items(c).t+" ")}
  println()

  }
}

//scala -J-Xmx3g knap data/ks_1000_0
scala -J-Xmx10g knap2 data/ks_10000_0

