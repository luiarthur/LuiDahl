//ListBuffer is too slow
import scala.collection.immutable.Vector.empty
import scala.io.Source

class Item(vc: Int, wc: Int, tc: Int){
  val v: Int = vc
  val w: Int = wc
  var t: Int = tc
}

object knap{
  val item = new Item(0,0,0)
  var items = Vector.fill(0)(item)
  var M = Vector.fill(0,0)(0)
  var N = 0; var K = 0

  def O(k: Int, j: Int): Int= {
    val w = items(j).w
    val v = items(j).v 
    if (w<=k) {
      ( ( M(k)(j-1) ) max ( v + M(k-w)(j-1) ) )
    }
    else { M(k)(j-1) }
  }

  def taken(k: Int, n: Int): Int= {
    if ( (k==0) || (n==0) ){0}
    else if ( M(k)(n) == M(k)(n-1) ) {taken(k, n-1)}
    else{
      val w = items(n).w
      items(n).t = 1
      taken(k-w,n-1)
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
  M = Vector.fill(K+1,N+1)(0);

  for (c <- 1 to N){
    for (r <- 0 to K){
      M = M updated (r, ( M(r) updated(c, O(r,c) ) ) )
      //if ( (r % 150000 == 0) ){ println(r+" "+c) }
    }
  }

  taken(K,N);
  println( (M(K))(N) +" "+"0")
  for (c <- 1 to N){print(items(c).t+" ")}
  println()

  }
}

// scala -J-Xmx3g knap data/ks_1000_0

