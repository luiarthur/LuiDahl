import scala.io.Source

class Item(vc: Int, wc: Int, tc: Int){
  val v: Int = vc
  val w: Int = wc
  var t: Int = tc
}

object knap{

import scala.collection.mutable.ListBuffer


  var N = 0; var K = 0
  var items = new ListBuffer[Item]
/*
  var M = new ListBuffer[ListBuffer[Int]]
  def O(k: Int, j: Int): Int= {
    var w = items(j).w
    var v = items(j).v 
    if (j==0){0}
    else if (w<=k) {
      (M(k)(j-1) max v+M(k-w)(j-1))
    }
    else {M(k)(j-1)}
  }

  def taken(k: Int, n: Int): Int= {
    if ( (k==0) || (n==0) ){0}
    else if (M(k)(n) == M(k)(n-1)) {taken(k, n-1)}
    else{
      var w = items(n).w
      items(n).t = 1
      taken(k-w,n-1)
    }
  }
*/
  def main(args: Array[String]){

    var v = 0; var w = 0; var i = 0

    for (ln <- Source.fromFile(args(0)).getLines()){
      if (i==0) {
        N = ln.split(" ")(0).toInt;
        K = ln.split(" ")(1).toInt
        var item = new Item (0,0,0)
        items += item
      }
      else{
        v = ln.split(" ")(0).toInt; w = ln.split(" ")(1).toInt
        var item = new Item (v,w,0)
        items += item
        i += 1
      }
    }

    for (i <- 0 to N){
      println(items(i).v+" "+items(i).w+" "+items(i).t)
    }
/*
  for (c <- 0 to N){
    for (r <- 0 to K){
      if (c==0){ var m = new ListBuffer[Int]; M += m }
      M(r) += O(r,c)
    }
  }

  taken(K,N);
  println(M(K)(N)+" "+"0")
  for (c <- 1 to N){print(items(i).t+" ")}
  println();

*/
  }

} 
