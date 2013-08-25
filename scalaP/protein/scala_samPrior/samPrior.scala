import Gamma.Gamma._
import scala.io.Source
import java.io.File
import scala.math
import util.Random

object rRho{

  def logLamGivenEta (eta: Char){
    if (eta=="H"){5+rnbinom(1.885880,6.953392)}
    else if (eta="E"){3+rbinom(mu=2.521091,size=2.899121)}
  }
}
