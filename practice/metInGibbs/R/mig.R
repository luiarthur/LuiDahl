data <- c( .81, .83, .79, .75, .8 )
la <- function( x, a, b ){
        n <- length(x)
        return( 4 * log(a) - a/20 + n*(lgamma(a+b)-lgamma(a)) + (a-1)*sum(log(x)) )
      }
lb <- function( x, a, b ){
        n <- length(x)
        return ( 7 * log(b) - b/30 + n * (lgamma(a+b)-lgamma(b)) + (b-1) * sum (log(1-x)) )
      }

mig <- function ( x, csa = 50, csb = 20, N = 10^6 ){
         cat(paste(rep("#",50),collapse="")); cat("\n")
         out <- matrix(0,N,2)
         out[1,1] <- 5 * 20
         out[1,2] <- 8 * 30
         cnta <- 0; cntb <- 0
         for (i in 2:N){
           out[i,] <- out[i-1,]
           
           canda <- rnorm(1, out[i], csa)
           if (canda > 0) {
             r <- la(x,canda,out[i,2]) - la(x,out[i,1],out[i,2])
             if (r>log(runif(1))){
               out[i,1] <- canda
               cnta <- cnta + 1
             }
           } 
           
           candb <- rnorm(1,out[i,2], csb)
           if (candb>0){
             r <- lb(x,out[i,1],candb)-lb(x,out[i,1],out[i,2])
             if (r > log(runif(1))) {
               out [i,2] <- candb
               cntb <- cntb + 1
             }
           }

           if (i %% (N/50) == 0) cat("#")

         }
         cat("\n")
         print(cnta/N); print(cntb/N)
         return(out[-(1:5000),])
}

X <- mig(data,N=10^5)
write(t(X), 'rout.txt', 2)
