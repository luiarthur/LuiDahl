rRho <- function(N=1,ko=200,d=4){
  r.v <- NULL
  #k <- ko
  #k <- round(ko/0.642975)
  k <- round(ko/0.62)
  for (nn in 1:N){
    rightSize <- F
    while (!rightSize){
      
      #k <- 2000; d <- 4   # Correct?
      m.v <- seq(3,k)  # Correct?    
      th<- c(.170207, .224021, .178348, .427424)
      
      f <- function(m.i){
        dnorm(m.i,2.347253+.154154*k, (5.526382/d))
      }
      
      #temp <- rnorm(50000,2.347253+.154154*k, (5.526382/d)^2)
      #plot(density(temp))
      #mean(temp)
      
      lGe <- function(e){
        if (e=='H'){return (5 + rnbinom(1,mu=1.885880,size=6.953392))}
        else if (e=='E'){return (3 + rnbinom(1,mu=2.521091,size=2.899121))}
        else if (e=='T'){return (3 + rnbinom(1,mu=0.839557,size=0.728294))}
        else {return (1 + rnbinom(1,mu=0.990796,size=3.725501))}
      }
      
      getRho <- function(E,L){
        r.v <- NULL; N = length(E); k = 1
        for (i in 1:N){
          for (j in 1:(L[i]) ){
            r.v[k] <- E[i]
            k <- k+1
          }
        }
        return ( r.v )
      }
      
      
      valid = F; #loops = 0
      while (!valid){
        m <- sample(c(0,1,2,m.v),prob=c(0,0,0,f(m.v)),size=1)
        mv <- rmultinom(1,m-2,th)
        eta <- NULL; eta[1] <- 'C'; eta[m] <- 'C'
        valid = T; #loops= loops+1
        temp <- sample(c(rep('H',mv[1],),rep('E',mv[2],),rep('T',mv[3],),rep('C',mv[4],)),replace=F)
        eta[2:(m-1)] <- temp[1:(m-2)]
        for (i in 2:(length(eta)-1)){
          if (eta[i]==eta[i-1]){
            for (j in i:(length(eta)-1)){
              if ( (eta[j]!=eta[i-1]) & (eta[j]!=i+1) & (eta[i]!=j+1) & (eta[i]!=j-1)){
                temp = eta[j]
                eta[j]=eta[i]
                eta[i]=temp
              }
            }
          }
        }
        for (i in 2:length(eta)){
          if (eta[i]==eta[i-1]){valid=F;break}
        }
      }
      #print(loops);
      
      lam <- NULL; for (i in 1:m){lam[i] <- lGe(eta[i])}
      r.v[[nn]] <- getRho(eta,lam); rl <- length(r.v[[nn]])
      if (rl == ko ){rightSize = T}
    }
    
  }
  return(r.v)
}
#source('/data/arthurll/arthur/Dahl/scalaP/protein/sp4.R')
#rho<-rRho()


#mu <- 4; sd <- 15

#f1 <- function(x){
#  5*dnorm(x,mu,sd)
#}
#f2 <- function(x){
#  dnorm(x,mu,sd)
#}
#mm <- seq(from=mu-3*sd, to=mu+3*sd,by=1)
#s1 <- sample(mm,prob=f1(mm),size=10^5,replace=T)
#s2 <- sample(mm,prob=f2(mm),size=10^5,replace=T)
#s3 <- rnorm(10^5,mu,sd)
#plot(density(s1),col='blue')
#lines(density(s2),col='red')
#lines(density(s3),col='orange')
#curve(dnorm(x,mu,sd),from=mu-3*sd, to=mu+3*sd,col='green',add=T)
