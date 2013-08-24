#library(MCMCpack)

cntEta <- function(e, s){ 
  cnt = ifelse(e[1] == s, 1, 0)
  for (i in 2:length(e)){
    if ( (e[i-1] != e[i]) ){
      if (e[i]==s){cnt = cnt +1}
    }
  }
  return(cnt)
}
trans <- c('H','E','T','C')
th<- c(.170207, .224021, .178348, .427424) #real

#fake data################################################
rho <- c('C','C','H','H','H','H','H','T','T','T','H','H',#
         'H','H','H','E','E','E','T','T','T','C')        #
a <- c(3,5,3,7,3,7,3,26,8,34,2,4,23,7)                   #
k <- length(a)#                                          #
d <- 4 #?????                                            #
##########################################################

inEta <- function(r.v){
  m.v <- c(cntEta(r.v,'H'), cntEta(r.v,'E'), cntEta(r.v,'T'),cntEta(r.v,'C'))
  m <- sum(m.v) - 2
  m.v <- c(m.v[1:3],m.v[4]-2)
  return ( list( m.v, m ) )
}

prEta <- function(i){#need the right d and k; i = inEta(r.v)
  dnorm(i[[2]], 2.347253+.154154*k, 5.526382/d) * dmultinom(i[[1]],i[[2]],th)
}

#cs is a vector? m.v is a matrix?
#mh <- function(r.v,al,N=15000){
##  out <- NULL
#  out[[1]] <- r.v[[1]]
#  cnt <- 0
#  for(i in 2:N){
#    out[[i]] <- out[[i-1]]
#    #if some condition for the proposal?
#      cand <- c(5+rnbinom(1,mu=1.885880,size=6.953392),3+rnbinom(1,mu=2.521091,size=2.899121),
#                3+rnbinom(1,mu=0.839557,size=0.728294),1+rnbinom(1,mu=0.990796,size=3.725501))
#      r <- ( prEta(trans[cand]) / prEta(trans[out[i]]) ) * 
#           (  )
#      if (r > runif(1)) {
#        out[i] <- cand
#        cnt <- cnt + 1
#      }
#    #}
#  }
#  print(cnt/N)
##  #return(out[-(1:5000)])
#  return(out)
#}

#oEta <- mh(m.vec,.42)
#probEta <- function(vec){
##  cnt <- 0
#  for (i in 1:length(oEta)){
#    if ( min(oEta[[i]] == vec) == 1 ) {cnt = cnt+1}
#  }
#  return(cnt/length(oEta))
#}
#probEta(c(5,3,3,3))

LgivenE <- function(l,e){
  if (e=='H'){return (dnbinom(l-5,mu=1.885880,size=6.953392))}
  else if (e=='E'){return (dnbinom(l-3,mu=2.521091,size=2.899121))}
  else if (e=='T'){return (dnbinom(l-3,mu=0.839557,size=0.728294))}
  else {return (dnbinom(l-1,mu=0.990796,size=3.725501))}
}

LGE <- function(lam,eta){
  LgE <- NULL
  for (i in 1:length(lam)){
    LgE[i] <- LgivenE(lam[i],eta[i])
  }
  LgE
}

getEL <- function(r.v){
  E <- NULL; L <- NULL
  l <-length(r.v); cnt = 0; i = 1
  while ( i <= l ){
    j = 0
    while( (r.v[i+j] == r.v[i+j+1]) & (i+j+1 <= l) ){
      j=j+1
    }
    cnt = cnt + 1
    E[cnt] <- r.v[i]; L[cnt] <- j+1
    i=i+j+1;
  }
  return ( list(E,L) )
}
