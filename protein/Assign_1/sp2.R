
#ll <- function(m,mv){
#  mH<-mv[1];mE<-mv[2];mT<-mv[3];mC<-mv[4]
#  log(pN(m)) + log (pMN(mH,mE,mT,mC-2)) + sum(log(p(lge)))
#}

rho <- c('C','C','H','H','H','H','H','T','T','T','H','H',
         'H','H','H','E','E','E','T','T','T','C') 
k <- length(rho)
d <- 4

LgE <- function(e,l){
  if (e=='H'){return (5 + dnbinom(l,mu=1.885880,size=6.953392))}
  else if (e=='E'){return (3 + dnbinom(l,mu=2.521091,size=2.899121))}
  else if (e=='T'){return (3 + dnbinom(l,mu=0.839557,size=0.728294))}
  else {return (1 + dnbinom(l,mu=0.990796,size=3.725501))}
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

inEta <- function(r.v){
  m.v <- c(cntEta(r.v,'H'), cntEta(r.v,'E'), cntEta(r.v,'T'),cntEta(r.v,'C'))
  m <- sum(m.v) - 2
  m.v <- c(m.v[1:3],m.v[4]-2)
  return ( list( m.v, m ) )
}

slog <- function(E,L){
  sl <- 0
  for (i in 1:length(E)){
    sl <- sl + log(LgE(E[i],L[i]))
  }
  return (sl)
}

ll <- function(r.v){
  m <- inEta(r.v); E <-getEL(r.v)[[1]]; L <- getEL(r.v)[[2]];
  if (E[1] != 'C') {return (0) }
  if (E[length(E)] !='C') {return (0)}
  for (i in 1:length(E)){
    if ( (E[i] == 'H') & (L[i] < 5) ) {return (0)}
    if ( (E[i] == 'E') & (L[i] < 3) ) {return (0)}
    if ( (E[i] == 'T') & (L[i] < 3) ) {return (0)}
  }
  return (
  log(dnorm(m[[2]], 2.347253+.154154*k, 5.526382/d))
  + log(dmultinom(m[[1]],m[[2]],th)) + slog(E,L) )
}

getRho <- function(E,L){
  r.v <- NULL; N = length(E); k = 1
  for (i in 1:N){
    for (j in 1: (L[i]) ){
      r.v[k] <- E[i]
      k <- k+1
    }
  }
  return ( r.v )
}

rRho <-function(l){
  r.v <- NULL
  r.v[1] <- 'C'
  j <- 1
  temp <- sample(c('H','E','T','C'),1)
  while ( (length(r.v) + ifelse(temp=='H',5,ifelse(temp=='C',1,3)) )< l){
    if ( temp == 'H' ){
      for (i in 1:5) {
        r.v[j+i] <- 'H'
      }
      j <- j+i
    }
    else if (temp =='C'){
      r.v[j+1] <- 'C'
      j <- j+1
    }
    else {
      for (i in 1:3) {
        r.v[j+i] <- temp
      }
      j <- j+i
    }
    temp <- sample(c('H','E','T','C'),1)
  }
  r.v[j+1] <- 'C'
  return (r.v)
}

mh <- function(r.v, N=15000){
  out <- NULL; l <- length(r.v)
  out[[1]] <- r.v
  cnt <- 0
  for(i in 2:N){
    out[[i]] <- out[[i-1]]
    cand <- rRho(l)
    if ( (ll(cand)!=0) ){
      r <- ll(out[[i]]) - ll(cand)
      if (r > log(runif(1))) {
        out[[i]] <- cand
        cnt <- cnt + 1
      }
    }
  }
  print(cnt/N)
  return(out[-(1:5000)])
}

out <- mh(rho)