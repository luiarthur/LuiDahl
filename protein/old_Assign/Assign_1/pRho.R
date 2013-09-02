th <- c(.170207, .224021, .178348, .427424)

cntEta <- function(e, s){ 
  cnt = ifelse(e[1] == s, 1, 0)
  for (i in 2:length(e)){
    if ( (e[i-1] != e[i]) ){
      if (e[i]==s){cnt = cnt +1}
    }
  }
  return(cnt)
}

inEta <- function(r.v){
  m.v <- c(cntEta(r.v,'H'), cntEta(r.v,'E'), cntEta(r.v,'T'),cntEta(r.v,'C'))
  m <- sum(m.v) - 2
  m.v <- c(m.v[1:3],m.v[4]-2)
  return ( list( m.v, m ) )
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

pRho <- function(rho,k=length(rho),d=4){
  m <- inEta(rho)[[2]] + 2
  mv <- inEta(rho)[[1]]
  E <- getEL(rho)[[1]]; L <- getEL(rho)[[2]]
  sumLogLamGivenEta <- 0; lge <- LGE(L,E)
  for (i in 1:length(E)){
    sumLogLamGivenEta <- sumLogLamGivenEta + log(lge[i])
  }
  
  exp(dnorm(m, 2.347253+.154154*k/.62, 5.526382/d,log=T) + dmultinom(mv,m-2,th,log=T) + sumLogLamGivenEta)
}
# for p=8, pRho is proportional to 1/.022483900175658
