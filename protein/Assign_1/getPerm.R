source('/data/arthurll/arthur/Dahl/scalaP/protein/sampPrior.R')
source('/data/arthurll/arthur/Dahl/scalaP/protein/sp4.R')
N <- 10^5

k <- 8; d <- 4
th<- c(.170207, .224021, .178348, .427424)
toStr <- function(x){
  s <- ''
  for (i in 1:length(x)){
    s = paste(s,x[i],sep='')
  }
  return (s)
}
rho <- rRho(N,k=k,d=d)
rs <- NULL
for (i in 1:length(rho)){
  rs[i] <- toStr(rho[[i]])
}
dfRho <- as.matrix(table(rs))


pRho <- function(rho){
  m <- inEta(rho)[[2]] + 2
  mv <- inEta(rho)[[1]]
  E <- getEL(rho)[[1]]; L <- getEL(rho)[[2]]
  #prodLGE <- 1; lge <- LGE(L,E)
  prodLGE <- 0; lge <- LGE(L,E)
  for (i in 1:length(E)){
    #prodLGE <- prodLGE * lge[i]
    prodLGE <- prodLGE + log(lge[i])
  }
  
  #dnorm(m, 2.347253+.154154*k, 5.526382/d) * dmultinom(mv,m-2,th) * prodLGE
  exp(dnorm(m, 2.347253+.154154*k/.62, 5.526382/d,log=T) + dmultinom(mv,m-2,th,log=T) + prodLGE)
}

M <- matrix(0,length(dfRho),2)
for (i in 1:length(dfRho)){
  M[i,1] <- dfRho[i]/length(rho)
  r <- NULL
  for (j in 1:k){
    r[j] <- substr(rownames(dfRho)[i],j,j)
  }
  M[i,2] <- pRho(r)
}

plot(M)
mod <- lm(M[,2] ~ M[,1]) 
mc <- mod$coef[2]
abline(mod)
newM  <- cbind(M[,1],M[,2]/mc)
rn <- rownames(dfRho)

write.table(cbind(rn,M[,1],M[,2]/sum(M[,2])),'/data/arthurll/arthur/res4.txt',
               row.names=F,col.names=F,sep ='\t',quote=F)
