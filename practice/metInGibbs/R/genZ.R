options("scipen" = 10)
options(digits = 10)
i <- .0001
u <- seq(i,1-i,by=i); q <- qnorm(u)
u <- format( round(u,4) , nsmall=4)
q <- format( round(q,4) , nsmall=4)
fo <- "/data/arthurll/arthur/Dahl/scala/metInGibbs/"
f <- paste(fo,"ztable.txt",sep='')
M <- cbind(u,q)
write(t(M), f, 2)

fin <- paste(fo,"mig2.cpp",sep='')
fout <- paste(fo,"cout.txt",sep='')

library(Rcpp)
sourceCpp(fin)
M <- read.table(fout,header=F)
head(M)
plot(density(M[,1]))
lines(out[,1],col='red')
