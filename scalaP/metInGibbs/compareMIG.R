rout <- read.table("/data/arthurll/arthur/Dahl/scalaP/metInGibbs/R/rout.txt",header=F)
cout <- read.table("/data/arthurll/arthur/Dahl/scalaP/metInGibbs/cpp/cout.txt",header=F)
sout <- read.table("/data/arthurll/arthur/Dahl/scalaP/metInGibbs/scala/sout.txt",header=F)

mr <- rout[,1]/(rout[,1]+rout[,2])
mc <- cout[,1]/(cout[,1]+cout[,2])
ms <- sout[,1]/(sout[,1]+sout[,2])

plot(density(mr),col='blue',lwd=2)
lines(density(mc),col='red',lwd=2)
lines(density(ms),col='green',lwd=2)

abline(v=mean(mr), col='blue')
abline(v=mean(mc), col='red')
abline(v=mean(ms), col='green')
