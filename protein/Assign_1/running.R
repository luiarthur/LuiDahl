source('/data/arthurll/arthur/Dahl/scalaP/protein/sp3.R')
rho <- rRho(1000,d=.02) #d=.121 => mean = 199

rl <- NULL
for (i in 1:100){
  rl[i] <- length( rho[[i]] )
}

plot(density(rl))
ma <- density(rl)$x[which.max(density(rl)$y)]
abline(v=mean(rl))
abline(v=ma)
ma



#source('/data/arthurll/arthur/Dahl/scalaP/protein/sp4.R')
#rho <- length(rRho(d=.3)[[1]])