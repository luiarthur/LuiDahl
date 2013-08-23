xx <- seq(0,60,length=1000)

g <- function(x){
  sqrt(2*pi)*(.3*dnorm(x,25,4)+.7*dnorm(x,40,9))
}

plot(xx,g(xx),type='l',lwd=2,col='blue') #kernel
lines(xx,.3*dnorm(xx,25,4)+.7*dnorm(xx,40,9),type='l',lwd=2) #true

#for sampling when not prior conjugates
#This is metropolis sampling: (different from metropolis hastings)

mh <- function(start,candSig,N=10^5){
  out <- NULL
  out[1] <- start #arbitrary
  count <- 0
  for(i in 2:N){
    out[i] <- out[i-1]
    cand <- rnorm(1,out[i-1],candSig) #this is the proposal
    r <- g(cand)/g(out[i-1])
    if(r >= runif(1)) {
      out[i] <- cand
      count <- count + 1
    }
  }
  print(count/N) #gives the proportion of times you move
  return(out)
}

#candSig too big, then your count/N will be less then 15%
#else, count/N > 40%
#we want count/N between 15% & 40%

out <- mh(start=50,candSig=5)
plot(out,type='l') #trace plot

plot(density(out),lwd=2,col='blue',type='l')
lines(xx,.3*dnorm(xx,25,4)+.7*dnorm(xx,40,9),type='l',lwd=2) #true