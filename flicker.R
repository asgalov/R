flicker <- function(n){
  hk <- vector(length = n)
  hk[1] <- 1
  for(k in 2:n){
    hk[k] <- (k - 0.5)*hk[k-1]/k
  }
  
  xx <- vector(length = n)
  ww <- 2.5*rnorm(n)
  xx[1] <- ww[1]

  for(k in 2:n){
    s <- 0
    m <- 1
    while(m < k){
      s <- s + hk[k-m]*ww[m]
      m <- m + 1
    }
    xx[k] <- s
  }
  xx
}

fl<-flicker(2000)
s<-spectrum(fl)
plot(s$freq, s$spec, log = "xy", type = "l", xlim=c(0.0001, 1),
     ylim=c(0.1, 1000), xlab = "frequency, Hz", ylab = "spectral density")
lines(s$freq, 1/(s$freq), log = "xy", type = "l", col="red")
lines(s$freq, 1/(s$freq^2), log = "xy", type = "l", col="blue")
legend("bottomleft", c("generated","1/f","1/(f^2)"), col=c("black", "red", "blue"), cex=1, lty=c(1,1,1))