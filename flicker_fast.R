flicker <- function(n, qw = 1){
  hk <- vector(length = n)
  hk[1] <- 1
  for(k in 2:n){
    hk[k] <- (k - 0.5)*hk[k-1]/k
  }
  
  xx <- vector(length = n)
  ww <- qw * rnorm(n)
  
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

flicker_fast <- function(n, qw = 1){
  hk <- vector(length = n)
  hk[1] <- 1
  for(k in 2:n){
    hk[k] <- (k - 0.5)*hk[k-1]/k
  }
  
  ww <- qw * rnorm(n)
  hw <-  fft(ww) * fft(hk)
  hw_ift <- Re(fft(hw, inverse = TRUE)/n)
  hw_ift
}

#require("allanvar")
n <- 100000
fl <- flicker_fast(n, 0.1) + 5*rnorm(n) + cumsum(0.1*rnorm(n))
#plot(fl, type="l")
print("compute allan variance  ...")
av <- avar(fl, frequency(fl))
#write(av$av, "/home/gas/tmp/allan")
#av <- scan("/home/gas/tmp/allan")
#plot(rnorm(100))
plot(av$av, log = "xy", type="l", col="blue", ylim=c(0.1, 100), lwd=1)
grid(equilogs=TRUE, lwd=1, col="orange")
#title(main = "Allan variance Analysis Comparison", xlab = "Cluster Times (Sec)", ylab = "Allan Standard Deviation (rad/s)")


#plot(fl, type = "l")
#av <- avar(fl, frequency(fl))
#plot(av$av, log = "xy", xaxt="n" , yaxt="n", type="l", col="blue", xlab="", ylab="")
#s<-spectrum(fl)
#plot(s$freq, s$spec, log = "xy", type = "l", xlim=c(0.0001, 1), ylim=c(0.1, 1000), xlab = "frequency, Hz", ylab = "spectral density")
#lines(s$freq, 1/(s$freq), log = "xy", type = "l", col="red")
#lines(s$freq, 1/(s$freq^2), log = "xy", type = "l", col="blue")
#legend("bottomleft", c("generated","1/f","1/(f^2)"), col=c("black", "red", "blue"), cex=1, lty=c(1,1,1))