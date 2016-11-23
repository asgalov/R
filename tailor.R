exp_tailor<-function(x, a){
  sum <- exp(a)
  for (i in 1:50) {
    sum <- sum + exp(a)*((x - a)^i)/factorial(i)
  }
  sum
}

x<-c(-1000:1000)/100
print(exp(8.3) - exp_tailor(8.3, 8))
plot(x, exp(x), type="l", col="blue")
lines(x, exp_tailor(x, 0), col="red")