## ch3
##

library(rethinking)
set.seed(2971)
N <- 100
a <- rnorm(N,178,20)
b <- rlnorm(N,0,1)

plot(NULL,xlim= range(d2$weight),ylim = c(-100,400),
     xlab = "weight",ylab = "height")
abline(h = 0,lty = 2)
abline(h = 272,lty = 1, lwd = .5)

mtext("b-dnorm(0,10)")

xbar <- mean(d2$weight)
for(i in 1:N) {
  curve(a[i] + b[i] * (x - xbar),
        from = min(d2$weight),
        to = max(d2$weight),
        add = TRUE,
        col = col.alpha("black",.2))
}


m4.3 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178,20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)),
  data = d2
  )

post <- extract.samples(m4.3)
post[1:5,]

plot( height ~ weight , data=d2 , col = rangi2)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x-xbar),add = TRUE)
