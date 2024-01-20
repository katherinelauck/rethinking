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

N <- 200
dN <- d2[1:N,]
mN <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*(weight - mean(weight)),
    a ~ dnorm(178,20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)),
  data = dN
)

post <- extract.samples(mN,n = 20)
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight", ylab="height")

mtext(concat("N = ",N))

for(i in 1:nrow(post)) {
  curve(post$a[i] + post$b[i] * (x - mean(dN$weight)),
        col = col.alpha("black",.3),add = TRUE)
}


post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50-xbar)
dens(mu_at_50,col = rangi2,lwd = 2, xlab = "mu|weight=50")
PI(mu_at_50,prob = .89)
mu <- link(m4.3)
str(mu)


weight.seq <- seq(from = 25,to = 70,by = 1)
mu <- link(m4.3,data = data.frame(weight = weight.seq))
str(mu)
plot(height ~ weight, d2, type = "n")
for(i in 1:1000) {
  points(weight.seq,mu[i,],pch = 16, col = col.alpha(rangi2,0.1))
}
