library(rethinking)

# Ch 4

pos <- replicate(1000, sum(runif(16,-1,1)))
hist(pos)
dens(pos)

growth <- replicate(1000, prod(1 + runif(12,0,.1)))
dens(growth, norm.comp = TRUE)



data("Howell1");d <- Howell1; d2 <- d[d$age >= 18,]
precis(d)
dens(d2$height)

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
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI,prob = .89)
mu.HPDI <- apply(mu,2,HPDI,prob = .89)

sim.height <- sim(m4.3, data = list(weight = weight.seq),n = 1e4)
height.PI <- apply(sim.height,2,PI,prob= .89)

plot(height ~ weight, data = d2, col = col.alpha(rangi2,.5))
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)
shade(height.PI,weight.seq)

plot(height ~ weight, d)

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(178,20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,1),
    sigma ~ dunif(0,50)),
  data = d)

precis(m4.5)

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3

m4.6 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
    a ~ dnorm(178,20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    sigma ~ dunif(0,50)),
  data = d)

precis(m4.6)

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 , weight_s3 = weight.seq^3)
mu <- link( m4.6 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.6 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

data("cherry_blossoms")
d <- cherry_blossoms
precis(d)
plot(doy~year,data = d)

d2 <- d[ complete.cases(d$doy) , ]
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

knot_list

library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )

plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )

m4.7 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)),
  data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

post <- extract.samples( m4.7 )
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

mu <- link( m4.7 )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16)
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )
lines(d2$year,mu_mean)

set.seed(2728)
N <- 1000
mu <- rnorm(N, 0,10)
sigma <- rexp(N,1)

y <- rnorm(N,mean = mu,sd = sigma)

# m4m2 <- quap(
#   alist(
#     y ~ dnorm(mu,sigma),
#     mu ~ dnorm(0,10),
#     sigma ~ dexp(1)))

## 4m4

data("cherry_blossoms")
d <- cherry_blossoms
precis(d)
plot(doy~year,data = d)

d2 <- d[ complete.cases(d$doy) , ]
num_knots <- 30
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

knot_list

library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )

plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )

m4.7 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)),
  data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

post <- extract.samples( m4.7 )
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

mu <- link( m4.7 )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16)
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )
lines(d2$year,mu_mean)

library(rethinking)

data("Howell1");d <- Howell1; d2 <- d[d$age >= 18,]
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
    mu <- a + b*(weight),
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
curve(a_map + b_map*(x),add = TRUE)
precis(m4.3)
cov2cor(vcov(m4.3))

## 4H1

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3

m4.6 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
    a ~ dnorm(178,20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    sigma ~ dunif(0,50)),
  data = d)

precis(m4.6)

pred_w_s <- (c(46.95,43.72,64.78,32.59,54.63) - mean(d$weight))/sd(d$weight)

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )

pred_dat <- list( weight_s=pred_w_s , weight_s2=pred_w_s^2 , weight_s3 = pred_w_s^3)
mu <- link( m4.6 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.6 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

mu.mean
height.PI

## 4H2

d2 <- d[d$age < 18,]

xbar <- mean(d2$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178,20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)),
  data = d2
)

precis(m4.3)

plot( height ~ weight , data=d2 , col = rangi2)

weight.seq <- seq(from = min(d2$weight),to = max(d2$weight),by = 1)
mu <- link(m4.3,data = data.frame(weight = weight.seq))
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI,prob = .89)
mu.HPDI <- apply(mu,2,HPDI,prob = .89)

sim.height <- sim(m4.3, data = list(weight = weight.seq),n = 1e4)
height.PI <- apply(sim.height,2,PI,prob= .89)

plot(height ~ weight, data = d2, col = col.alpha(rangi2,.5))
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)
shade(height.PI,weight.seq)

### I think a better model would allow the curve to change slope at different weights, assuming that children of different ages will grow at different rates. Would be nice to have an age predictor to relate this change to biologically meaningful infomation.

## 4H3

xbar <- mean(d$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*(log(weight)-log(xbar)),
    a ~ dnorm(178,20),
    b ~ dlnorm(4,.7),
    sigma ~ dunif(0,20)),
  data = d
)



precis(m4.3)

plot( height ~ weight , data=d , col = rangi2)

weight.seq <- seq(from = min(d$weight),to = max(d$weight),by = 1)
mu <- link(m4.3,data = data.frame(weight = weight.seq))
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI,prob = .89)
mu.HPDI <- apply(mu,2,HPDI,prob = .89)

sim.height <- sim(m4.3, data = list(weight = weight.seq),n = 1e4)
height.PI <- apply(sim.height,2,PI,prob= .89)

plot(height ~ weight, data = d, col = col.alpha(rangi2,.5))
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)
shade(height.PI,weight.seq)

set.seed(2971)
N <- 100
a <- rnorm(N,178,20)
b <- rlnorm(N,4,.7)

plot(NULL,xlim= range(d$weight),ylim = c(-100,400),
     xlab = "weight",ylab = "height")
abline(h = 0,lty = 2)
abline(h = 272,lty = 1, lwd = .5)

mtext("b-dlnorm(0,.5)")

xbar <- mean(d$weight)
for(i in 1:N) {
  curve(a[i] + b[i] * (log(x) - log(xbar)),
        from = min(d$weight),
        to = max(d$weight),
        add = TRUE,
        col = col.alpha("black",.2))
}

