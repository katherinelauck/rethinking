library(rethinking)
library(tidyverse)

data("WaffleDivorce")
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

sd(d$MedianAgeMarriage)

m5.1 <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a + bA * A,
    a ~ dnorm(0,.2),
    bA ~ dnorm(0,.5),
    sigma ~ dexp(1)),
  data = d
)

set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A = c(-2,2)))
plot(NULL, xlim = c(-2,2),ylim = c(-2,2))
for(i in 1:50) lines( c(-2,2), mu[i,], col = col.alpha("black",.4))

A_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.1, data = list(A = A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(D~A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

m5.2 <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a + bM * M,
    a ~ dnorm(0,.2),
    bM ~ dnorm(0,.5),
    sigma ~ dexp(1)),
  data = d
)

m5.3 <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a + bA* A + bM * M,
    a ~ dnorm(0,.2),
    bA ~ dnorm(0,.5),
    bM ~ dnorm(0,.5),
    sigma ~ dexp(1)),
  data = d
)
precis(m5.3)

plot(coeftab(m5.1,m5.2,m5.3), par = c("bA", "bM"))

m5.4 <- quap(
  alist(
    M ~ dnorm(mu,sigma),
    mu <- a + bAM* A,
    a ~ dnorm(0,.2),
    bAM ~ dnorm(0,.5),
    sigma ~ dexp(1)),
  data = d
)

mu <- link(m5.4)
mu_mean <- apply(mu,2,mean)
mu_resid <- d$M - mu_mean


data("WaffleDivorce")
d <- list()

d$D <- standardize(WaffleDivorce$Divorce)
d$M <- standardize(WaffleDivorce$Marriage)
d$A <- standardize(WaffleDivorce$MedianAgeMarriage)

m5.3_A <- quap(
  alist(
    ### A -> D <- M
    D ~ dnorm(mu,sigma),
    mu <- a + bA* A + bM * M,
    a ~ dnorm(0,.2),
    bA ~ dnorm(0,.5),
    bM ~ dnorm(0,.5),
    sigma ~ dexp(1),
    ### A -> M
    M ~ dnorm(mu_M,sigma_M),
    mu_M <- aM + bAM* A,
    aM ~ dnorm(0,.2),
    bAM ~ dnorm(0,.5),
    sigma_M ~ dexp(1)
    ),
  data = d
)

precis(m5.3_A)

A_seq <- seq(from = -2, to = 2, length.out = 30)

sim_dat <- data.frame(A = A_seq)
s <- sim(m5.3_A, data = sim_dat, vars = c("M", "D"))

plot(sim_dat$A, colMeans(s$D), ylim = c(-2,2), type = "l",
     xlab = "manipulated A",ylab = "counterfactual D")
shade( apply(s$D, 2, PI), sim_dat$A)

plot(si_dat$A, colMeans(s$M), ylim = c(-2,2), type = "l",
     xlab = "manipulated A",ylab = "counterfactual M")
shade( apply(s$M, 2, PI), sim_dat$A)


## masked relationship

data(milk)
d <- milk
str(d)


d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

m5.5_draft <- quap(
  alist(
    K~dnorm(mu, sigma),
    mu <- a + bN *N,
    a ~ dnorm(0,1),
    bN ~ dnorm(0,1),
    sigma ~ dexp(1)
    ),
  data = d
)

dcc <- d[complete.cases(d$K,d$N,d$M),]

m5.5 <- quap(
  alist(
    K~dnorm(mu, sigma),
    mu <- a + bN *N,
    a ~ dnorm(0,.2),
    bN ~ dnorm(0,.5),
    sigma ~ dexp(1)
  ),
  data = dcc
)

prior <- extract.prior(m5.5_draft)
xseq <- c(-2,2)
mu <- link(m5.5_draft, post = prior, data = list(N = xseq))

plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50) lines(xseq, mu[i,], col = col.alpha("black",.3))

precis(m5.5)


xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

m5.6 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )
precis(m5.6)

xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.6 , data=list(M=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ M , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

m5.7 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )
precis(m5.7)

plot( coeftab( m5.5 , m5.6 , m5.7 ) , pars=c("bM","bN") )


pairs( ~K + M + N ,
       dcc )

xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.7 , data=data.frame( M=xseq , N=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$M) , ylim=range(dcc$K) )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.7 , data=data.frame( N=xseq , M=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$N) , ylim=range(dcc$K) )
lines( xseq , mu_mean , lwd=2 )

shade( mu_PI , xseq )


## categorical variables

data(Howell1)
d <- Howell1
str(d)
mu_female <- rnorm(1e4,178,20)
mu_male <- rnorm(1e4,178,20) + rnorm(1e4,0,10)
precis( data.frame( mu_female , mu_male ) )

d$sex <- ifelse( d$male==1 , 2 , 1 )
str( d$sex )

m5.8 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a[sex] ,
    a[sex] ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )
precis( m5.8 , depth=2 )

post <- extract.samples(m5.8)
post$diff_fm <- post$a[,1] - post$a[,2]
precis( post , depth=2 )

data(milk)
d <- milk
unique(d$clade)

d$clade_id <- as.integer( d$clade )

d$K <- scale( d$kcal.per.g )
m5.9 <- quap(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ) , data=d )
labels <- paste( "a[" , 1:4 , "]:" , levels(d$clade) , sep="" )
plot( precis( m5.9 , depth=2 , pars="a" ) , labels=labels ,
      xlab="expected kcal (std)" )


set.seed(63)
d$house <- sample( rep(1:4,each=8) , size=nrow(d) )
m5.10 <- quap(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a[clade_id] + h[house],
    a[clade_id] ~ dnorm( 0 , 0.5 ),
    h[house] ~ dnorm( 0 , 0.8 ),
    sigma ~ dexp( 1 )
  ) , data=d )
precis(m5.10,depth = 2)

labels <- paste( "h[" , 1:4 , "]:" , levels(d$house) , sep="" )
plot( precis( m5.10 , depth=2 , pars="h" ) , labels=labels ,
      xlab="expected kcal (std)" )



## Practice problems

#5E2

