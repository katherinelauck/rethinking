### compute grid approx

library(rethinking)

grid <- seq(from = 0, to = 1, length.out = 20)
prior <- rep(1,20)
likelihood <- dbinom(3, size=3,prob = grid)
unstd.post <- likelihood * prior
post <- unstd.post/sum(unstd.post)
plot(grid,post,type = "b",
     xlab = "pr(water)", ylab = "posterior prob")

l2 <- dbinom(3,size =4,prob = grid)
us.post2 <- l2*prior
post2 <- us.post2 / sum(us.post2)
points(grid,post2,type = "b",col="blue")

l3 <- dbinom(5,size =7,prob = grid)
us.post3 <- l3*prior
post3 <- us.post3 / sum(us.post3)
points(grid,post3,type = "b",col="red")

prior <- ifelse(grid < .5,0,1)

likelihood <- dbinom(3, size=3,prob = grid)
unstd.post <- likelihood * prior
post <- unstd.post/sum(unstd.post)
plot(grid,post,type = "b",
     xlab = "pr(water)", ylab = "posterior prob")

l2 <- dbinom(3,size =4,prob = grid)
us.post2 <- l2*prior
post2 <- us.post2 / sum(us.post2)
points(grid,post2,type = "b",col="blue")

l3 <- dbinom(5,size =7,prob = grid)
us.post3 <- l3*prior
post3 <- us.post3 / sum(us.post3)
points(grid,post3,type = "b",col="red")

## ch3
#1
set.seed(100)
p_grid <- seq(from = 0,to = 1,length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(6,size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4,replace = TRUE)
samples
sum(samples < .2)/1e4

#2
sum(samples > .8)/1e4

#3
sum(samples > .2 & samples < .8)/1e4

#4
quantile(samples,.2)
quantile(samples, .8)

#6
HPDI(samples, .66)

#7
quantile(samples,(1-.66)/2)
quantile(samples,.66 + (1-.66)/2)

#8
set.seed(100)
p_grid <- seq(from = 0,to = 1,length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(8,size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e5,replace = TRUE)
#9
HPDI(samples, .9)

#10
w <- rbinom(1e4,size = 15, prob = samples)
sum(w == 8)/1e4

#11
w <- rbinom(1e4,size = 9,prob = samples)
sum(w == 6)/1e4


#12
set.seed(100)
p_grid <- seq(from = 0,to = 1,length.out = 1000)
prior <- ifelse(p_grid < .5, 0,1)
likelihood <- dbinom(8,size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e5,replace = TRUE)
HPDI(samples, .9)

w <- rbinom(1e4,size = 15, prob = samples)
sum(w == 8)/1e4
w <- rbinom(1e4,size = 15, prob = .7)
sum(w == 8)/1e4

w <- rbinom(1e4,size = 9,prob = samples)
sum(w == 6)/1e4
w <- rbinom(1e4,size = 9,prob = .7)
sum(w == 6)/1e4

#13
set.seed(100)
p_grid <- seq(from = 0,to = 1,length.out = 1000)
prior <- ifelse(p_grid < .5, 0,1)
likelihood <- dbinom(.7 * 1e5,size = 1e5, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e7,replace = TRUE)
PI(samples,.99)

#14
library(rethinking)
data(homeworkch3)

grid <- seq(from = 0,to = 1, length.out = 1000)
prior <- rep(1,length(grid))
likelihood <- dbinom(sum(birth1) + sum(birth2),size = 200, prob = grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

plot(posterior ~ grid)

grid[which.max(posterior)]

samples <- sample(grid,prob = posterior,size = 1e4,replace = TRUE)

HPDI(samples, .5)
HPDI(samples, .89)
HPDI(samples, .97)

b <- rbinom(1e4,size = 200,prob = samples)
hist(b)
dens(b)
abline(v = 111)

sum(birth1)
b <- rbinom(1e4,size = 100,prob = samples)
dens(b)
abline(v=51)

b <- rbinom(1e4,size = sum(ifelse(birth1 == 0,1,0)),prob = samples)
dens(b)
after_girl <- birth2[which(birth1 == 0)]
abline(v = sum(after_girl))
