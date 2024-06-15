## ch3
##

library(rethinking)
library(tidyverse)

p.grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(6, size = 9, prob = p.grid)
posterior <- prob_data * prob_p
posterior <- posterior/sum(posterior)

samples <- sample(p.grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)
dens(samples)
sum(samples<.5)/1e4

quantile(samples,.8)
HPDI(samples,.5)

sum(posterior * abs(.5 - p.grid))
loss <- sapply(p.grid,function(d) sum(posterior * abs(d - p.grid)))
p.grid[which.min(loss)]

dbinom(0:2,size = 2, prob = .7)
dummy_w <- rbinom(1e5, size = 2, prob = .7)
table(dummy_w)/1e5

dummy_w <- rbinom(1e5, size = 20, prob = .7)
hist(dummy_w/20,xlab = "dummy water prop")

dummy_w <- rbinom(1e3, size = 9, prob = .7)
hist(dummy_w/9,xlab = "dummy water prop")

dummy_w <- rbinom(1000, size = 15, prob = .5)
simplehist(dummy_w,xlab = "dummy water count")


# Easy questions:

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep (1,1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid,prob = posterior, size = 1e4, replace = TRUE)

# 3E1.
sum(samples < .2)/1e4
# 3E2.
sum(samples > .8)/1e4
# 3E3.
sum(samples > .2 & samples < .8)/1e4
# 3E4.
quantile(samples, .2)
# 3E5.
quantile(samples, .8)
# 3E6.
HPDI(samples, prob = .66)
# 3E7.
quantile(samples,(1-.66)/2)
quantile(samples, 1- ((1-.66)/2))

# 3M1.
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep (1,1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
# 3M2.
set.seed(100)
samples <- sample(p_grid,prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples,prob = .9)
# 3M3.
w <- rbinom(1e4,size = 15, prob = samples)
sum(w == 8)/1e4
# 3M4.
w <- rbinom(1e4,size = 9, prob = samples)
sum(w == 6)/1e4
# 3M5.
# new prior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- if_else(p_grid < 0.5, 0, 1)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid,prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples,prob = .9)
# with prior
w <- rbinom(1e4,size = 15, prob = samples)
sum(w == 8)/1e4


# old prior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep (1,1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid,prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples,prob = .9)
w <- rbinom(1e4,size = 15, prob = samples)
sum(w == 8)/1e4

# compare to true value
w <- rbinom(1e4,size = 15, prob = .7)
sum(w == 8)/1e4

# 3M6.
size <- seq(from = 2500, to = 2700,by = 1)
set.seed(100)
dummy_w <- map(size, function(n) rbinom(100,size = n, prob = samples))
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- if_else(p_grid < 0.5, 0, 1)
likelihood <- map2(dummy_w, size, \(x,y) map(x, \(x) dbinom(x,y,prob = p_grid)))
posterior <- map(likelihood, \(x) map(x, \(x) x * prior))
posterior <- map(posterior, \(x) map(x, \(x) x / sum(x)))
samples_w <- map(posterior, \(x) map(x, \(x) sample(p_grid,size = 1e4, replace = TRUE, prob = x)))
width <- map(samples_w, \(x) map(x, \(x) quantile(x,.995)-quantile(x,.005))) %>%
  map(\(x) unlist(x) %>% HPDI() %>% t() %>% as.data.frame()) %>% list_rbind() %>%
  mutate(size = size)
width %>% filter(`0.89|` < 0.05)

# hard problems
data(homeworkch3)

# 3H1.
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep (1,1000)
likelihood <- dbinom(sum(birth1) + sum(birth2), size = length(birth1) + length(birth2), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
p_grid[which.max(posterior)]

# 3H2.
set.seed(100)
samples <- sample(p_grid,prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples,prob = c(.5,.89,.97))

# 3H3.
b <- rbinom(1e4, size = 200, prob = samples)
dens(b)

# 3H4.
b1 <- rbinom(1e4, size = 100, prob = samples)
dens(b1)
sum(birth1)

# 3H5.
b2 <- rbinom(1e4, size = sum(birth1 == 0), prob = samples)
dens(b2)
sum(birth2[which(birth1 == 0)])
