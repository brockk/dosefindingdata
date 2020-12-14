
# ED_50

# Three dose scenario
n_d <- 3
set.seed(65741)
x <- rnorm(n = 100000, mean = n_d / 2, sd = n_d - 1)
y <- x[x>=0]
mean(y)
mean(y > n_d) # 0.29

# Five dose scenario
n_d <- 5
set.seed(65741)
x <- rnorm(n = 100000, mean = n_d / 2, sd = n_d - 1)
y <- x[x>=0]
mean(y)
mean(y > n_d) # 0.36

n_d <- 10
set.seed(65741)
x <- rnorm(n = 100000, mean = n_d / 2, sd = n_d - 1)
y <- x[x>=0]
mean(y)
mean(y > n_d) # 0.41


# N
set.seed(65741)
x <- rnorm(n = 100000, mean = 1, sd = 3)
y <- x[x>=0]
mean(y)


# Combined
library(dplyr)
library(ggplot2)

n <- 1000
set.seed(65741)
e0_ <- runif(n)
gamma_ <- runif(n)
emax_ <- gamma_ - e0_

# Five dose example
set.seed(65741)
n_d <- 5
ed50_ <- rnorm(n, mean = n_d / 2, sd = n_d - 1)
# ed50_ <- rnorm(n, mean = n_d / 2, sd = (n_d - 1) / 2)
N_ <- rnorm(n = n, mean = 1, sd = 3)
# N_ <- rnorm(n = n, mean = 1, sd = 3 / 2)

# Filter out the replicates corresponding to impossible values for N
keep <- (N_ >= 0) & (ed50_ > 0)
mean(keep)
e0 <- e0_[keep]
emax <- emax_[keep]
gamma <- gamma_[keep]
ed50 <- ed50_[keep]
N <- N_[keep]
X <- sapply(0:n_d, function(i) e0 + (emax * (i^N)) / (ed50 + (i^N)))
colnames(X) <- 0:n_d
rownames(X) <- 1:nrow(X)
X <- as_tibble(X)

bind_cols(
  data.frame(e0, gamma, emax, ed50, N) %>% head(20),
  X %>% head(20)
)

X %>% 
  as_tibble() %>% 
  # slice(20) %>% 
  head(50) %>%
  mutate(row = 1:nrow(.)) %>% 
  gather(Dose, ProbEvent, -row) %>% 
  mutate(Dose = Dose %>% as.numeric) %>% 
  ggplot(aes(x = Dose, y = ProbEvent, group = row)) + 
  geom_line(alpha = 0.1) + 
  scale_x_continuous(breaks = 0:n_d) + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = 'Probability of event')


# Ten dose example
set.seed(65741)
n_d <- 10
# ed50_ <- rnorm(n, mean = n_d / 2, sd = n_d - 1)
ed50_ <- rnorm(n, mean = n_d / 2, sd = (n_d - 1) / 2)
# N_ <- rnorm(n = n, mean = 1, sd = 3)
N_ <- rnorm(n = n, mean = 1, sd = 3 / 2)

# Filter out the replicates corresponding to impossible values for N
keep <- (N_ >= 0) & (ed50_ > 0)
mean(keep)
e0 <- e0_[keep]
emax <- emax_[keep]
gamma <- gamma_[keep]
ed50 <- ed50_[keep]
# HACK
# ed50[20] <- 2
N <- N_[keep]
# HACK
# N[20] <- 10
X <- sapply(0:n_d, function(i) e0 + (emax * (i^N)) / (ed50 + (i^N)))
colnames(X) <- 0:n_d
rownames(X) <- 1:nrow(X)
X <- as_tibble(X)

bind_cols(
  data.frame(e0, gamma, emax, ed50, N) %>% head(20),
  X %>% head(20)
)

X %>% 
  as_tibble() %>% 
  # slice(20) %>% 
  head(30) %>%
  mutate(row = 1:nrow(.)) %>% 
  gather(Dose, ProbEvent, -row) %>% 
  mutate(Dose = Dose %>% as.numeric) %>% 
  ggplot(aes(x = Dose, y = ProbEvent, group = row)) + 
  geom_line(alpha = 0.1) + 
  scale_x_continuous(breaks = 0:n_d) + 
  ylim(0, 1)

hist(e0)
hist(gamma)
hist(emax)
hist(ed50)
hist(N)
