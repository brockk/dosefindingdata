
library(broom)
library(ggplot2)


# Example 1 - rate goes from 0 to 1 ----
test_data <- data.frame(
  n = rep(100, 7),
  success = c(1, 3, 10, 50, 85, 95, 100),
  x = c(-3, -2, -1, 0, 1, 2, 3)
) %>% mutate(success_rate = success / n)

test_data %>% 
  ggplot(aes(x = x, y = success_rate)) + 
  geom_point() + geom_line()

glm0 <- glm(success_rate ~ 1 + x, family = binomial('logit'), 
            data = test_data, weights = n)
glm0

augment(glm0, type.predict = "link")

augment(glm0, type.predict = "response") %>% 
  ggplot(aes(x = x, y = .fitted)) + 
  geom_point() + geom_line() + 
  geom_point(aes(y = success_rate), col = 'red')
# When the event rate goes to 1, the logit model makes sense.
# All good.


# Example 2 - rate goes from 0 to 0.35 ----
test_data <- data.frame(
  n = rep(100, 7),
  success = c(1, 2, 7, 15, 27, 33, 35),
  x = c(-3, -2, -1, 0, 1, 2, 3)
) %>% mutate(success_rate = success / n)

test_data %>% 
  ggplot(aes(x = x, y = success_rate)) + 
  geom_point() + geom_line()

glm0 <- glm(success_rate ~ 1 + x, family = binomial('logit'), 
            data = test_data, weights = n)
glm0

augment(glm0, type.predict = "response") %>% 
  ggplot(aes(x = x, y = .fitted)) + 
  geom_point() + geom_line() + 
  geom_point(aes(y = success_rate), col = 'red')
# When the event rate plateaus below 1, logit yields a poor fit.
# The inflection point has not been modelled.

# On log-odds scale
augment(glm0, type.predict = "link") %>%
  mutate(success_logodds = logit_scaled(success_rate)) %>% 
  ggplot(aes(x = x, y = .fitted)) + 
  geom_point() + geom_line() + 
  geom_point(aes(y = success_logodds), col = 'red')



# The EMax model ----
E0 <- 0.1
EMax <- 0.7
ED50 <- 2
lambda <- 4

emax <- function(d, E0, EMax, ED50, lambda) {
  E0 + (EMax * d^lambda) / (ED50 + d^lambda)
}

emax(0, E0, EMax, ED50, lambda)
emax(10, E0, EMax, ED50, lambda)
emax(2, E0, EMax, ED50, lambda)
emax(2, E0, EMax, ED50, lambda = 1)

curve(emax(x, E0, EMax, ED50, lambda = 1), from = 0, to = 5)
curve(emax(x, E0, EMax, ED50, lambda = 2), from = 0, to = 5)
curve(emax(x, E0, EMax, ED50 = 3, lambda = 2), from = 0, to = 5)
curve(emax(x, E0, EMax, ED50 = 5, lambda = 2), from = 0, to = 5)
curve(emax(x, E0, EMax, ED50 = 5, lambda = 3), from = 0, to = 5)

# These curves achieve the desired shape. But how to fit them?
# There is an rstanemax library:
# https://cran.r-project.org/web/packages/rstanemax/vignettes/emaxmodel.html
install.packages('rstanemax')
library(rstanemax)
library(dplyr)
exposure.response.sample %>% head
exposure.response.sample %>% group_by(dose) %>% 
  summarise(n(), mean(exposure), mean(response))
fit.emax <- stan_emax(response ~ exposure, data = exposure.response.sample)
fit.emax
# gamma is fixed
class(fit.emax)

plot(fit.emax)
# The ribbon shows confidence interval for the mean response. 
# Contrast with the plot below, for example...

preds <- posterior_predict(fit.emax, 
                           newdata = c(0, 50, 100, 250, 500, 1000, 2000), 
                           returnType = "tibble")
preds %>% 
  group_by(exposure) %>% 
  summarise(mean = mean(response), 
            l = quantile(response, 0.025), 
            u = quantile(response, 0.975)) %>% 
  ggplot(aes(x = exposure, y = mean)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.1)
# The ribbon here shows CIs for the responses

# How does this model fit the troublesome dataset from above?
test_data <- test_data %>% mutate(
  log_odds_success = logit_scaled(success_rate),
  dose_n = 1:nrow(.)
  )
fit <- stan_emax(log_odds_success ~ dose_n, data = test_data,
                 control = list(adapt_delta = 0.99))
plot(fit)
# At least the first-derivative has the correct sign now

# Scratch
#' @title Convert \code{\link{stanemax}} to instance of
#' \code{\link[coda]{mcmc.list}}
#'
#' @description This function allows stanemax to use tidybayes functions.
#'
#' @param stanemax Object of class \code{\link{stanemax}}
#' @param ... Extra variables that are passed onwards.
#'
#' @return Object of class \code{\link[coda]{mcmc.list}}
#' @method as.mcmc.list crm_fit
#'
#' @importFrom rstan As.mcmc.list
#' @export
as.mcmc.list.stanemax <- function(stanemax, ...) {
  # As.mcmc.list(stanemax$stanfit, ...)
  rstan::As.mcmc.list(stanemax$stanfit, ...)
}

library(tidybayes)
get_variables(fit.emax)

# Try fitting Emax model to one of my response series in need of help:
getwd()
source('Load.R')
obj_resp %>% 
  filter(Study == 'Larocca2013_1') %>% 
  mutate(DoseLevelN = 1:nrow(.),
         LogOddsEvent = logit_scaled(ProbEvent)) -> model_dat
model_dat
mod1 <- stan_emax(LogOddsEvent ~ DoseLevelN, data = model_dat, 
                  control = list(adapt_delta = 0.99))
mod1
plot(mod1) -> p1; p1

preds1 <-  posterior_predict(mod1, returnType = "tibble")
preds1 %>% 
  group_by(exposure) %>% 
  summarise(mean_lo = mean(response), 
            l_lo = quantile(response, 0.025), 
            u_lo = quantile(response, 0.975)) %>% 
  mutate(mean = inv_logit_scaled(mean_lo), 
         l = inv_logit_scaled(l_lo),
         u = inv_logit_scaled(u_lo)) %>% 
  ggplot(aes(x = exposure, y = mean)) + 
  geom_point(col = 'red') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, size = n), 
             col = 'blue') + 
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.1) + 
  ylim(0, 1) -> p1b
p1b
# This fitted series is certainly flatter than the mixed-effects models I'd run.

# How about one with gamma free?
mod2 <- stan_emax(LogOddsEvent ~ DoseLevelN, gamma.fix = NULL, 
                  data = model_dat, control = list(adapt_delta = 0.99))
# Divergence city! Yuck.

# Tighter prior on gamma and run for longer
mod3 <- stan_emax(LogOddsEvent ~ DoseLevelN, gamma.fix = NULL, 
                  data = model_dat, 
                  priors = list(gamma = c(0, 1)),  # Default is N(0, 3)
                  control = list(adapt_delta = 0.99), iter = 5000)
# Divergence-free
mod3
# gamma is pretty indistinguishable from 1 in this case.
plot(mod3) -> p3
p1 + p3
# Indistinguishable 


preds3 <-  posterior_predict(mod3, returnType = "tibble")
preds3 %>% head

# Sampled series
# Predictive dist, i.e. with noise arising from sigma  
preds3 %>% 
  mutate(prob_event = inv_logit_scaled(response)) %>% 
  head(2000) %>%
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(aes(group = mcmcid), alpha = 0.1, col=  'darkorange') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = 'Prob(ObjResp)') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# Predictive noise makes it up-and-downy.

# Posterior dist
preds3 %>% 
  head(4000) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(aes(group = mcmcid), alpha = 0.1, col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')

# In this model, what makes a line flat?
preds3 %>% 
  head(8) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(aes(group = mcmcid), col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# The second line (rows 5-8) is very flat
preds3 %>% 
  head(8)
# Is is gamma near 0? Or emax very low?

# Average param values:
expand.grid(exposure = 1:4, emax = 1.68, e0 = -1.01, ec50 = 4.54, 
            gamma = 0.97) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# The average line is flat

# Greater emax:
expand.grid(exposure = 1:4, emax = 10, e0 = -1.01, ec50 = 4.54, 
            gamma = 0.97) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# The line is higher and has positive gradient

# Greater e0:
expand.grid(exposure = 1:4, emax = 1.68, e0 = 5, ec50 = 4.54, 
            gamma = 0.97) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# The average line is much is higher but still flat

# Greater ec50:
expand.grid(exposure = 1:4, emax = 1.68, e0 = -1.01, ec50 = 10, 
            gamma = 0.97) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# The average line lower but still flat

# Greater gamma:
expand.grid(exposure = 1:4, emax = 1.68, e0 = -1.01, ec50 = 4.54, 
            gamma = 3) %>% 
  mutate(logodds_event = emax(exposure, E0 = e0, EMax = emax, ED50 = ec50, 
                              lambda = gamma),
         prob_event = inv_logit_scaled(logodds_event)) %>% 
  ggplot(aes(x = exposure, y = prob_event)) +
  geom_line(col=  'darkgreen') + 
  ylim(0, 1) + 
  labs(x = 'Dose-level', y = '') + 
  geom_point(data = model_dat, aes(x = DoseLevelN, y = ProbEvent, 
                                   size = n), col = 'black')
# The line has positive gradient.

# Intercept is controlled by emax, e0, ec50
# Gradient is controlled by emax and gamma
