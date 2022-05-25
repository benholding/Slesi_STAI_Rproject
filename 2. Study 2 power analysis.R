pacman::p_load(brms, dplyr, tidyr, purrr, tibble, ggplot2)

#power analysis
# based on: https://solomonkurz.netlify.app/post/bayesian-power-analysis-part-i/
n_sim <- 100

#Effect of sleep restriction on state anxiety: expected effect 0.60
mu_c <- 0
mu_t <- 0.6
n <- 50

d <-
  tibble(group = rep(c("control", "treatment"), each = n)) %>% 
  mutate(treatment = ifelse(group == "control", 0, 1),
         y         = ifelse(group == "control", 
                            rnorm(n, mean = mu_c, sd = 1),
                            rnorm(n, mean = mu_t, sd = 1)))

fit <-
  brm(data = d,
      family = gaussian,
      y ~ 0 + Intercept + treatment,
      prior = c(prior(normal(0, 2), class = b),
                prior(student_t(3, 1, 1), class = sigma)),
      seed = 1)


sim_d_and_fit1 <- function(seed, n) {
  
  mu_t <- .6
  mu_c <- 0
  
  set.seed(seed)
  
  d <-
    tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter") %>% 
    filter(parameter == "treatment")
}

s1 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d_and_fit1, n = 60)) %>% 
  unnest(b1)

s1 %>% 
  ggplot(aes(x = seed, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

s1 %>% 
  mutate(check = ifelse(Q2.5 > 0, 1, 0)) %>% 
  summarise(power = mean(check))

#Predictive effect of trait anxiety on state anxiety: expected effect 0.55

mu_t <- 0.55

d <-
  tibble(group = rep(c("control", "treatment"), each = n)) %>% 
  mutate(treatment = ifelse(group == "control", 0, 1),
         y         = ifelse(group == "control", 
                            rnorm(n, mean = mu_c, sd = 1),
                            rnorm(n, mean = mu_t, sd = 1)))

fit <-
  brm(data = d,
      family = gaussian,
      y ~ 0 + Intercept + treatment,
      prior = c(prior(normal(0, 2), class = b),
                prior(student_t(3, 1, 1), class = sigma)),
      seed = 1)


sim_d_and_fit2 <- function(seed, n) {
  
  mu_t <- .55
  mu_c <- 0
  
  set.seed(seed)
  
  d <-
    tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter") %>% 
    filter(parameter == "treatment")
}

s2 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d_and_fit2, n = 60)) %>% 
  unnest(b1)

s2 %>% 
  ggplot(aes(x = seed, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

s2 %>% 
  mutate(check = ifelse(Q2.5 > 0, 1, 0)) %>% 
  summarise(power = mean(check))

#Predictive effect of interaction effect: expected effect between -0.5 and 0.5 (i.e. overlapping zero)

mu_t <- 0.0

d <-
  tibble(group = rep(c("control", "treatment"), each = n)) %>% 
  mutate(treatment = ifelse(group == "control", 0, 1),
         y         = ifelse(group == "control", 
                            rnorm(n, mean = mu_c, sd = 1),
                            rnorm(n, mean = mu_t, sd = 1)))

fit <-
  brm(data = d,
      family = gaussian,
      y ~ 0 + Intercept + treatment,
      prior = c(prior(normal(0, 2), class = b),
                prior(student_t(3, 1, 1), class = sigma)),
      seed = 1)


sim_d_and_fit3 <- function(seed, n) {
  
  mu_t <- 0
  mu_c <- 0
  
  set.seed(seed)
  
  d <-
    tibble(group     = rep(c("control", "treatment"), each = n)) %>% 
    mutate(treatment = ifelse(group == "control", 0, 1),
           y         = ifelse(group == "control", 
                              rnorm(n, mean = mu_c, sd = 1),
                              rnorm(n, mean = mu_t, sd = 1)))
  
  update(fit,
         newdata = d, 
         seed = seed) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter") %>% 
    filter(parameter == "treatment")
}

s3 <-
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d_and_fit3, n = 60)) %>% 
  unnest(b1)

s3 %>% 
  ggplot(aes(x = seed, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_hline(yintercept = c(0, .5), color = "white") +
  geom_pointrange(fatten = 1/2) +
  labs(x = "seed (i.e., simulation index)",
       y = expression(beta[1]))

s3 %>% 
  mutate(check = ifelse(Q2.5 < 0 & Q97.5 > 0, 1, 0)) %>% 
  summarise(power = mean(check))

s3 %>% 
  mutate(check = ifelse(Q2.5 > -0.7 & Q97.5 < 0.7, 1, 0)) %>% 
  summarise(power = mean(check))

s3 %>% 
  mutate(check = ifelse(Q2.5 > -0.5 & Q97.5 < 0.5, 1, 0)) %>% 
  summarise(power = mean(check))

s3 %>% 
  mutate(check = ifelse(Q2.5 > -0.3 & Q97.5 < 0.3, 1, 0)) %>% 
  summarise(power = mean(check))
