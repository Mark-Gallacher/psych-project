library(tidyverse)
library(pwr)
library(Rmpfr)

options(digits = 7)

sample_size <- c(c(2:19), seq(20, 10000, 10))
effect_size <-  0.2
effect_sizes <-  c(0.1, 0.2, 0.3, 0.4, .5)
alpha <-  0.05
vector_len <- length(sample_size)*length(effect_sizes)
criteff_vector <- vector(length = vector_len)
beta_vector <- vector(length = vector_len)
alpha_vector <- vector(length = vector_len)
effect_vector <- vector(length = vector_len)
#----------------------------------------------------------
# Method 1 - using Base R and pwr package - to prevent R from rounding numbers to 1 or 0

### Min Alpha - Beta Rule - Stop when power is 1 to stop weird lines in graph
for(d in 1:length(effect_sizes)){
  power <- 0.5 # just to reset power, before it check it in the if statement
  effect_size <- effect_sizes[d]
  alpha <- 0.05

  for(n in 1:length(sample_size)){
    # position in vector
    pos <- length(sample_size) * (d - 1) + n
    
    if(round(power, digits = 21) >= 1 | round(alpha, digits = 21) <= 0){
      # beta_vector[pos] <- beta_vector[pos-1]
      # alpha_vector[pos] <- alpha_vector[pos-1]
      # criteff_vector[[pos]] <- criteff_vector[[pos-1]]
      # effect_vector[pos] <- effect_size
      break
    }else{
    # find power
    power <- pwr.t.test(n = sample_size[n], 
                        d = effect_size,
                        sig.level = alpha, 
                        alternative = "two.sided",
                        type = "two.sample")$power
    # find beta
    beta_vector[pos] <- 1 - power
    
    # alpha <- min(alpha, beta_vector[pos])
    # alpha_vector[pos] <- alpha
    # Without Sequential Alpha, the graphs go beyond the effect / 2 (where they are meant to plateau)
    alpha_vector[pos] <- min(alpha, beta_vector[pos])

    effect_vector[pos] <- effect_size
    
    if(round(power, digits = 16) >= 1 | round(alpha, digits = 16) <= 0){
      # beta_vector[pos] <- beta_vector[pos-1]
      # alpha_vector[pos] <- alpha_vector[pos-1]
      # criteff_vector[[pos]] <- criteff_vector[[pos-1]]
      # effect_vector[pos] <- effect_size
      break
    }else{
      criteff_vector[[pos]] <- pwr.t.test(n = sample_size[n], 
                                        power = 0.8, 
                                        sig.level = alpha_vector[pos], 
                                        alternative = "two.sided", 
                                        type = "two.sample")$d
    
    print(paste("N =",sample_size[n],"Alpha =", alpha, "Power = ", power, "d =", effect_sizes[d]))
    }
  }
  }
  }

df <- tibble(
  n = rep(sample_size, times = length(effect_sizes)), 
  crit_eff = criteff_vector, 
  alpha = alpha_vector, 
  beta = beta_vector, 
  effect = effect_vector, 
  type = "min_a_b"
) |> 
  filter(!is.na(alpha), 
           crit_eff > 0)


vector_len <- length(sample_size)*length(effect_sizes)
criteff_vector <- vector(length = vector_len)
beta_vector <- vector(length = vector_len)
alpha_vector <- vector(length = vector_len)
effect_vector <- vector(length = vector_len)

### Beta / 5 rule, stop when power is 1, to stop weird lines on graph
for(d in 1:length(effect_sizes)){
  
  effect_size <- effect_sizes[d]
  alpha <- 0.05
  for(n in 1:length(sample_size)){
    
    if(round(power, digits = 21) >= 1 | round(alpha, digits = 21) <= 0){
      break
    }else{
      # position in vector
      pos <- length(sample_size) * (d - 1) + n
      
      # find power
      power <- pwr.t.test(n = sample_size[n], 
                          d = effect_size,
                          sig.level = alpha, 
                          alternative = "two.sided", 
                          type = "two.sample")$power
      # find beta
      beta_vector[pos] <-  1 - power

      # Without Sequential Alpha, the graphs go beyond the effect / 2 (where they are meant to plateau)
      alpha_vector[pos] <- beta_vector[pos]/5
      # alpha <- beta_vector[pos]/5
      # alpha_vector[pos] <- alpha®
      
      effect_vector[pos] <- effect_size
      
      if(round(power, digits = 21) >= 1 | round(alpha, digits = 21) <= 0){
        break
      }else{
      criteff_vector[[pos]] <- pwr.t.test(n = sample_size[n], 
                                          power = 0.8, 
                                          sig.level = alpha_vector[pos], 
                                          alternative = "two.sided")$d
      
      print(paste("N =",sample_size[n],"Alpha =", alpha))
    }
  }
  }
}

df_2 <- tibble(
  n = rep(sample_size, times = length(effect_sizes)), 
  crit_eff = criteff_vector, 
  alpha = alpha_vector, 
  beta = beta_vector, 
  effect = effect_vector, 
  type = "beta_5"
) |>  
  filter(!is.na(alpha), 
         crit_eff > 0) |> 
  bind_rows(df)


## Plotting Alpha, Beta and Critical Effect Size
## See how the line flattens 
## Only when we use beta > some very small number, does not flatten when we use condition beta > 0, this is probably a slight bug in the code or an error with rounding

df_2|>
  ggplot(aes(x = n, y = crit_eff, group = effect, colour = effect))+
  # geom_point(alpha = 0.4, size = .5)+
  geom_line(size = 1)+
  geom_hline(aes(colour = effect, yintercept = effect/2))+
  scale_x_log10()+
  scale_y_continuous(limits = c(0, 2))+
  facet_grid(cols = vars(type))

# df_2|>
#   ggplot(aes(x = n, y = beta, group = effect, colour = effect))+
#   geom_point(alpha = 0.4)+
#   geom_line()+
#   scale_x_log10()+
#   facet_grid(cols = vars(type))
# 
# df_2|>
#   ggplot(aes(x = n, y = alpha, group = effect, colour = effect))+
#   geom_point(alpha = 0.4)+
#   geom_line()+
#   scale_x_log10()+
#   facet_grid(cols = vars(type))

#----------------------------------------------------------
# Method 2 - using Rmpfr package to increase precision - to prevent R from rounding numbers to 1 or 0

library(tidyverse)
library(pwr)
library(Rmpfr)
library(parallel)
library(doParallel)

doParallel::registerDoParallel(cores = parallel::detectCores() - 2)

options(digits = 7)

precison <- 200

sample_size <- seq(5, 10000, 5)
effect_sizes <-  seq(0.05, 0.5, 0.05)
# effect_sizes <-  .4

ss_long <- rep(sample_size, times = length(effect_sizes))
es_long <- rep(effect_sizes, each = length(sample_size))

alpha <-  0.05

matrix <- mpfrArray(NA, precBits = precison, dim = c(length(ss_long), 6))
# criteff_vector <- vector(length = vector_len)
# beta_vector <- vector(length = vector_len)
# alpha_vector <- vector(length = vector_len)
# effect_vector <- vector(length = vector_len)
# col 1 - sample size
# col 2 - alpha
# col 3 - beta
# col 4 - power
# col 5 - effect_size (predicted)
# col 6 - critical effect size

## find power for various sample sizes
power <- Rmpfr::mpfr( 
  pwr::pwr.t.test(
    n = ss_long, 
    d = es_long, 
    sig.level = alpha, 
    type = "two.sample", 
    alternative = "two.sided")$power, 
  precBits = precison
)
# some powers are greater than one or equal to one (R is the rounding/ We need beta to be greater than zero), find closest value to one then replace them with it
largest_power <- sort(power[power > 1-1e-14 & power<1], decreasing = T)[1]

power[power>=1] <- largest_power

# find beta 
beta <- 1 - power

# create alpha vector
alpha_vec <- mpfrArray(alpha, dim = length(beta), precBits = precison)
# input beta when beta is smaller than alpha
alpha_vec[beta < alpha] <- beta[beta < alpha] 
matrix[,6] <- foreach::foreach(n = 1:length(ss_long), .combine = c) %dopar% {
  
  Rmpfr::mpfr( 
    pwr::pwr.t.test(
      n = ss_long[[n]], 
      power = 0.95, 
      sig.level = as.numeric(alpha_vec)[[n]], 
      type = "two.sample", 
      alternative = "two.sided")$d, 
    precBits = precison
  )
}

## finalise matrix
# matrix[,1] <- ss_long
# matrix[,2] <- alpha_vec
# matrix[,3] <- beta
# matrix[,4] <- power
# matrix[,5] <- es_long

dyn_df <- tibble(
  n = ss_long, 
  alpha = as.numeric(alpha_vec),
  beta = as.numeric(beta),
  power = as.numeric(power),
  eff_size = as.numeric(es_long),
  crit_eff = as.numeric(matrix[,6])
) # crit effect

dyn_crit1 <- dyn_df |> 
  ggplot(aes(x = n, y = crit_eff, group = eff_size, colour = eff_size))+
  geom_point(alpha = .2)+
  geom_line(alpha = .2, linewidth = 1)+
  scale_x_log10("Sample Size")+
  scale_y_continuous("Cohen's d", breaks = seq(0, 2, 0.5), limits = c(0, 2))+
  scale_colour_gradient("Predicted Effect Size", 
                        guide = guide_colourbar(barwidth = 15, barheight = 0.8, title.vjust = 1))+
  labs(title = "Critical Effect Sizes vs Sample Sizes with Dynamic Alpha Rule")+
  theme_project_light(base_size = 13)+
  theme(panel.grid.major.y = element_line())

dyn_crit2 <- dyn_df |>
  filter(!alpha < 1e-10) |> 
  ggplot(aes(x = n, y = crit_eff, group = eff_size, colour = eff_size))+
  geom_point(alpha = .2)+
  geom_line(alpha = .2, linewidth = 1)+
  scale_x_log10("Sample Size")+
  scale_y_continuous("Cohen's d", breaks = seq(0, 2, 0.5), limits = c(0, 2))+
  scale_colour_gradient("Predicted Effect Size", 
                        guide = guide_colourbar(barwidth = 15, barheight = 0.8, title.vjust = 1))+
  labs(title = "Critical Effect Sizes vs Sample Sizes with Dynamic Alpha Rule")+
  theme_project_light(base_size = 13)+
  theme(panel.grid.major.y = element_line())

## artificially putting in where line should be
## the 0.98*crit_eff allows the graph to have a neat look, 
## although it is technically incorrect, as the plateau should be at crit_eff,
## or following Free Lunch Paper - 1/2 of Predicted Effect - Which I don't observe
dyn_crit3 <- dyn_df |>
  mutate(crit_eff = if_else(
    alpha < 1e-10, 0.98*eff_size, crit_eff)
    ) |> 
  ggplot(aes(x = n, y = crit_eff, group = eff_size, colour = eff_size))+
  geom_point(alpha = .2)+
  geom_line(alpha = .2, linewidth = 1)+
  scale_x_log10("Sample Size")+
  scale_y_continuous("Cohen's d", breaks = seq(0, 2, 0.5), limits = c(0, 2))+
  scale_colour_gradient("Predicted Effect Size", 
                        guide = guide_colourbar(barwidth = 15, barheight = 0.8, title.vjust = 1))+
  labs(title = "Critical Effect Sizes vs Sample Sizes with Dynamic Alpha Rule")+
  theme_project_light(base_size = 13)+
  theme(panel.grid.major.y = element_line())

ggsave(filename = here::here("images", "ch1", "dyn_crit.png"), plot = dyn_crit1, width = 10, height = 8, dpi = 360) 
ggsave(filename = here::here("images", "ch1", "dyn_crit_tidy.png"), plot = dyn_crit2, width = 10, height = 8, dpi = 360) 
ggsave(filename = here::here("images", "ch1", "dyn_crit_fake.png"), plot = dyn_crit3, width = 10, height = 8, dpi = 360) 

# # beta
# dyn_df |>
#   ggplot(aes(x = n, y = beta, colour = eff_size))+
#   geom_point()+
#   scale_x_log10()
# # alpha
# dyn_df |>
#   ggplot(aes(x = n, y = alpha, colour = eff_size))+
#   geom_point()+
#   scale_x_log10()
# # # power
# dyn_df |> 
#   ggplot(aes(x = n, y = power, colour = eff_size))+
#   geom_point()+
#   scale_x_log10()
#   scale_x_log10()



my_power.t.test <- function (
    n = NULL, 
    delta = NULL, 
    sd = 1, 
    sig.level = 0.05, 
    power = NULL,
    type = c("two.sample", "one.sample", "paired"),
    alternative = c("two.sided", "one.sided"), 
    strict = FALSE, 
    null_ncp = 0, 
    tol = .Machine$double.eps^0.25)
{
  if (sum(vapply(list(n, delta, sd, power, sig.level), is.null,
                 NA)) != 1)
    stop("exactly one of 'n', 'delta', 'sd', 'power', and 'sig.level' must be NULL")
  # assert_NULL_or_prob(sig.level)
  # assert_NULL_or_prob(power)
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
  force(tsample)
  tside <- switch(alternative, one.sided = 1, two.sided = 2)
  if (tside == 2 && !is.null(delta))
    delta <- abs(delta)
  p.body <- if (strict && tside == 2)
    quote({
      nu <- pmax(1e-07, n - 1) * tsample
      qu <- qt(sig.level/tside, ncp = sqrt(n/tsample) * null_ncp/sd, nu, lower.tail = FALSE)
      pt(qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = FALSE) +
        pt(-qu, nu, ncp = sqrt(n/tsample) * delta/sd,
           lower.tail = TRUE)
    })
  else quote({
    nu <- pmax(1e-07, n - 1) * tsample
    pt(qt(sig.level/tside, nu, lower.tail = FALSE), nu, ncp = sqrt(n/tsample) *
         delta/sd, lower.tail = FALSE)
  })
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07),
                 tol = tol, extendInt = "upX")$root
  else if (is.null(sd))
    sd <- uniroot(function(sd) eval(p.body) - power, delta *
                    c(1e-07, 1e+07), tol = tol, extendInt = "downX")$root
  else if (is.null(delta))
    delta <- uniroot(function(delta) eval(p.body) - power,
                     sd * c(1e-07, 1e+07), tol = tol, extendInt = "upX")$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) - power, 
                          interval = c(1e-10, 1 - 1e-10), 
                          tol = tol, extendInt = "yes")$root
  else stop("internal error", domain = NA)
  NOTE <- switch(type, paired = "n is number of *pairs*, sd is std.dev. of *differences* within pairs",
                 two.sample = "n is number in *each* group", NULL)
  METHOD <- paste(switch(type, one.sample = "One-sample", two.sample = "Two-sample",
                         paired = "Paired"), "t test power calculation")
  structure(list(n = n, delta = delta, sd = sd, sig.level = sig.level,
                 power = power, alternative = alternative, note = NOTE,
                 method = METHOD), class = "power.htest")
}

library(tidyverse)
library(pwr)
library(parallel)
library(doParallel)

doParallel::registerDoParallel(cores = parallel::detectCores() - 2)

sample_size <- c(c(2:19), seq(20, 1000, 10), seq(1100, 100000, 100), seq(101000, 10000000, 1000))
effect_sizes <-  c(0.1, 0.2, 0.3, 0.4, .5)
null_ncp <- seq(0, .5, .1)
alpha <-  0.05
crit_eff <- list()



for(ncp in 1:length(null_ncp)){

crit_eff[[ncp]] <- foreach::foreach(s = 1:length(sample_size), .combine = "c")%dopar%{
  my_power.t.test(n = sample_size[[s]], 
                  delta = NULL, 
                  sd = 1, 
                  sig.level = alpha, 
                  power = 0.8, 
                  type = "two.sample", 
                  alternative = "two.sided",
                  strict = TRUE, 
                  null_ncp = null_ncp[[ncp]])$delta
}
  
}

df <- tibble(
  sample = rep(sample_size, times = length(null_ncp)),
  ncp = rep(null_ncp, each = length(sample_size)),
  crit_eff = crit_eff |> unlist()
)


df |> 
  ggplot(aes(x = sample, y = crit_eff, colour = ncp, group = ncp))+
  geom_line(size = 1)+
  scale_x_log10()+
  scale_y_continuous(limits = c(0, 2))+
  geom_hline(aes(yintercept= ncp), alpha = .5)+
  theme_minimal()

custom <- function(x, df = 2, ncp = 0) {dt(x = x, df = df, ncp = ncp)}


ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
  stat_function(fun = custom) +
  stat_function(fun = custom, args = list(df = 5)) +
  stat_function(fun = custom, args = list(df = 8)) +
  stat_function(fun = custom, args = list(df = 10)) +
  stat_function(fun = custom, args = list(df = 15)) +
  stat_function(fun = custom, args = list(df = 20)) +
  geom_vline(xintercept = optimise(f = custom, interval = c(0, 10), maximum = T)$maximum)+
  theme_minimal()

ggplot(data.frame(x = c(-4, 10)), aes(x = x)) +
  stat_function(fun = custom, args = list(df = 100)) +
  stat_function(fun = custom, args = list(df = 100, ncp = 1)) +
  stat_function(fun = custom, args = list(df = 100, ncp = 2)) +
  stat_function(fun = custom, args = list(df = 100, ncp = 3)) +
  stat_function(fun = custom, args = list(df = 100, ncp = 4)) +
  stat_function(fun = custom, args = list(df = 100, ncp = 5)) +
  # geom_vline(xintercept = optimise(f = custom, interval = c(0, 10), maximum = T)$maximum)+
  theme_minimal()

x = seq(-5, 25, 0.01)
degs = c(2, 5, 8, 10, 15, 20, 100, 1000)
ncps = seq(0, .5, .1)


t_df <- expand.grid(x, degs, ncps) |> 
  as_tibble(.name_repair = ~c("x", "deg", "ncp")) |> 
  mutate(
    prob = custom(x = x, df = deg, ncp = sqrt(deg/2) * ncp),
    crit = qt(p = 0.9725, df = deg, ncp = sqrt(deg/2) * ncp)
    )


t_df |> 
  filter(prob > 0.0001) |> 
  ggplot(aes(x = x, y = prob, colour = ncp, group = ncp))+
  geom_line(size = 1.2, alpha = .7)+
  geom_vline(aes(xintercept = crit, colour = ncp))+
  facet_wrap(~deg, scales = "free")+
  theme_minimal()

t_df |> 
  filter(ncp == 0) |> 
  ggplot(aes(x = x, y = prob, colour = deg, group = deg))+
  geom_line(size = .7, alpha = 1)+
  geom_vline(aes(xintercept = crit, colour = deg))+
  theme_minimal()


n_size <- 50
min_eff <- 0.3

t_value <- qt(1 - (0.05 /2), df = n_size - 2, ncp = sqrt(n_size / 2) * min_eff, lower.tail = T)
2*pt(t_value, df = n_size - 2, ncp = sqrt(n_size / 2) * min_eff, lower.tail = F)

2*pt(t_value, df = n_size - 2, ncp = 0, lower.tail = F)


reps <- 100

samples <- rep(c(c(2, 3, 5, 7, 9, seq(10, 200, 5))), times = reps)
diffs <- seq(0, -1, -0.1)
margins <- seq(0, 1, .1)

p_values <- list()


for(m in 1:length(margins)){
  for (d in 1:length(diffs)){
    i <- (length(diff) * (m - 1)) + d
    
    p_values[[i]] <- foreach::foreach(s = 1:length(samples))%dopar%{
        t.test(x = rnorm(samples[[s]], 0, 1),
                y = rnorm(samples[[s]], diffs[[d]], 1),
                alternative = "two.sided",
                var.equal = F,
                mu = margins[[m]])$p.value
    }
  }
}


p_df <- expand.grid(samples, diffs, margins) |> 
  as_tibble(.name_repair = ~ c("sample", "diff", "margin")) |> 
  mutate(
  p = p_values |> unlist() |>  as.vector()
) |> 
  group_by(sample, margin, diff) |> 
  mutate(
    fp = sum(p < 0.05)/n(), 
    n = n()) 

p_df |> 
  ggplot(aes(x = sample, y = fp, colour = diff, group = diff))+
  geom_line(size = .7, alpha = 1)+
  theme_minimal()+
  facet_wrap(~margin)


