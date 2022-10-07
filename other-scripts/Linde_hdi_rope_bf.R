myargs = as.numeric(commandArgs(trailingOnly=TRUE))
print(myargs)
set.seed(123)
iii <- myargs
n_Sim <- 100 ##2500 is default

###########################################################################
###########################################################################
###########################################################################

# Code used for the simulation study Campbell and Gustafson (2021) "re:Linde" 
# based on the code by Maximilian Linde
# modified by Harlan Campbell harlan.campbell@stat.ubc.ca


###########################################################################
###########################################################################
###########################################################################



# description -------------------------------------------------------------

# The goal is to compare the classification performances of the TOST,
# HDI-ROPE, and BF procedures for various scenarios. More specifically, we
# determine the proportion/probability of predicting equivalence when two
# groups are truly equivalent or truly non-equivalent.

# Throughout, we mimic a two-conditions independent-samples experiment with
# a continuous outcome variable. Data values for each condition are drawn
# from Normal distributions, with an equal number of cases in both
# conditions. Moreover, we assume homoscedasticity in the population.

# The proportion/probability of predicting equivalence can be determined
# analytically for the TOST procedure. For this we use the formula and
# R code from Shieh (2016). Importantly, power is calculated through 
# numerical integration in the case of Shieh (2016). We improve upon this 
# by adapting Shieh's (2016) code and using proper integration. Estimates 
# for the proportion/probability of predicting equivalence will be 
# obtained through simulations for the HDI-ROPE and BF procedures.

isFALSE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

subset.array <- function (x, dim, i, drop = FALSE) 
{
  ndim <- length(dim(x))
  stopifnot(dim > 0, dim <= ndim)
  args <- rep.int(alist(i = ), ndim)
  args[[dim]] <- i
  do.call("[", c(alist(x), args, drop = drop))
}

# load packages and scripts -----------------------------------------------

library("baymedr")
library("reshape2")
library("dplyr")
library("miniCRAN")
library("tidyr")
library("abind")
library("VGAM")


# Load necessary script with functions from Gronau, Ly, & Wagenmakers,
# which is available at https://osf.io/bsp6z/.
#source("informedTtest_functions.R")


integrand_t <- function(delta, t, n, nu, mu.delta, gamma, kappa) {
  
  suppressWarnings(
    dt(x = t, df = nu, ncp = sqrt(n) * delta) *
      1 / gamma * dt( (delta - mu.delta) / gamma, df = kappa)
  )
  
}

posterior_t <- function(delta,
                        t,
                        n1,
                        n2 = NULL,
                        independentSamples = FALSE,
                        prior.location,
                        prior.scale,
                        prior.df,
                        rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.location
  gamma <- prior.scale
  kappa <- prior.df
  
  numerator <- suppressWarnings(
    dt(x = t, df = nu, ncp = sqrt(neff) * delta) *
      1 / gamma * dt( (delta - mu.delta) / gamma, df = kappa)
  )
  
  denominator <- integrate(integrand_t,
                           lower = -Inf, upper = Inf,
                           t = t, n = neff, nu = nu,
                           mu.delta = mu.delta,
                           gamma = gamma,
                           kappa = kappa,
                           rel.tol = rel.tol)$value
  
  out <- numerator / denominator
  out[is.na(out)] <- 0
  
  return(out)
  
}

cdf_t <- function(x,
                  t,
                  n1,
                  n2 = NULL,
                  independentSamples = FALSE,
                  prior.location,
                  prior.scale,
                  prior.df,
                  rel.tol = .Machine$double.eps^0.25) {
  
  out <- integrate(posterior_t,
                   lower = -Inf, upper = x,
                   t = t, n1 = n1, n2 = n2,
                   independentSamples = independentSamples,
                   prior.location = prior.location,
                   prior.scale = prior.scale,
                   prior.df = prior.df,
                   rel.tol = rel.tol)$value
  
  # catch numerical errors
  if (out > 1 & out < 1.001) {
    out <- 1
    warning(
      "Numerical integration yields a CDF value slightly larger than 1. The CDF value has been replaced by 1.",
      call. = FALSE
    )
  }
  
  return(out)
  
}

quantile_t <- function(q,
                       t,
                       n1,
                       n2 = NULL,
                       independentSamples = FALSE,
                       prior.location,
                       prior.scale,
                       prior.df,
                       tol = 0.0001,
                       max.iter = 100,
                       rel.tol = .Machine$double.eps^0.25) {
  
  # compute quantiles via Newton-Raphson method
  
  x.cur <- Inf
  # get reasonable starting value
  delta <- seq(-2, 2, length.out = 400)
  dens <- posterior_t(delta, t = t, n1 = n1, n2 = n2,
                      independentSamples = independentSamples,
                      prior.location = prior.location,
                      prior.scale = prior.scale,
                      prior.df = prior.df)
  x.new <- delta[which.max(dens)]
  i <- 1
  
  while (abs(x.cur - x.new) > tol && i < max.iter) {
    
    x.cur <- x.new
    x.new <- x.cur - (cdf_t(x.cur, t = t, n1 = n1, n2 = n2,
                            independentSamples = independentSamples,
                            prior.location = prior.location,
                            prior.scale = prior.scale,
                            prior.df = prior.df,
                            rel.tol = rel.tol) - q)/
      posterior_t(x.cur, t = t, n1 = n1, n2 = n2,
                  independentSamples = independentSamples,
                  prior.location = prior.location,
                  prior.scale = prior.scale,
                  prior.df = prior.df)
    i <- i + 1
    
  }
  
  return(x.new)
  
}

ciPlusMedian_t <- function(t,
                           n1,
                           n2 = NULL,
                           independentSamples = FALSE,
                           prior.location,
                           prior.scale,
                           prior.df,
                           ci = .95,
                           type = "two-sided",
                           tol = 0.0001,
                           max.iter = 100,
                           rel.tol = .Machine$double.eps^0.25) {
  
  lower <- (1 - ci)/2
  upper <- ci + (1 - ci)/2
  med <- .5
  
  postAreaSmaller0 <- cdf_t(x = 0, t = t, n1 = n1, n2 = n2,
                            independentSamples = independentSamples,
                            prior.location = prior.location,
                            prior.scale = prior.scale,
                            prior.df = prior.df,
                            rel.tol = rel.tol)
  
  if (type == "plus-sided") {
    
    lower <- postAreaSmaller0 + (1 - postAreaSmaller0)*lower
    upper <- postAreaSmaller0 + (1 - postAreaSmaller0)*upper
    med <- postAreaSmaller0 + (1 - postAreaSmaller0)*med
    
  } else if (type == "min-sided") {
    
    lower <- postAreaSmaller0*lower
    upper <- postAreaSmaller0*upper
    med <- postAreaSmaller0*med
    
  }
  
  ciLower <- quantile_t(lower, t = t, n1 = n1, n2 = n2,
                        independentSamples = independentSamples,
                        prior.location = prior.location,
                        prior.scale = prior.scale,
                        prior.df = prior.df,
                        rel.tol = rel.tol)
  ciUpper <- quantile_t(upper, t = t, n1 = n1, n2 = n2,
                        independentSamples = independentSamples,
                        prior.location = prior.location,
                        prior.scale = prior.scale,
                        prior.df = prior.df,
                        rel.tol = rel.tol)
  median <- quantile_t(med, t = t, n1 = n1, n2 = n2,
                       independentSamples = independentSamples,
                       prior.location = prior.location,
                       prior.scale = prior.scale,
                       prior.df = prior.df,
                       rel.tol = rel.tol)
  
  return(list(ciLower = ciLower, median = median, ciUpper = ciUpper))
  
}

posterior_normal <- function(delta,
                             t,
                             n1,
                             n2 = NULL,
                             independentSamples = FALSE,
                             prior.mean,
                             prior.variance) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.mean
  g <- prior.variance
  
  numerator <- dt(x = t, df = nu, ncp = sqrt(neff) * delta) *
    dnorm(x = delta, mean = mu.delta, sd = sqrt(g))
  
  denominator <- 1 / sqrt(1 + neff * g) *
    dt(x = t / sqrt(1 + neff * g),
       df = nu,
       ncp = sqrt(neff / (1 + neff * g)) * mu.delta)
  
  out <- numerator / denominator
  out[is.na(out)] <- 0
  
  return(out)
  
}

cdf_normal <- function(x,
                       t,
                       n1,
                       n2 = NULL,
                       independentSamples = FALSE,
                       prior.mean,
                       prior.variance,
                       rel.tol = .Machine$double.eps^0.25) {
  
  out <- integrate(posterior_normal, lower = -Inf, upper = x,
                   t = t, n1 = n1, n2 = n2,
                   independentSamples = independentSamples,
                   prior.mean = prior.mean,
                   prior.variance = prior.variance,
                   rel.tol = rel.tol)$value
  
  # catch numerical errors
  if (out > 1 & out < 1.001) {
    out <- 1
    warning(
      "Numerical integration yields a CDF value slightly larger than 1. The CDF value has been replaced by 1.",
      call. = FALSE
    )
  }
  
  return(out)
  
}

quantile_normal <- function(q,
                            t,
                            n1,
                            n2 = NULL,
                            independentSamples = FALSE,
                            prior.mean,
                            prior.variance,
                            tol = 0.0001,
                            max.iter = 100,
                            rel.tol = .Machine$double.eps^0.25) {
  
  # compute quantiles via Newton-Raphson method
  
  x.cur <- Inf
  # get reasonable start value
  delta <- seq(-2, 2, length.out = 400)
  dens <- posterior_normal(delta, t = t, n1 = n1, n2 = n2,
                           independentSamples = independentSamples,
                           prior.mean = prior.mean,
                           prior.variance = prior.variance)
  x.new <- delta[which.max(dens)]
  i <- 1
  
  while (abs(x.cur - x.new) > tol && i < max.iter) {
    
    x.cur <- x.new
    x.new <- x.cur - (cdf_normal(x.cur, t = t, n1 = n1, n2 = n2,
                                 independentSamples = independentSamples,
                                 prior.mean = prior.mean,
                                 prior.variance = prior.variance, 
                                 rel.tol = rel.tol) - q)/
      posterior_normal(x.cur, t = t, n1 = n1, n2 = n2,
                       independentSamples = independentSamples,
                       prior.mean = prior.mean,
                       prior.variance = prior.variance)
    i <- i + 1
    
  }
  
  return(x.new)
  
}

ciPlusMedian_normal <- function(t,
                                n1,
                                n2 = NULL,
                                independentSamples = FALSE,
                                prior.mean,
                                prior.variance,
                                ci = .95,
                                type = "two-sided",
                                tol = 0.0001,
                                max.iter = 100,
                                rel.tol = .Machine$double.eps^0.25) {
  
  lower <- (1 - ci)/2
  upper <- ci + (1 - ci)/2
  med <- .5
  
  postAreaSmaller0 <- cdf_normal(x = 0, t = t, n1 = n1, n2 = n2,
                                 independentSamples = independentSamples,
                                 prior.mean = prior.mean,
                                 prior.variance = prior.variance,
                                 rel.tol = rel.tol)
  
  if (type == "plus-sided") {
    
    lower <- postAreaSmaller0 + (1 - postAreaSmaller0)*lower
    upper <- postAreaSmaller0 + (1 - postAreaSmaller0)*upper
    med <- postAreaSmaller0 + (1 - postAreaSmaller0)*med
    
  } else if (type == "min-sided") {
    
    lower <- postAreaSmaller0*lower
    upper <- postAreaSmaller0*upper
    med <- postAreaSmaller0*med
    
  }
  
  ciLower <- quantile_normal(lower, t = t, n1 = n1, n2 = n2,
                             independentSamples = independentSamples,
                             prior.mean = prior.mean,
                             prior.variance = prior.variance,
                             rel.tol = rel.tol)
  ciUpper <- quantile_normal(upper, t = t, n1 = n1, n2 = n2,
                             independentSamples = independentSamples,
                             prior.mean = prior.mean,
                             prior.variance = prior.variance,
                             rel.tol = rel.tol)
  median <- quantile_normal(med, t = t, n1 = n1, n2 = n2,
                            independentSamples = independentSamples,
                            prior.mean = prior.mean,
                            prior.variance = prior.variance,
                            rel.tol = rel.tol)
  
  return(list(ciLower = ciLower, median = median, ciUpper = ciUpper))
  
}

bf10_t <- function(t,
                   n1,
                   n2 = NULL,
                   independentSamples = FALSE,
                   prior.location,
                   prior.scale,
                   prior.df,
                   rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.location
  gamma <- prior.scale
  kappa <- prior.df
  numerator <- integrate(integrand_t, lower = -Inf, upper = Inf,
                         t = t, n = neff, nu = nu,
                         mu.delta = mu.delta,
                         gamma = gamma,
                         kappa = kappa,
                         rel.tol = rel.tol)$value
  denominator <- dt(x = t, df = nu)
  
  BF10 <- numerator / denominator
  priorAreaSmaller0 <- pt(q = - mu.delta / gamma, df = kappa)
  postAreaSmaller0 <- cdf_t(x = 0, t = t, n1 = n1, n2 = n2,
                            independentSamples = independentSamples,
                            prior.location = prior.location,
                            prior.scale = prior.scale,
                            prior.df = prior.df,
                            rel.tol = rel.tol)
  BFmin1 <- postAreaSmaller0 / priorAreaSmaller0
  BFplus1 <- (1 - postAreaSmaller0) / (1 - priorAreaSmaller0)
  BFmin0 <- BFmin1 * BF10
  BFplus0 <- BFplus1 * BF10
  
  return(list(BF10 = BF10, BFplus0 = BFplus0, BFmin0 = BFmin0))
  
}

bf10_normal <- function(t,
                        n1,
                        n2 = NULL,
                        independentSamples = FALSE,
                        prior.mean,
                        prior.variance,
                        rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.mean
  g <- prior.variance
  numerator <- 1 / sqrt(1 + neff * g) *
    dt(x = t / sqrt(1 + neff * g),
       df = nu,
       ncp = sqrt(neff / (1 + neff * g)) * mu.delta)
  denominator <- dt(x = t, df = nu)
  
  BF10 <- numerator / denominator
  priorAreaSmaller0 <- pnorm(0, mean = prior.mean,
                             sd = sqrt(prior.variance))
  postAreaSmaller0 <- cdf_normal(x = 0, t = t, n1 = n1, n2 = n2,
                                 independentSamples = independentSamples,
                                 prior.mean = prior.mean,
                                 prior.variance = prior.variance,
                                 rel.tol = rel.tol)
  BFmin1 <- postAreaSmaller0 / priorAreaSmaller0
  BFplus1 <- (1 - postAreaSmaller0) / (1 - priorAreaSmaller0)
  BFmin0 <- BFmin1 * BF10
  BFplus0 <- BFplus1 * BF10
  
  return(list(BF10 = BF10, BFplus0 = BFplus0, BFmin0 = BFmin0))
  
}


# function for analytic TOST ----------------------------------------------

# The function tost() calculates the proportion/probability of predicting
# equivalence for the TOST procedure, given the sample size, the population
# effect size, the equivalence margin, and the type 1 error rate. This
# function originates from Shieh (2016), who used numerical integration, 
# but was adapted by Jorge N. Tendeiro, who used proper integration.
tost_power <- function (n1,     # sample size 1st group
                        n2,     # sample size 2nd group
                        alpha,  # type I error rate
                        margin, # equivalence bound
                        es,     # value in (-margin, margin)
                        sd) {   # population SD
  sd_sq <- sd^2
  df <- n1 + n2 - 2
  t_crit <- qt(p = 1 - alpha, df = df)
  n_fac <- 1 / n1 + 1 / n2
  var <- sd_sq * n_fac
  std <- sqrt(var)
  cu <- (df * margin^2) / (var * t_crit^2)
  epower <- integrate(
    f = function(x) {
      (pnorm((margin-es) / std - sqrt(x / df) * t_crit) - 
         pnorm((-margin-es) / std + sqrt(x / df) * t_crit)) * 
        dchisq(x, df)
    }, 
    lower = 0,
    upper = cu,
    rel.tol = 1e-12
  )$value
  return(epower)
}



# function for simulated HDI-ROPE -----------------------------------------

# The function hdi_rope() estimates the prediction (equivalence [1] vs.
# non-equivalence [0]) for the HDI-ROPE procedure for a certain scenario.
# The arguments of the function are "con" for the data of the control
# condition, "exp" for the data of the experimental condition, and "margin"
# for the margin of the equivalence interval. The argument "prior_scale"
# corresponds to the scale of the Cauchy prior. We did not
# create a separate function for the BF approach because we can use the
# equiv_bf() function from the baymedr R package 
# (Linde & van Ravenzwaaij, 2019).
hdi_rope <- function(con,
                     exp,
                     interval,
                     prior_scale,
                     interval_std = TRUE) {
  
  n_x <- length(con)
  n_y <- length(exp)
  mean_x <- mean(con)
  mean_y <- mean(exp)
  sd_x <- sd(con)
  sd_y <- sd(exp)
  s_P = sqrt((sd_x ^ 2 + sd_y ^ 2) / 2)
  se = (s_P*sqrt(1/n_x + 1/n_y))
  t_stat <- ( (mean_y - mean_x) / se)
  
  
  if(isFALSE(interval_std)){
    margin <- interval/s_P
  }
  
  if(interval_std){
    margin <- interval
  }
  
  HDI_ROPE_p <- function(z){
    
    the_diff <-   inside <- hdi <- NA
    tryCatch({ 
      hdi <- ciPlusMedian_t(t = t_stat,
                            n1 = n_x,
                            n2 = n_y,
                            independentSamples = TRUE,
                            prior.location = 0,
                            prior.scale = prior_scale,
                            prior.df = 1,
                            ci = z)
      
      lower_hdi <- hdi$ciLower
      upper_hdi <- hdi$ciUpper
      
      inside <- all((c(lower_hdi, upper_hdi) > -margin) & (c(lower_hdi, upper_hdi) < margin))
      the_diff <-  max(c(abs(lower_hdi), abs(upper_hdi))) - (margin) 
      
    }, error=function(e){})	
    
    
    return(list(diff=the_diff, inside=inside))
  }
  HDI_ROPE_diff<-function(z){HDI_ROPE_p(z)$diff}
  
  p1 <- inside999 <- inside0001 <- NA
  
  inside999 <- HDI_ROPE_p(0.999)$inside
  inside0001 <- HDI_ROPE_p(0.0001)$inside
  
  if(!is.na(inside999)){ 
    if(inside999){	p1 <- 0.001}
    
    if(!is.na(inside0001)){ 
      if(!inside999 & !inside0001){	p1 <- 1}
    }
  }
  
  if(is.na(p1)){
    tryCatch({
      p1 <- 1-uniroot(HDI_ROPE_diff, c(0, 0.999), tol=0.0001)$root
    }, error=function(e){})
  }
  
  return(p1)
}



# function for simulated optim-equiv -----------------------------------------

optim_equiv <- function(con,
                        exp,
                        interval) {
  
  margin <- interval
  
  thesample <- c(con, exp)
  n1 <- length(con)
  n2 <- length(exp)
  trt <- c(rep(0, n1), rep(1, n2))
  delta <- margin
  
  s2_1 <- sd(thesample[trt==0])^2
  s2_2 <- sd(thesample[trt==1])^2
  s_P = sqrt(( ((n1 - 1) * s2_1) + ((n2 - 1) * s2_2) )/(n1 + n2 - 2))
  
  xbar1 <- mean(thesample[trt==0])
  xbar2 <- mean(thesample[trt==1])
  se.diff <- (s_P*sqrt(1/n1 + 1/n2))
  t_1 <- (xbar1-xbar2 - (-delta))/se.diff
  t_2 <- (xbar1-xbar2 - (delta))/se.diff
  
  pval1 <- 1-pt(t_1, n1+n2-2)
  pval2 <- 1-pt((t_2), n1+n2-2, lower=FALSE)
  equiv_pval <- max(c(pval1, pval2))
  
  optimal_equiv <- function(x){abs(xbar1 - xbar2) - qfoldnorm(x, delta, se.diff, lower.tail=TRUE)}
  
  
  optim_pval <- NA
  if(round(optimal_equiv(0.0001),6)==0){optim_pval <- 0}
  if(round(optimal_equiv(0.9999),6)==0){optim_pval <- 1}
  
  if(is.na(optim_pval)){
    tryCatch({optim_pval <- uniroot(optimal_equiv, c(0, (1-1/10e15)), tol=0.0001)$root
    }, error=function(e){})}
  
  
  return(c(optim_pval))}

### TOST ###

tost_equiv <- function(con, exp, interval) {
  
  margin <- abs(interval)
  
  n1 <- length(con); n2 <- length(exp)
  s2_1 <- sd(con)^2; s2_2 <- sd(exp)^2
  s_P = sqrt(( ((n1 - 1) * s2_1) + 
                 ((n2 - 1) * s2_2) )/(n1 + n2 - 2))
  xbar1 <- mean(con); xbar2 <- mean(exp)
  se.diff <- (s_P*sqrt(1/n1 + 1/n2))
  
  t_1 <- (xbar1 - xbar2 - (-margin))/se.diff
  t_2 <- (xbar1 - xbar2 - (margin))/se.diff
  pval1 <- 1 - pt(t_1, n1+n2-2)
  pval2 <- 1 - pt(t_2, n1+n2-2, lower=FALSE)
  tost_pval <- max(c(pval1, pval2))
  
  #t_abs <- (abs(xbar1 - xbar2)/se.diff  - abs(margin)/se.diff )
  #tost_pval <- 1 - pt(t_abs, n1+n2-2, lower=FALSE)
  
  
  return(tost_pval)}



sim_study <- function(margin, n, scale, true_es, nSim){
  
  n_rep <- nSim
  # create empty arrays -----------------------------------------------------
  
  # Create empty arrays that are later filled with the decisions of the
  # TOST, HDI-ROPE, and BF procedures.
  
  
  res_df0 <- data.frame(expand.grid(margin, n, true_es, scale, c(1:n_rep)))
  
  colnames(res_df0)<-c("margin", "n", "true_es", "scale", "rep_id")
  res_df <- data.frame(res_df0, 
                       "equiv"=rep(0,dim(res_df0)[1]), 
                       "hdi"=rep(0,dim(res_df0)[1]), 
                       "bf"=rep(0,dim(res_df0)[1]), 
                       "tost"=rep(0,dim(res_df0)[1]))
  
  
  
  # analytic solution for TOST and simulations for HDI-ROPE and BF ---------
  
  # Loop over the equivalence margin, the sample size within each condition,
  # and the true population effect size. Calculate the
  # proportion/probability of predicting equivalence for the TOST procedure
  # based on these three parameters. Further, loop over the number of
  # simulation repetitions. For each repetition we generate data for the
  # control and experimental conditions, drawn from Normal distributions.
  # The HDI-ROPE and the BF approaches use three different Cauchy prior
  # scales on these data. Included is a counter which roughly indicates the
  # status of the calculations. This counter is printed in the console.
  for(ee in 1:dim(res_df)[1]){
    cat(paste0("\n",
               "-----margin=", res_df[ee,"margin"],
               "-----n=", res_df[ee,"n"],
               "-----true_es=", res_df[ee,"true_es"],
               "\n"))
    
    
    con <- rnorm(n = res_df[ee,"n"],
                 mean = 0,
                 sd = 1)
    exp <- rnorm(n = res_df[ee,"n"],
                 mean = res_df[ee,"true_es"],
                 sd = 1)
    
    res_df[ee,"equiv"] <- optim_equiv(
      con = con,
      exp = exp,
      interval = res_df[ee,"margin"]
    )
    
    res_df[ee,"tost"] <- tost_equiv(
      con = con,
      exp = exp,
      interval = res_df[ee,"margin"]
    )                     
    
    res_df[ee, "hdi"] <- hdi_rope(
      con = con,
      exp = exp,
      interval = res_df[ee,"margin"],
      prior_scale = res_df[ee,"scale"],
      interval_std = FALSE
    )
    
    res_df[ee, "bf"] <- get_bf(equiv_bf(
      x = con,
      y = exp,
      interval = res_df[ee,"margin"],
      prior_scale = res_df[ee,"scale"],
      interval_std = FALSE
    ))
    
    if(runif(1)>0.98){
      print(ee)
      print( res_df[ee, ])}
    
  }
  
  
  return(res_df)
}
######### ######### ######### ######### ######### #########


lvls <- list()

lvls[[1]] <- c(0.1, 0.2, 0.3)	# margin
lvls[[2]] <- c(50, 100, 250, 500)	# n
lvls[[3]] <- seq(from = 0, to = 0.5, by = 0.1)	# true_es
lvls[[4]] <- c(0.5 / sqrt(2), 1 / sqrt(2), 2 / sqrt(2))	# scale

dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]]))

colnames(dsgn)<-c("margin", "n", "true_es", "scale")

dim(dsgn)

results_list <- list()

print(c("iii", iii))

ls()

results_list <- sim_study(
  margin = dsgn[iii, 1], 
  n = dsgn[iii, 2], 
  true_es = dsgn[iii, 3],
  scale = dsgn[iii, 4], 
  nSim = n_Sim)

ls()

return_list <- results_list

saveRDS(return_list, file=paste(paste(
  "linde_nonstd", iii, sep="_"),".rds",sep=""))

warnings()

