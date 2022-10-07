## optim_equiv: a function for two-sample equivalence
## testing. Produces both TOST p-val and optimal test p-val
optim_equiv <- function(sample1, sample2, margin) {
  require("VGAM")
  n1 <- length(sample1); n2 <- length(sample2)
  s2_1 <- sd(sample1)^2; s2_2 <- sd(sample2)^2
  s_P = sqrt(( ((n1 - 1) * s2_1) +
                 ((n2 - 1) * s2_2) )/(n1 + n2 - 2))
  xbar1 <- mean(sample1); xbar2 <- mean(sample2)
  se.diff <- (s_P*sqrt(1/n1 + 1/n2))
  t_1 <- (xbar1 - xbar2 - (-margin))/se.diff
  t_2 <- (xbar1 - xbar2 - (margin))/se.diff
  pval1 <- 1 - pt(t_1, n1 + n2 - 2)
  pval2 <- 1 - pt(t_2, n1 + n2 - 2, lower = FALSE)
  tost_pval <- max(c(pval1, pval2))
  optimal_equiv <- function(x){ abs(xbar1 - xbar2) - qfoldnorm(x, margin, se.diff) }
  optim_pval <- NA
  if(is.na(optim_pval)){
    tryCatch({optim_pval <- uniroot(optimal_equiv,
                                    c(0, (1 - 1/10e15)), tol = 0.0001)$root
    }, error=function(e){})}
  return(c(tost = tost_pval, optim = optim_pval))}
# Examples:
set.seed(123)
optim_equiv(rnorm(100), rnorm(260), margin = 0.4)
# tost optim
# 0.003542515 0.003349803
optim_equiv(rnorm(40), rnorm(26), margin = 0.4)
# tost optim
# 0.05371685 0.01259863


