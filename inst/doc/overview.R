## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.path = "figures/overview-"
)
library(BayesianQDM)

## ----function-table, echo = FALSE---------------------------------------------
knitr::kable(data.frame(
  Function = c(
    "pbayespostpred1bin()", "pbayespostpred1cont()",
    "pbayespostpred2bin()", "pbayespostpred2cont()",
    "pbayesdecisionprob1bin()", "pbayesdecisionprob1cont()",
    "pbayesdecisionprob2bin()", "pbayesdecisionprob2cont()",
    "getgamma1bin()", "getgamma1cont()",
    "getgamma2bin()", "getgamma2cont()",
    "pbetadiff()", "pbetabinomdiff()",
    "ptdiff_NI()", "ptdiff_MC()", "ptdiff_MM()",
    "rdirichlet()",
    "getjointbin()", "allmultinom()"
  ),
  Description = c(
    "Posterior/predictive probability, single binary endpoint",
    "Posterior/predictive probability, single continuous endpoint",
    "Region probabilities, two binary endpoints",
    "Region probabilities, two continuous endpoints",
    "Go/NoGo/Gray decision probabilities, single binary endpoint",
    "Go/NoGo/Gray decision probabilities, single continuous endpoint",
    "Go/NoGo/Gray decision probabilities, two binary endpoints",
    "Go/NoGo/Gray decision probabilities, two continuous endpoints",
    "Optimal Go/NoGo thresholds, single binary endpoint",
    "Optimal Go/NoGo thresholds, single continuous endpoint",
    "Optimal Go/NoGo thresholds, two binary endpoints",
    "Optimal Go/NoGo thresholds, two continuous endpoints",
    "CDF of difference of two Beta distributions",
    "Beta-binomial posterior predictive probability",
    "CDF of difference of two t-distributions (numerical integration)",
    "CDF of difference of two t-distributions (Monte Carlo)",
    "CDF of difference of two t-distributions (moment-matching)",
    "Random sampler for the Dirichlet distribution",
    "Joint binary probability from marginals and correlation",
    "Enumerate all multinomial outcome combinations"
  )
), col.names = c("Function", "Description"))

## ----qs-bin-post--------------------------------------------------------------
# P(pi_treat - pi_ctrl > 0.05 | data)
# Observed: 7/10 responders (treatment), 3/10 (control)
pbayespostpred1bin(
  prob = 'posterior', design = 'controlled', theta0 = 0.05,
  n_t = 10, n_c = 10, y_t = 7, y_c = 3,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = NULL, m_c = NULL, z = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  lower.tail = FALSE
)

## ----qs-cont-post-------------------------------------------------------------
# P(mu_treat - mu_ctrl > 1.0 | data) using MM method
# Observed: mean 3.5 (treatment), 1.2 (control); SD ~ 2.0
pbayespostpred1cont(
  prob = 'posterior', design = 'controlled', prior = 'vague',
  CalcMethod = 'MM', theta0 = 1.0, nMC = NULL,
  n_t = 10, n_c = 10,
  bar_y_t = 3.5, s_t = 2.0,
  bar_y_c = 1.2, s_c = 2.0,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL
)

## ----qs-oc, fig.width = 8, fig.height = 6-------------------------------------
# Operating characteristics for single binary endpoint
oc_res <- pbayesdecisionprob1bin(
  prob      = 'posterior',
  design    = 'controlled',
  theta_TV  = 0.30,  theta_MAV = 0.10,  theta_NULL = NULL,
  gamma_go  = 0.80,  gamma_nogo = 0.20,
  pi_t      = seq(0.15, 0.9, l = 10),
  pi_c      = rep(0.15, 10),
  n_t = 10,  n_c = 10,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  z = NULL, m_t = NULL, m_c = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL
)
print(oc_res)
plot(oc_res, base_size = 20)

## ----qs-gamma, fig.width = 8, fig.height = 6----------------------------------
# Find gamma_go  : smallest gamma s.t. Pr(Go)   < 0.05 under Null (pi_t = pi_c = 0.15)
# Find gamma_nogo: smallest gamma s.t. Pr(NoGo) < 0.20 under Alt  (pi_t = 0.35, pi_c = 0.15)
gg_res <- getgamma1bin(
  prob = 'posterior', design = 'controlled',
  theta_TV = 0.30, theta_MAV = 0.10, theta_NULL = NULL,
  pi_t_go = 0.15, pi_c_go = 0.15,
  pi_t_nogo = 0.35, pi_c_nogo = 0.15,
  target_go = 0.05, target_nogo = 0.20,
  n_t = 12L, n_c = 12L,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  z = NULL, m_t = NULL, m_c = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL
)
plot(gg_res, base_size = 20)

