## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----old-post, eval = FALSE---------------------------------------------------
# pbayespostpred1bin(
#   prob = 'posterior', design = 'controlled', theta0 = 0.05,
#   n_t = 10, n_c = 10, y_t = 7, y_c = 3,
#   a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
#   m_t = NULL, m_c = NULL, z = NULL,
#   ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
#   alpha0e_t = NULL, alpha0e_c = NULL,
#   lower.tail = FALSE
# )

## ----new-post-----------------------------------------------------------------
pbetadiff(0.05, a_t = 7.5, b_t = 3.5, a_c = 3.5, b_c = 7.5,
          lower.tail = FALSE)

des <- design_binary(
  n_t = 10, n_c = 10,
  prior_t = prior_beta(0.5, 0.5), prior_c = prior_beta(0.5, 0.5),
  criterion = criterion_posterior(theta_tv = 0.05, theta_mav = 0,
                                  gamma_go = 0.8, gamma_nogo = 0.8)
)
qdm_decide(des, data_t = data_binary(10, 7), data_c = data_binary(10, 3))

## ----old-oc, eval = FALSE-----------------------------------------------------
# pbayesdecisionprob1bin(
#   prob = 'posterior', design = 'controlled',
#   theta_TV = 0.30, theta_MAV = 0.10, theta_NULL = NULL,
#   gamma_go = 0.80, gamma_nogo = 0.20,
#   pi_t = seq(0.15, 0.9, l = 10), pi_c = rep(0.15, 10),
#   n_t = 10, n_c = 10,
#   a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
#   z = NULL, m_t = NULL, m_c = NULL,
#   ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
#   alpha0e_t = NULL, alpha0e_c = NULL
# )

## ----new-oc-------------------------------------------------------------------
des <- design_binary(
  n_t = 10, n_c = 10,
  prior_t = prior_beta(0.5, 0.5), prior_c = prior_beta(0.5, 0.5),
  criterion = criterion_posterior(theta_tv = 0.30, theta_mav = 0.10,
                                  gamma_go = 0.80, gamma_nogo = 0.20)
)
oc <- qdm_oc(des, data.frame(pi_t = seq(0.15, 0.9, length.out = 10),
                             pi_c = 0.15))
oc

## ----old-cal, eval = FALSE----------------------------------------------------
# getgamma1bin(
#   prob = 'posterior', design = 'controlled',
#   theta_TV = 0.30, theta_MAV = 0.10, theta_NULL = NULL,
#   pi_t_go = 0.15, pi_c_go = 0.15, pi_t_nogo = 0.35, pi_c_nogo = 0.15,
#   target_go = 0.05, target_nogo = 0.20,
#   n_t = 12L, n_c = 12L,
#   a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
#   z = NULL, m_t = NULL, m_c = NULL,
#   ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
#   alpha0e_t = NULL, alpha0e_c = NULL
# )

## ----new-cal------------------------------------------------------------------
des12 <- design_binary(
  n_t = 12, n_c = 12,
  prior_t = prior_beta(0.5, 0.5), prior_c = prior_beta(0.5, 0.5),
  criterion = criterion_posterior(theta_tv = 0.30, theta_mav = 0.10,
                                  gamma_go = 0.80, gamma_nogo = 0.20)
)
cal <- qdm_calibrate(des12,
                     scenario_go = data.frame(pi_t = 0.15, pi_c = 0.15),
                     scenario_nogo = data.frame(pi_t = 0.35, pi_c = 0.15),
                     target_go = 0.05, target_nogo = 0.20)
cal

## ----hypothetical-------------------------------------------------------------
design_binary(
  n_t = 12, n_c = 0,
  prior_t = prior_beta(0.5, 0.5), prior_c = prior_beta(0.5, 0.5),
  criterion = criterion_posterior(theta_tv = 0.30, theta_mav = 0.10,
                                  gamma_go = 0.80, gamma_nogo = 0.20),
  hypothetical_c = hypothetical(data = data_binary(n = 12, y = 2))
)

