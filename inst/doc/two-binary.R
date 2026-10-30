## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----regions------------------------------------------------------------------
qdm_regions("posterior")

## ----regions-pred-------------------------------------------------------------
qdm_regions("predictive")

## ----design-------------------------------------------------------------------
prior <- prior_dirichlet(rep(0.25, 4))
des <- design_binary2(
  n_t = 10, n_c = 10, prior_t = prior, prior_c = prior,
  criterion = criterion_posterior(theta_tv = c(0.15, 0.15),
                                  theta_mav = c(0.10, 0.10),
                                  gamma_go = 0.6, gamma_nogo = 0.6),
  nMC = 2000
)
dec <- qdm_decide(des, data_t = data_binary2(c(3, 2, 2, 3)),
                  data_c = data_binary2(c(6, 2, 1, 1)), seed = 1)
dec

## ----and----------------------------------------------------------------------
des_and <- design_binary2(
  n_t = 10, n_c = 10, prior_t = prior, prior_c = prior,
  criterion = criterion_posterior(theta_tv = c(0.15, 0.15),
                                  theta_mav = c(0.10, 0.10),
                                  gamma_go = 0.6, gamma_nogo = 0.6,
                                  rule = "and"),
  nMC = 2000
)
qdm_decide(des_and, data_t = data_binary2(c(3, 2, 2, 3)),
           data_c = data_binary2(c(6, 2, 1, 1)), seed = 1)

## ----predictive---------------------------------------------------------------
des_pred <- design_binary2(
  n_t = 10, n_c = 10, prior_t = prior, prior_c = prior,
  criterion = criterion_predictive(theta_null = c(0, 0), m_t = 30,
                                   gamma_go = 0.6, gamma_nogo = 0.6)
)
qdm_decide(des_pred, data_t = data_binary2(c(3, 2, 2, 3)),
           data_c = data_binary2(c(6, 2, 1, 1)))

## ----external-----------------------------------------------------------------
des_ext <- design_binary2(
  n_t = 10, n_c = 10, prior_t = prior, prior_c = prior,
  criterion = criterion_posterior(theta_tv = c(0.15, 0.15),
                                  theta_mav = c(0.10, 0.10),
                                  gamma_go = 0.6, gamma_nogo = 0.6),
  external_c = borrow(data_binary2(c(12, 3, 3, 2)), a0 = 0.5),
  nMC = 2000
)
qdm_decide(des_ext, data_t = data_binary2(c(3, 2, 2, 3)),
           data_c = data_binary2(c(6, 2, 1, 1)), seed = 1)$posterior_c

## ----hypothetical-------------------------------------------------------------
des_hyp <- design_binary2(
  n_t = 10, n_c = 0, prior_t = prior, prior_c = prior,
  criterion = criterion_posterior(theta_tv = c(0.15, 0.15),
                                  theta_mav = c(0.10, 0.10),
                                  gamma_go = 0.6, gamma_nogo = 0.6),
  hypothetical_c = hypothetical(data = data_binary2(c(6, 2, 1, 1))),
  nMC = 2000
)
qdm_decide(des_hyp, data_t = data_binary2(c(3, 2, 2, 3)), seed = 1)

## ----fixed--------------------------------------------------------------------
des_fix <- design_binary2(
  n_t = 10, n_c = 0, prior_t = prior,
  criterion = criterion_posterior(theta_tv = c(0.15, 0.15),
                                  theta_mav = c(0.10, 0.10),
                                  gamma_go = 0.6, gamma_nogo = 0.6),
  fixed_c = c(0.2, 0.2), nMC = 2000
)
qdm_decide(des_fix, data_t = data_binary2(c(3, 2, 2, 3)), seed = 1)

## ----regions-safety-----------------------------------------------------------
qdm_regions("posterior", direction = c("greater", "less"))

## ----safety-------------------------------------------------------------------
des_safety <- design_binary2(
  n_t = 10, n_c = 10, prior_t = prior, prior_c = prior,
  criterion = criterion_posterior(theta_tv = c(0.15, 0.1),
                                  theta_mav = c(0.10, 0.2),
                                  gamma_go = 0.5, gamma_nogo = 0.6,
                                  direction = c("greater", "less")),
  nMC = 2000
)
qdm_decide(des_safety, data_t = data_binary2(c(2, 1, 5, 2)),
           data_c = data_binary2(c(6, 1, 2, 1)), seed = 1)

## ----cellprob-----------------------------------------------------------------
cellprob_binary2(pi1 = 0.35, pi2 = 0.35, rho = 0.3)

## ----oc-----------------------------------------------------------------------
effects <- expand.grid(theta1 = c(0, 0.15, 0.3), theta2 = c(0, 0.15, 0.3))
scen <- data.frame(pi_t1 = 0.2 + effects$theta1,
                   pi_t2 = 0.2 + effects$theta2, rho_t = 0.3,
                   pi_c1 = 0.2, pi_c2 = 0.2, rho_c = 0.3)
oc <- qdm_oc(des, scen, seed = 1)
oc
plot(oc)

## ----oc-overlay---------------------------------------------------------------
eff_wide <- expand.grid(theta1 = seq(0, 0.5, by = 0.1),
                        theta2 = seq(0, 0.5, by = 0.1))
scen_wide <- data.frame(pi_t1 = 0.2 + eff_wide$theta1,
                        pi_t2 = 0.2 + eff_wide$theta2, rho_t = 0.3,
                        pi_c1 = 0.2, pi_c2 = 0.2, rho_c = 0.3)
oc_wide <- qdm_oc(des, scen_wide, seed = 1)
plot(oc_wide, overlay = TRUE)
plot(oc_wide, overlay = TRUE, cell_values = FALSE)

## ----calibrate----------------------------------------------------------------
cal <- qdm_calibrate(des, scenario_go = scen[1, ], scenario_nogo = scen[9, ],
                     target_go = 0.1, target_nogo = 0.2, seed = 1)
cal
plot(cal)

