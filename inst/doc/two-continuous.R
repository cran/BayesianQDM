## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----design-------------------------------------------------------------------
L0 <- matrix(c(80^2, 15, 15, 6^2), 2, 2)
crit <- criterion_posterior(theta_tv = c(10, 2), theta_mav = c(5, 1),
                            gamma_go = 0.6, gamma_nogo = 0.6)
des <- design_continuous2(
  n_t = 30, n_c = 30,
  prior_t = prior_niw(mu0 = c(0, 1), kappa0 = 0.1, nu0 = 4, Lambda0 = L0),
  prior_c = prior_niw(mu0 = c(-10, -1), kappa0 = 0.1, nu0 = 4,
                      Lambda0 = L0),
  criterion = crit
)
data_t <- data_continuous2(30, mean = c(12, 3),
                           cov = matrix(c(6400, 190, 190, 36), 2, 2))
data_c <- data_continuous2(30, mean = c(-10, -1),
                           cov = matrix(c(5600, 150, 150, 42), 2, 2))
dec <- qdm_decide(des, data_t = data_t, data_c = data_c)
dec

## ----mc-----------------------------------------------------------------------
des_mc <- design_continuous2(
  n_t = 30, n_c = 30,
  prior_t = prior_niw(mu0 = c(0, 1), kappa0 = 0.1, nu0 = 4, Lambda0 = L0),
  prior_c = prior_niw(mu0 = c(-10, -1), kappa0 = 0.1, nu0 = 4,
                      Lambda0 = L0),
  criterion = crit, method = "MC", nMC = 10000
)
dec_mc <- qdm_decide(des_mc, data_t = data_t, data_c = data_c, seed = 1)
rbind(MM = dec$prob_regions, MC = dec_mc$prob_regions)

## ----hypothetical-------------------------------------------------------------
des_hyp <- design_continuous2(
  n_t = 30, n_c = 0,
  prior_t = prior_niw(mu0 = c(0, 1), kappa0 = 0.1, nu0 = 4, Lambda0 = L0),
  criterion = crit,
  hypothetical_c = hypothetical(mean = c(-10, -1), r = 1)
)
dec_hyp <- qdm_decide(des_hyp, data_t = data_t)
dec_hyp

## ----fixed--------------------------------------------------------------------
des_fix <- design_continuous2(
  n_t = 30, n_c = 0,
  prior_t = prior_niw(mu0 = c(0, 1), kappa0 = 0.1, nu0 = 4, Lambda0 = L0),
  criterion = crit, fixed_c = c(-10, -1)
)
dec_fix <- qdm_decide(des_fix, data_t = data_t)
dec_fix

## ----oc-----------------------------------------------------------------------
effects <- expand.grid(theta1 = c(0, 10, 20), theta2 = c(0, 2, 4))
scen <- data.frame(mu_t1 = -10 + effects$theta1,
                   mu_t2 = -1 + effects$theta2, sd_t1 = 80, sd_t2 = 6,
                   rho_t = 0.3, mu_c1 = -10, mu_c2 = -1, sd_c1 = 80,
                   sd_c2 = 6, rho_c = 0.3)
oc <- qdm_oc(des, scen, nsim = 1000, seed = 1)
oc
plot(oc)

## ----oc-overlay---------------------------------------------------------------
eff_wide <- expand.grid(theta1 = seq(0, 40, by = 10),
                        theta2 = seq(0, 8, by = 2))
scen_wide <- data.frame(mu_t1 = -10 + eff_wide$theta1,
                        mu_t2 = -1 + eff_wide$theta2, sd_t1 = 80,
                        sd_t2 = 6, rho_t = 0.3, mu_c1 = -10, mu_c2 = -1,
                        sd_c1 = 80, sd_c2 = 6, rho_c = 0.3)
plot(qdm_oc(des, scen_wide, nsim = 1000, seed = 1), overlay = TRUE)

## ----boundary-table-----------------------------------------------------------
qdm_boundary(des, mean_t = list(c(0, 10, 20, 30), seq(-3, 7, by = 0.25)),
             cov_t = data_t$cov, data_c = data_c)

## ----boundary-----------------------------------------------------------------
b <- qdm_boundary(des, mean_t = list(seq(-30, 50, by = 2),
                                     seq(-3, 7, by = 0.25)),
                  cov_t = data_t$cov, data_c = data_c)
plot(b)

## ----calibrate----------------------------------------------------------------
cal <- qdm_calibrate(des, scenario_go = scen[1, ], scenario_nogo = scen[9, ],
                     target_go = 0.1, target_nogo = 0.2, nsim = 1000,
                     seed = 1)
cal
plot(cal)

