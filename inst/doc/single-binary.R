## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----design-------------------------------------------------------------------
jeffreys <- prior_beta(0.5, 0.5)
crit <- criterion_posterior(theta_tv = 0.2, theta_mav = 0.1,
                            gamma_go = 0.5, gamma_nogo = 0.8)
des <- design_binary(n_t = 12, n_c = 12, prior_t = jeffreys,
                     prior_c = jeffreys, criterion = crit)
dec <- qdm_decide(des, data_t = data_binary(12, 9),
                  data_c = data_binary(12, 3))
dec

## ----pbetadiff----------------------------------------------------------------
post_t <- dec$posterior_t
post_c <- dec$posterior_c
pbetadiff(0.2, a_t = post_t[["a"]], b_t = post_t[["b"]],
          a_c = post_c[["a"]], b_c = post_c[["b"]], lower.tail = FALSE)
pbetadiff(0.1, a_t = post_t[["a"]], b_t = post_t[["b"]],
          a_c = post_c[["a"]], b_c = post_c[["b"]])

## ----external-----------------------------------------------------------------
des_ext <- design_binary(
  n_t = 12, n_c = 12, prior_t = jeffreys, prior_c = jeffreys,
  criterion = crit,
  external_c = borrow(data_binary(n = 20, y = 5), a0 = 0.5)
)
dec_ext <- qdm_decide(des_ext, data_t = data_binary(12, 9),
                      data_c = data_binary(12, 3))
dec_ext$posterior_c

## ----external-only------------------------------------------------------------
des_ext0 <- design_binary(
  n_t = 12, n_c = 0, prior_t = jeffreys, prior_c = jeffreys,
  criterion = crit,
  external_c = borrow(data_binary(n = 40, y = 10), a0 = 1)
)
qdm_decide(des_ext0, data_t = data_binary(12, 9))

## ----hypothetical-------------------------------------------------------------
des_hyp <- design_binary(
  n_t = 12, n_c = 0, prior_t = jeffreys, prior_c = jeffreys,
  criterion = crit,
  hypothetical_c = hypothetical(data = data_binary(n = 12, y = 3))
)
qdm_decide(des_hyp, data_t = data_binary(12, 9))

## ----fixed--------------------------------------------------------------------
crit_fix <- criterion_posterior(theta_tv = 0.2, theta_mav = 0.05,
                                gamma_go = 0.8, gamma_nogo = 0.8)
des_fix <- design_binary(n_t = 20, n_c = 0, prior_t = prior_beta(1, 1),
                         criterion = crit_fix, fixed_c = 0.2)
dec_fix <- qdm_decide(des_fix, data_t = data_binary(20, 11))
dec_fix
pbeta(0.2 + 0.2, 1 + 11, 1 + 20 - 11, lower.tail = FALSE)

## ----predictive---------------------------------------------------------------
des_pred <- design_binary(
  n_t = 12, n_c = 12, prior_t = jeffreys, prior_c = jeffreys,
  criterion = criterion_predictive(theta_null = 0, m_t = 40,
                                   gamma_go = 0.9, gamma_nogo = 0.7)
)
dec_pred <- qdm_decide(des_pred, data_t = data_binary(12, 9),
                       data_c = data_binary(12, 3))
dec_pred
post_t <- dec_pred$posterior_t
post_c <- dec_pred$posterior_c
pbetabinomdiff(0, m_t = 40, a_t = post_t[["a"]], b_t = post_t[["b"]],
               m_c = 40, a_c = post_c[["a"]], b_c = post_c[["b"]],
               lower.tail = FALSE)

## ----oc-----------------------------------------------------------------------
oc <- qdm_oc(des, data.frame(pi_t = seq(0.2, 0.8, by = 0.05), pi_c = 0.2))
oc
plot(oc)

## ----oc-hyp-------------------------------------------------------------------
oc_hyp <- qdm_oc(des_hyp, data.frame(pi_t = seq(0.2, 0.8, by = 0.05)))
plot(oc_hyp)

## ----boundary-----------------------------------------------------------------
b <- qdm_boundary(des)
b
plot(b)

## ----boundary-fixed-----------------------------------------------------------
b_fix <- qdm_boundary(des_fix, n_t = seq(10, 40, by = 5))
b_fix
plot(b_fix)

## ----safety-------------------------------------------------------------------
crit_ae <- criterion_posterior(theta_tv = -0.1, theta_mav = 0,
                               gamma_go = 0.6, gamma_nogo = 0.7,
                               direction = "less")
des_ae <- design_binary(n_t = 12, n_c = 12, prior_t = jeffreys,
                        prior_c = jeffreys, criterion = crit_ae)
dec_ae <- qdm_decide(des_ae, data_t = data_binary(12, 2),
                     data_c = data_binary(12, 6))
dec_ae
post_t <- dec_ae$posterior_t
post_c <- dec_ae$posterior_c
pbetadiff(-0.1, a_t = post_t[["a"]], b_t = post_t[["b"]],
          a_c = post_c[["a"]], b_c = post_c[["b"]])
pbetadiff(0, a_t = post_t[["a"]], b_t = post_t[["b"]],
          a_c = post_c[["a"]], b_c = post_c[["b"]], lower.tail = FALSE)

## ----safety-oc----------------------------------------------------------------
plot(qdm_oc(des_ae, data.frame(pi_t = seq(0.1, 0.6, by = 0.05),
                               pi_c = 0.4)))
qdm_boundary(des_ae)

## ----calibrate----------------------------------------------------------------
cal <- qdm_calibrate(des,
                     scenario_go = data.frame(pi_t = 0.2, pi_c = 0.2),
                     scenario_nogo = data.frame(pi_t = 0.5, pi_c = 0.2),
                     target_go = 0.05, target_nogo = 0.2)
cal
plot(cal)

## ----calibrated-oc------------------------------------------------------------
qdm_oc(cal$design, data.frame(pi_t = c(0.2, 0.35, 0.5), pi_c = 0.2))

