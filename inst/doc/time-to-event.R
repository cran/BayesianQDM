## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----design-------------------------------------------------------------------
crit <- criterion_posterior(theta_tv = 0.15, theta_mav = 0,
                            gamma_go = 0.6, gamma_nogo = 0.8)
des <- design_tte(n_t = 30, n_c = 0, prior_t = prior_gamma(0.1, 1),
                  criterion = crit, milestone = 3, fixed_c = 0.72)
dec <- qdm_decide(des, data_t = data_tte(events = 6, exposure = 150))
dec

## ----fixed-check--------------------------------------------------------------
pgamma(-log(0.87) / 3, shape = 0.1 + 6, rate = 1 + 150)

## ----concurrent---------------------------------------------------------------
des_c <- design_tte(n_t = 30, n_c = 30, prior_t = prior_gamma(0.1, 1),
                    prior_c = prior_gamma(0.1, 1), criterion = crit,
                    milestone = 3)
dec_c <- qdm_decide(des_c, data_t = data_tte(6, 150),
                    data_c = data_tte(12, 140))
dec_c
post_t <- dec_c$posterior_t
post_c <- dec_c$posterior_c
psurvdiff(0.15, milestone = 3, shape_t = post_t[["shape"]],
          rate_t = post_t[["rate"]], shape_c = post_c[["shape"]],
          rate_c = post_c[["rate"]], lower.tail = FALSE)

## ----conduct------------------------------------------------------------------
cond <- conduct_tte(accrual_time = c(0, 5, 10, 15),
                    accrual_rate = c(0.4, 2, 4.5),
                    dropout_hazard = 0.00226, cut_time = 15.44)
cond
des <- design_tte(n_t = 30, n_c = 0, prior_t = prior_gamma(0.1, 1),
                  criterion = crit, milestone = 3, conduct = cond,
                  fixed_c = 0.72)

## ----predictive---------------------------------------------------------------
des_pred <- design_tte(
  n_t = 30, n_c = 0, prior_t = prior_gamma(0.1, 1),
  criterion = criterion_predictive(theta_null = 0.05, m_t = 60,
                                   gamma_go = 0.8, gamma_nogo = 0.3),
  milestone = 3, conduct = cond, fixed_c = 0.72, nMC = 5000
)
qdm_decide(des_pred, data_t = data_tte(6, 150), seed = 1)

## ----oc-----------------------------------------------------------------------
med0 <- 3 * log(2) / -log(0.72)
med1 <- 3 * log(2) / -log(0.72 + 0.15)
c(med0, med1)
scen <- data.frame(median_t = c(6, 8, 10, 12, 15, 20))
oc <- qdm_oc(des, scen, seed = 1)
oc
plot(oc)

## ----oc-concurrent------------------------------------------------------------
des_c <- design_tte(n_t = 30, n_c = 30, prior_t = prior_gamma(0.1, 1),
                    prior_c = prior_gamma(0.1, 1), criterion = crit,
                    milestone = 3, conduct = cond)
oc_c <- qdm_oc(des_c, data.frame(median_t = c(med0, 10, 15, 20),
                                 median_c = med0), nsim = 1000, seed = 1)
oc_c

## ----calibrate----------------------------------------------------------------
cal <- qdm_calibrate(des, scenario_go = data.frame(median_t = med0),
                     scenario_nogo = data.frame(median_t = med1),
                     target_go = 0.05, target_nogo = 0.1, seed = 1)
cal
plot(cal)

## ----boundary-----------------------------------------------------------------
b <- qdm_boundary(des, exposure_t = seq(100, 300, by = 50))
b

## ----boundary-plot------------------------------------------------------------
plot(qdm_boundary(des, exposure_t = seq(20, 400, by = 5)))

## ----hr-----------------------------------------------------------------------
crit_hr <- criterion_posterior(theta_tv = 0.67, theta_mav = 0.8,
                               gamma_go = 0.5, gamma_nogo = 0.6,
                               direction = "less")
des_hr <- design_tte(
  n_t = 112, n_c = 56, prior_t = prior_gamma(0.01, 0.01),
  prior_c = prior_gamma(0.01, 0.01), criterion = crit_hr,
  effect = "hazard_ratio",
  conduct = conduct_tte(accrual_time = c(0, 13), dropout_hazard = 0.003,
                        cut_events = 127)
)
dec_hr <- qdm_decide(des_hr, data_t = data_tte(75, 1100),
                     data_c = data_tte(52, 420))
dec_hr
post_t <- dec_hr$posterior_t
post_c <- dec_hr$posterior_c
phazardratio(0.67, shape_t = post_t[["shape"]], rate_t = post_t[["rate"]],
             shape_c = post_c[["shape"]], rate_c = post_c[["rate"]])

## ----hr-oc--------------------------------------------------------------------
oc_hr <- qdm_oc(des_hr, data.frame(median_t = 8.5 / c(0.5, 0.6, 0.7, 0.8, 1),
                                   median_c = 8.5), seed = 1)
oc_hr
plot(oc_hr)

## ----hr-boundary--------------------------------------------------------------
qdm_boundary(des_hr, exposure_t = seq(800, 1400, by = 200),
             data_c = data_tte(52, 420))

