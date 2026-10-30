## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----design-------------------------------------------------------------------
des <- design_binary(
  n_t = 12, n_c = 12,
  prior_t = prior_beta(0.5, 0.5), prior_c = prior_beta(0.5, 0.5),
  criterion = criterion_posterior(theta_tv = 0.2, theta_mav = 0.1,
                                  gamma_go = 0.5, gamma_nogo = 0.8)
)
des

## ----decide-------------------------------------------------------------------
dec <- qdm_decide(des, data_t = data_binary(12, 9),
                  data_c = data_binary(12, 4))
dec

## ----oc-----------------------------------------------------------------------
oc <- qdm_oc(des, data.frame(pi_t = seq(0.3, 0.7, by = 0.05), pi_c = 0.3))
oc
plot(oc)

## ----calibrate----------------------------------------------------------------
cal <- qdm_calibrate(des,
                     scenario_go = data.frame(pi_t = 0.3, pi_c = 0.3),
                     scenario_nogo = data.frame(pi_t = 0.6, pi_c = 0.3),
                     target_go = 0.05, target_nogo = 0.2)
cal
plot(cal)

## ----boundary-----------------------------------------------------------------
b <- qdm_boundary(des)
b
plot(b)

## ----plot-style---------------------------------------------------------------
plot(oc, col_go = "darkgreen", col_nogo = "firebrick", linewidth = 0.7,
     base_size = 13, legend_position = "right") +
  ggplot2::labs(title = "Operating characteristics of the design")

## ----validation, eval = FALSE-------------------------------------------------
# system.file("validation", "results", "summary.md", package = "BayesianQDM")

