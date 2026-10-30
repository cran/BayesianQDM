## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----design-------------------------------------------------------------------
crit <- criterion_posterior(theta_tv = 15, theta_mav = 5,
                            gamma_go = 0.6, gamma_nogo = 0.7)
des <- design_continuous(n_t = 12, n_c = 12, prior_t = prior_vague(),
                         prior_c = prior_vague(), criterion = crit)
dec <- qdm_decide(des, data_t = data_continuous(n = 12, mean = 22, sd = 25),
                  data_c = data_continuous(n = 12, mean = 2, sd = 25))
dec

## ----methods------------------------------------------------------------------
post_t <- dec$posterior_t
post_c <- dec$posterior_c
scale_t <- post_t[["sigma"]] / sqrt(post_t[["kappa"]])
scale_c <- post_c[["sigma"]] / sqrt(post_c[["kappa"]])
set.seed(1)
sapply(c("NI", "MM", "MC"), function(m) {
  ptdiff(15, mu_t = post_t[["mu"]], scale_t = scale_t, df_t = post_t[["nu"]],
         mu_c = post_c[["mu"]], scale_c = scale_c, df_c = post_c[["nu"]],
         method = m, lower.tail = FALSE)
})

## ----nix----------------------------------------------------------------------
des_nix <- design_continuous(
  n_t = 12, n_c = 12, prior_t = prior_vague(),
  prior_c = prior_nix(mu0 = 0, kappa0 = 2, nu0 = 4, sigma0 = 25),
  criterion = crit
)
qdm_decide(des_nix, data_t = data_continuous(12, 22, 25),
           data_c = data_continuous(12, 2, 25))$posterior_c

## ----external-----------------------------------------------------------------
des_ext <- design_continuous(
  n_t = 12, n_c = 12, prior_t = prior_vague(), prior_c = prior_vague(),
  criterion = crit,
  external_c = borrow(data_continuous(n = 30, mean = 0, sd = 25), a0 = 0.5)
)
dec_ext <- qdm_decide(des_ext, data_t = data_continuous(12, 22, 25),
                      data_c = data_continuous(12, 2, 25))
dec_ext$posterior_c

## ----hypothetical-------------------------------------------------------------
des_hyp <- design_continuous(
  n_t = 12, n_c = 0, prior_t = prior_vague(), criterion = crit,
  hypothetical_c = hypothetical(mean = 0, r = 1)
)
qdm_decide(des_hyp, data_t = data_continuous(12, 22, 25))

## ----fixed--------------------------------------------------------------------
des_fix <- design_continuous(n_t = 12, n_c = 0, prior_t = prior_vague(),
                             criterion = crit, fixed_c = -10)
dec_fix <- qdm_decide(des_fix, data_t = data_continuous(12, 22, 25))
dec_fix
pt((22 - (-10 + 15)) / (25 / sqrt(12)), df = 11)

## ----predictive---------------------------------------------------------------
des_pred <- design_continuous(
  n_t = 12, n_c = 12, prior_t = prior_vague(), prior_c = prior_vague(),
  criterion = criterion_predictive(theta_null = 0, m_t = 50,
                                   gamma_go = 0.9, gamma_nogo = 0.7)
)
qdm_decide(des_pred, data_t = data_continuous(12, 22, 25),
           data_c = data_continuous(12, 2, 25))

## ----oc-----------------------------------------------------------------------
des_mm <- design_continuous(n_t = 12, n_c = 12, prior_t = prior_vague(),
                            prior_c = prior_vague(), criterion = crit,
                            method = "MM")
scen <- data.frame(mu_t = seq(0, 40, by = 5), sd_t = 25, mu_c = 0, sd_c = 25)
oc <- qdm_oc(des_mm, scen, seed = 1)
oc
plot(oc)

## ----oc-ni--------------------------------------------------------------------
scen3 <- scen[c(1, 3, 5), ]
oc_ni <- qdm_oc(des, scen3, nsim = 1000, seed = 1)
oc_mm <- qdm_oc(des_mm, scen3, nsim = 1000, seed = 1)
data.frame(mu_t = scen3$mu_t, Go_NI = oc_ni$Go, Go_MM = oc_mm$Go,
           NoGo_NI = oc_ni$NoGo, NoGo_MM = oc_mm$NoGo)

## ----boundary-----------------------------------------------------------------
b <- qdm_boundary(des_mm, sd_t = seq(10, 40, by = 5),
                  data_c = data_continuous(12, 2, 25))
b
plot(b)

## ----boundary-fixed-----------------------------------------------------------
b_fix <- qdm_boundary(des_fix, n_t = c(6, 12, 20, 30, 40), sd_t = 25)
b_fix
plot(b_fix)

## ----calibrate----------------------------------------------------------------
cal <- qdm_calibrate(
  des_mm,
  scenario_go = data.frame(mu_t = 0, sd_t = 25, mu_c = 0, sd_c = 25),
  scenario_nogo = data.frame(mu_t = 20, sd_t = 25, mu_c = 0, sd_c = 25),
  target_go = 0.05, target_nogo = 0.2, seed = 1
)
cal
plot(cal)

