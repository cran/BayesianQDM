## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)
library(BayesianQDM)

## ----spec-start---------------------------------------------------------------
spec <- qdm_endpoint("binary") |>
  qdm_sample_size(n_t = 12, n_c = 12)
spec

## ----spec-complete------------------------------------------------------------
spec <- spec |>
  qdm_prior(prior_t = prior_beta(0.5, 0.5),
            prior_c = prior_beta(0.5, 0.5)) |>
  qdm_criterion(criterion_posterior(theta_tv = 0.2, theta_mav = 0.1,
                                    gamma_go = 0.5, gamma_nogo = 0.8))
spec

## ----build--------------------------------------------------------------------
des <- design_binary(
  n_t = 12, n_c = 12,
  prior_t = prior_beta(0.5, 0.5), prior_c = prior_beta(0.5, 0.5),
  criterion = criterion_posterior(theta_tv = 0.2, theta_mav = 0.1,
                                  gamma_go = 0.5, gamma_nogo = 0.8)
)
identical(qdm_build(spec), des)

## ----error--------------------------------------------------------------------
try(spec |> qdm_sample_size(n_t = 0, n_c = 12) |> qdm_build())

## ----decide-------------------------------------------------------------------
spec |>
  qdm_decide(data_t = data_binary(12, 9), data_c = data_binary(12, 4))

## ----oc, fig.alt = "Probabilities of the decisions against the true difference in response rates"----
scen <- data.frame(pi_t = seq(0.3, 0.7, by = 0.05), pi_c = 0.3)
spec |>
  qdm_oc(scen) |>
  plot()

## ----calibrate----------------------------------------------------------------
cal <- spec |>
  qdm_calibrate(scenario_go = data.frame(pi_t = 0.3, pi_c = 0.3),
                scenario_nogo = data.frame(pi_t = 0.6, pi_c = 0.3),
                target_go = 0.05, target_nogo = 0.2)
cal

## ----calibrated---------------------------------------------------------------
cal$design |>
  qdm_decide(data_t = data_binary(12, 9), data_c = data_binary(12, 4))

## ----boundary, fig.alt = "Regions of the decisions by the numbers of control and treatment responders"----
spec |>
  qdm_boundary() |>
  plot()

## ----sample-size--------------------------------------------------------------
scen2 <- data.frame(pi_t = c(0.2, 0.5), pi_c = 0.2)
n <- c(12, 18, 24, 30)
go <- vapply(n, function(k) {
  oc <- spec |>
    qdm_sample_size(n_t = k, n_c = k) |>
    qdm_oc(scen2)
  oc$Go
}, numeric(2))
data.frame(n = n, no_effect = go[1, ], effect = go[2, ])

## ----compare------------------------------------------------------------------
designs <- lapply(n, function(k) qdm_sample_size(des, n_t = k, n_c = k))
names(designs) <- paste(n, "per group")
qdm_compare(designs, scen2)

## ----external-----------------------------------------------------------------
spec |>
  qdm_external(external_c = borrow(data_binary(n = 40, y = 8), a0 = 0.5)) |>
  qdm_oc(scen2)

## ----predictive---------------------------------------------------------------
spec |>
  qdm_criterion(criterion_predictive(theta_null = 0.1, m_t = 50, m_c = 50,
                                     gamma_go = 0.8, gamma_nogo = 0.8)) |>
  qdm_decide(data_t = data_binary(12, 9), data_c = data_binary(12, 4))

## ----fixed--------------------------------------------------------------------
spec |>
  qdm_sample_size(n_t = 20, n_c = 0) |>
  qdm_prior(prior_t = prior_beta(0.5, 0.5)) |>
  qdm_control(fixed_c = 0.2) |>
  qdm_decide(data_t = data_binary(20, 9))

## ----continuous---------------------------------------------------------------
qdm_endpoint("continuous") |>
  qdm_sample_size(n_t = 15, n_c = 15) |>
  qdm_prior(prior_t = prior_vague(), prior_c = prior_vague()) |>
  qdm_criterion(criterion_posterior(theta_tv = 30, theta_mav = 15,
                                    gamma_go = 0.6, gamma_nogo = 0.7)) |>
  qdm_computation(method = "MM") |>
  qdm_oc(data.frame(mu_t = c(0, 20, 40), sd_t = 25, mu_c = 0, sd_c = 25),
         nsim = 2000, seed = 1)

## ----tte----------------------------------------------------------------------
qdm_endpoint("tte") |>
  qdm_sample_size(n_t = 30, n_c = 30) |>
  qdm_prior(prior_t = prior_gamma(0.1, 1), prior_c = prior_gamma(0.1, 1)) |>
  qdm_criterion(criterion_posterior(theta_tv = 0.67, theta_mav = 0.8,
                                    gamma_go = 0.5, gamma_nogo = 0.6,
                                    direction = "less")) |>
  qdm_tte(effect = "hazard_ratio") |>
  qdm_decide(data_t = data_tte(events = 12, exposure = 300),
             data_c = data_tte(events = 20, exposure = 260))

## ----report-------------------------------------------------------------------
spec |>
  qdm_oc(scen2) |>
  show_report(file_type = "text", open = TRUE)

## ----report-html, eval = FALSE------------------------------------------------
# spec |>
#   qdm_oc(scen2) |>
#   show_report(file = "decision_criteria.html")

## ----code---------------------------------------------------------------------
show_code(des)
show_code(des, style = "design")

