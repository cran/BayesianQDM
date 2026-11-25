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

## ----scenarios----------------------------------------------------------------
qdm_scenarios(des, pi_t = c(0.3, 0.5), pi_c = c(0.2, 0.3))

## ----scenarios-theta----------------------------------------------------------
scen <- qdm_scenarios(des, theta = seq(0, 0.4, by = 0.1), pi_c = 0.3)
scen
qdm_oc(des, qdm_scenarios(des, theta = "thresholds", pi_c = 0.3))

## ----compare, fig.alt = "Probabilities of Go, NoGo and Gray decisions against the treatment effect for three sample sizes"----
n <- c(12, 18, 24)
designs <- lapply(n, function(k) qdm_sample_size(des, n_t = k, n_c = k))
names(designs) <- paste(n, "per group")
cmp <- qdm_compare(designs, scen)
cmp
plot(cmp)

## ----compare-numbers, include = FALSE-----------------------------------------
go_effect <- function(label) {
  formatC(cmp$Go[cmp$design == label & abs(cmp$theta - 0.3) < 1e-9],
          format = "f", digits = 2)
}

## ----compare-borrowing, fig.alt = "Probabilities of Go and NoGo decisions against the treatment effect, with and without borrowing external control data"----
ext <- des |>
  qdm_external(external_c = borrow(data_binary(n = 40, y = 12), a0 = 0.5))
qdm_compare(list(Concurrent = des, Borrowing = ext), scen) |>
  plot(decisions = c("Go", "NoGo"))

## ----find-n, fig.alt = "Probabilities of meeting the Go and NoGo criteria against the candidate sample size, with the target and the chosen sample size"----
res <- qdm_find_n(des,
                  scenario_go = data.frame(pi_t = 0.3, pi_c = 0.3),
                  scenario_nogo = data.frame(pi_t = 0.6, pi_c = 0.3),
                  target_go = 0.1, target_nogo = 0.1,
                  n_t = seq(6, 30, by = 2))
res
plot(res)

## ----find-n-text, include = FALSE---------------------------------------------
stable_text <- if (is.na(res$n_t_stable)) {
  "the largest candidate does not meet them"
} else if (res$n_t_stable == res$n_t) {
  "every larger candidate also meets them"
} else {
  fail <- res$curve$n_t[res$curve$n_t > res$n_t & !res$curve$met]
  many <- length(fail) > 1L
  sprintf(paste("the larger candidate%s with %s subjects per group %s not",
                "meet them, and every candidate from %d subjects per group",
                "on does"), if (many) "s" else "", paste(fail, collapse = ", "),
          if (many) "do" else "does", res$n_t_stable)
}

## ----tables-------------------------------------------------------------------
priors <- list(Jeffreys = prior_beta(0.5, 0.5), Uniform = prior_beta(1, 1),
               Skeptical = prior_beta(2, 6))
decisions <- lapply(priors, function(p) {
  des |>
    qdm_prior(prior_t = p, prior_c = p) |>
    qdm_decide(data_t = data_binary(12, 8), data_c = data_binary(12, 4))
})
do.call(rbind, lapply(decisions, as.data.frame))

## ----plot-decision, fig.alt = "Posterior density of the difference in response rates with the Go and NoGo regions"----
dec <- qdm_decide(des, data_t = data_binary(12, 8),
                  data_c = data_binary(12, 4))
plot(dec)

## ----code---------------------------------------------------------------------
show_code(res$design)
show_code(res$design, style = "design")

