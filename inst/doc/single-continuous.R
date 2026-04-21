## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 4.5,
  fig.path = "figures/single-cont-"
)
library(BayesianQDM)

## ----method-compare-----------------------------------------------------------
# Observed data (nMC excluded from base list; passed individually per method)
args_base <- list(
  prob = 'posterior', design = 'controlled', prior = 'vague',
  theta0 = 1.0,
  n_t = 15, n_c = 15,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = 1.1, s_c = 1.8,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL
)

p_ni <- do.call(pbayespostpred1cont,
                c(args_base, list(CalcMethod = 'NI', nMC = NULL)))
p_mm <- do.call(pbayespostpred1cont,
                c(args_base, list(CalcMethod = 'MM', nMC = NULL)))
set.seed(42)
p_mc <- do.call(pbayespostpred1cont,
                c(args_base, list(CalcMethod = 'MC', nMC = 10000L)))

cat(sprintf("NI : %.6f  (exact)\n",      p_ni))
cat(sprintf("MM : %.6f  (approx)\n",     p_mm))
cat(sprintf("MC : %.6f  (n_MC=10000)\n", p_mc))

## ----ctrl-post----------------------------------------------------------------
# P(mu_t - mu_c > theta_TV | data) and P(mu_t - mu_c <= theta_MAV | data)
p_tv <- pbayespostpred1cont(
  prob = 'posterior', design = 'controlled', prior = 'vague',
  CalcMethod = 'NI', theta0 = 1.5, nMC = NULL,
  n_t = 15, n_c = 15,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = 1.1, s_c = 1.8,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL,
  lower.tail = FALSE
)

p_mav <- pbayespostpred1cont(
  prob = 'posterior', design = 'controlled', prior = 'vague',
  CalcMethod = 'NI', theta0 = 0.5, nMC = NULL,
  n_t = 15, n_c = 15,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = 1.1, s_c = 1.8,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL,
  lower.tail = FALSE
)

cat(sprintf("P(theta > theta_TV  | data) = %.4f  -> Go  criterion: >= 0.80? %s\n",
            p_tv, ifelse(p_tv >= 0.80, "YES", "NO")))
cat(sprintf("P(theta <= theta_MAV | data) = %.4f  -> NoGo criterion: >= 0.20? %s\n",
            1 - p_mav, ifelse((1 - p_mav) >= 0.20, "YES", "NO")))
cat(sprintf("Decision: %s\n",
            ifelse(p_tv >= 0.80 & (1 - p_mav) < 0.20, "Go",
                   ifelse(p_tv < 0.80 & (1 - p_mav) >= 0.20, "NoGo",
                          ifelse(p_tv >= 0.80 & (1 - p_mav) >= 0.20, "Miss", "Gray")))))

## ----ctrl-post-niChisq--------------------------------------------------------
p_tv_inf <- pbayespostpred1cont(
  prob = 'posterior', design = 'controlled', prior = 'N-Inv-Chisq',
  CalcMethod = 'NI', theta0 = 1.5, nMC = NULL,
  n_t = 15, n_c = 15,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = 1.1, s_c = 1.8,
  m_t = NULL, m_c = NULL,
  kappa0_t = 5, kappa0_c = 5,
  nu0_t    = 5, nu0_c    = 5,
  mu0_t    = 3.0, mu0_c  = 1.0,
  sigma0_t = 2.0, sigma0_c = 1.8,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL,
  lower.tail = FALSE
)
cat(sprintf("P(theta > theta_TV | data, N-Inv-Chisq prior) = %.4f\n", p_tv_inf))

## ----ctrl-pred----------------------------------------------------------------
p_pred <- pbayespostpred1cont(
  prob = 'predictive', design = 'controlled', prior = 'vague',
  CalcMethod = 'NI', theta0 = 1.0, nMC = NULL,
  n_t = 15, n_c = 15, m_t = 60, m_c = 60,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = 1.1, s_c = 1.8,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL,
  lower.tail = FALSE
)
cat(sprintf("Predictive probability (m_t = m_c = 60) = %.4f\n", p_pred))

## ----unctrl-post--------------------------------------------------------------
p_unctrl <- pbayespostpred1cont(
  prob = 'posterior', design = 'uncontrolled', prior = 'vague',
  CalcMethod = 'MM', theta0 = 1.5, nMC = NULL,
  n_t = 15, n_c = NULL,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = NULL, s_c = NULL,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = 1.0,   # hypothetical control mean
  sigma0_t = NULL, sigma0_c = NULL,
  r = 1.0,                     # equal variance assumption
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL,
  lower.tail = FALSE
)
cat(sprintf("P(theta > theta_TV | data, uncontrolled) = %.4f\n", p_unctrl))

## ----ext-post-----------------------------------------------------------------
p_ext <- pbayespostpred1cont(
  prob = 'posterior', design = 'external', prior = 'vague',
  CalcMethod = 'MM', theta0 = 1.5, nMC = NULL,
  n_t = 15, n_c = 15,
  bar_y_t = 3.2, s_t = 2.0,
  bar_y_c = 1.1, s_c = 1.8,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = 20L, alpha0e_t = NULL, alpha0e_c = 0.5,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = 0.9, se_c = 1.8,
  lower.tail = FALSE
)
cat(sprintf("P(theta > theta_TV | data, external control design, alpha=0.5) = %.4f\n",
            p_ext))

## ----ext-borrowing------------------------------------------------------------
# alpha0e_c must be in (0, 1]; start from 0.01 to avoid validation error
alpha_seq <- c(0.01, seq(0.1, 1.0, by = 0.1))
p_alpha   <- sapply(alpha_seq, function(a) {
  pbayespostpred1cont(
    prob = 'posterior', design = 'external', prior = 'vague',
    CalcMethod = 'MM', theta0 = 1.5, nMC = NULL,
    n_t = 15, n_c = 15,
    bar_y_t = 3.2, s_t = 2.0,
    bar_y_c = 1.1, s_c = 1.8,
    m_t = NULL, m_c = NULL,
    kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
    mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
    r = NULL,
    ne_t = NULL, ne_c = 20L, alpha0e_t = NULL, alpha0e_c = a,
    bar_ye_t = NULL, se_t = NULL, bar_ye_c = 0.9, se_c = 1.8,
    lower.tail = FALSE
  )
})
data.frame(alpha_ec = alpha_seq, P_gt_TV = round(p_alpha, 4))

## ----oc-controlled, fig.width = 8, fig.height = 6-----------------------------
oc_ctrl <- pbayesdecisionprob1cont(
  nsim       = 500L,
  prob       = 'posterior',
  design     = 'controlled',
  prior      = 'vague',
  CalcMethod = 'MM',
  theta_TV   = 1.5,  theta_MAV = 0.5,  theta_NULL = NULL,
  nMC        = NULL,
  gamma_go   = 0.80, gamma_nogo = 0.20,
  n_t = 15,   n_c = 15,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  mu_t    = seq(1.0, 4.0, by = 0.5),
  mu_c    = 1.0,
  sigma_t = 2.0,
  sigma_c = 2.0,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, se_t = NULL, bar_ye_c = NULL, se_c = NULL,
  error_if_Miss = TRUE, Gray_inc_Miss = FALSE,
  seed = 42L
)
print(oc_ctrl)
plot(oc_ctrl, base_size = 20)

## ----getgamma-ctrl, fig.width = 8, fig.height = 6-----------------------------
res_gamma <- getgamma1cont(
  nsim       = 1000L,
  prob       = 'posterior',
  design     = 'controlled',
  prior      = 'vague',
  CalcMethod = 'MM',
  theta_TV   = 1.5, theta_MAV = 0.5, theta_NULL = NULL,
  nMC        = NULL,
  mu_t_go    = 1.0, mu_c_go    = 1.0,   # null scenario: no treatment effect
  sigma_t_go = 2.0, sigma_c_go = 2.0,
  mu_t_nogo    = 2.5, mu_c_nogo    = 1.0,
  sigma_t_nogo = 2.0, sigma_c_nogo = 2.0,
  target_go = 0.05, target_nogo = 0.20,
  n_t = 15L, n_c = 15L, m_t = NULL, m_c = NULL,
  kappa0_t = NULL, kappa0_c = NULL, nu0_t = NULL, nu0_c = NULL,
  mu0_t = NULL, mu0_c = NULL, sigma0_t = NULL, sigma0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  gamma_grid = seq(0.01, 0.99, by = 0.01),
  seed = 42L
)
plot(res_gamma, base_size = 20)

