## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 4.5,
  fig.path = "figures/single-bin-"
)
library(BayesianQDM)

## ----ctrl-post----------------------------------------------------------------
# P(pi_t - pi_c > TV  | data)
p_tv <- pbayespostpred1bin(
  prob = 'posterior', design = 'controlled', theta0 = 0.20,
  n_t = 12, n_c = 12, y_t = 8, y_c = 3,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = NULL, m_c = NULL, z = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  lower.tail = FALSE
)

# P(pi_t - pi_c > MAV | data)
p_mav <- pbayespostpred1bin(
  prob = 'posterior', design = 'controlled', theta0 = 0.05,
  n_t = 12, n_c = 12, y_t = 8, y_c = 3,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = NULL, m_c = NULL, z = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  lower.tail = FALSE
)

cat(sprintf("P(theta > TV  | data) = %.4f  -> Go  criterion (>= %.2f): %s\n",
            p_tv,  0.80, ifelse(p_tv  >= 0.80, "YES", "NO")))
cat(sprintf("P(theta <= MAV | data) = %.4f  -> NoGo criterion (>= %.2f): %s\n",
            1 - p_mav, 0.20, ifelse((1 - p_mav) >= 0.20, "YES", "NO")))
cat(sprintf("Decision: %s\n",
            ifelse(p_tv >= 0.80 & (1 - p_mav) < 0.20, "Go",
                   ifelse(p_tv < 0.80 & (1 - p_mav) >= 0.20, "NoGo",
                          ifelse(p_tv >= 0.80 & (1 - p_mav) >= 0.20, "Miss", "Gray")))))

## ----ctrl-pred----------------------------------------------------------------
p_pred <- pbayespostpred1bin(
  prob = 'predictive', design = 'controlled', theta0 = 0.10,
  n_t = 12, n_c = 12, y_t = 8, y_c = 3,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = 40, m_c = 40, z = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  lower.tail = FALSE
)
cat(sprintf("Predictive probability (m_t = m_c = 40) = %.4f\n", p_pred))

## ----ctrl-vector--------------------------------------------------------------
grid <- expand.grid(y_t = 0:12, y_c = 0:12)
p_all <- pbayespostpred1bin(
  prob = 'posterior', design = 'controlled', theta0 = 0.20,
  n_t = 12, n_c = 12, y_t = grid$y_t, y_c = grid$y_c,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = NULL, m_c = NULL, z = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  lower.tail = FALSE
)
# Show results for a selection of outcome pairs
sel <- data.frame(y_t = grid$y_t, y_c = grid$y_c, P_gt_TV = round(p_all, 4))
head(sel[order(-sel$P_gt_TV), ], 10)

## ----unctrl-post--------------------------------------------------------------
# Hypothetical control: z = 2 responders out of n_c = 12
p_unctrl <- pbayespostpred1bin(
  prob = 'posterior', design = 'uncontrolled', theta0 = 0.20,
  n_t = 12, n_c = 12, y_t = 8, y_c = NULL,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = NULL, m_c = NULL, z = 2L,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  lower.tail = FALSE
)
cat(sprintf("P(theta > TV | data, uncontrolled, z=2) = %.4f\n", p_unctrl))

## ----unctrl-z-sens------------------------------------------------------------
z_seq <- 0:12
p_z   <- sapply(z_seq, function(z) {
  pbayespostpred1bin(
    prob = 'posterior', design = 'uncontrolled', theta0 = 0.20,
    n_t = 12, n_c = 12, y_t = 8, y_c = NULL,
    a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
    m_t = NULL, m_c = NULL, z = z,
    ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
    alpha0e_t = NULL, alpha0e_c = NULL,
    lower.tail = FALSE
  )
})
data.frame(z = z_seq, P_gt_TV = round(p_z, 4))

## ----ext-post-----------------------------------------------------------------
p_ext <- pbayespostpred1bin(
  prob = 'posterior', design = 'external', theta0 = 0.20,
  n_t = 12, n_c = 12, y_t = 8, y_c = 3,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  m_t = NULL, m_c = NULL, z = NULL,
  ne_t = 15L, ne_c = 15L, ye_t = 5L, ye_c = 4L,
  alpha0e_t = 0.5, alpha0e_c = 0.5,
  lower.tail = FALSE
)
cat(sprintf("P(theta > TV | data, external, alpha0e=0.5) = %.4f\n", p_ext))

## ----ext-borrowing------------------------------------------------------------
# alpha0e must be in (0, 1]; avoid alpha0e = 0
ae_seq <- c(0.01, seq(0.1, 1.0, by = 0.1))
p_ae   <- sapply(ae_seq, function(ae) {
  pbayespostpred1bin(
    prob = 'posterior', design = 'external', theta0 = 0.20,
    n_t = 12, n_c = 12, y_t = 8, y_c = 3,
    a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
    m_t = NULL, m_c = NULL, z = NULL,
    ne_t = 15L, ne_c = 15L, ye_t = 5L, ye_c = 4L,
    alpha0e_t = 0.5, alpha0e_c = ae,
    lower.tail = FALSE
  )
})
data.frame(alpha0e_c = ae_seq, P_gt_TV = round(p_ae, 4))

## ----oc-controlled, fig.width = 8, fig.height = 6-----------------------------
oc_ctrl <- pbayesdecisionprob1bin(
  prob      = 'posterior',
  design    = 'controlled',
  theta_TV  = 0.30,  theta_MAV = 0.15,  theta_NULL = NULL,
  gamma_go  = 0.80,  gamma_nogo = 0.20,
  pi_t      = seq(0.10, 0.80, by = 0.05),
  pi_c      = rep(0.10, length(seq(0.10, 0.80, by = 0.05))),
  n_t = 12,  n_c = 12,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  z = NULL, m_t = NULL, m_c = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  error_if_Miss = TRUE, Gray_inc_Miss = FALSE
)
print(oc_ctrl)
plot(oc_ctrl, base_size = 20)

## ----getgamma-ctrl, fig.width = 8, fig.height = 6-----------------------------
res_gamma <- getgamma1bin(
  prob      = 'posterior',
  design    = 'controlled',
  theta_TV  = 0.30, theta_MAV = 0.15, theta_NULL = NULL,
  pi_t_go   = 0.10, pi_c_go   = 0.10,
  pi_t_nogo = 0.30, pi_c_nogo = 0.10,
  target_go = 0.05, target_nogo = 0.20,
  n_t = 12L, n_c = 12L,
  a_t = 0.5, b_t = 0.5, a_c = 0.5, b_c = 0.5,
  z = NULL, m_t = NULL, m_c = NULL,
  ne_t = NULL, ne_c = NULL, ye_t = NULL, ye_c = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  gamma_grid = seq(0.01, 0.99, by = 0.01)
)
plot(res_gamma, base_size = 20)

