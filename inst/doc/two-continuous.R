## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5.5,
  fig.path = "figures/2cont-"
)
library(BayesianQDM)

## ----nine-region-cont, echo = FALSE, results = 'asis'-------------------------
cat('
<table style="border-collapse:collapse; text-align:center; font-size:0.9em;">
<caption>Nine-region grid for two-endpoint posterior probability</caption>
<thead>
  <tr>
    <th colspan="2" rowspan="2" style="border:1px solid #aaa; background:
      linear-gradient(to top right, white 49.5%, #aaa 49.5%, #aaa 50.5%, white 50.5%);
      min-width:80px; min-height:60px; padding:4px;">
    </th>
    <th colspan="3" style="border:1px solid #aaa; padding:6px; font-weight:normal;">Endpoint 1</th>
  </tr>
  <tr>
    <th style="border:1px solid #aaa; padding:6px; font-weight:normal;">&theta;<sub>1</sub> &gt; &theta;<sub>TV1</sub></th>
    <th style="border:1px solid #aaa; padding:6px; font-weight:normal;">&theta;<sub>TV1</sub> &ge; &theta;<sub>1</sub> &gt; &theta;<sub>MAV1</sub></th>
    <th style="border:1px solid #aaa; padding:6px; font-weight:normal;">&theta;<sub>MAV1</sub> &ge; &theta;<sub>1</sub></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td rowspan="3" style="border:1px solid #aaa; padding:6px; writing-mode:vertical-rl;
        transform:rotate(180deg);">Endpoint 2</td>
    <td style="border:1px solid #aaa; padding:6px; text-align:left;">
      &theta;<sub>2</sub> &gt; &theta;<sub>TV2</sub></td>
    <td style="border:1px solid #aaa; padding:6px;">R1</td>
    <td style="border:1px solid #aaa; padding:6px;">R4</td>
    <td style="border:1px solid #aaa; padding:6px;">R7</td>
  </tr>
  <tr>
    <td style="border:1px solid #aaa; padding:6px; text-align:left;">
      &theta;<sub>TV2</sub> &ge; &theta;<sub>2</sub> &gt; &theta;<sub>MAV2</sub></td>
    <td style="border:1px solid #aaa; padding:6px;">R2</td>
    <td style="border:1px solid #aaa; padding:6px;">R5</td>
    <td style="border:1px solid #aaa; padding:6px;">R8</td>
  </tr>
  <tr>
    <td style="border:1px solid #aaa; padding:6px; text-align:left;">
      &theta;<sub>MAV2</sub> &ge; &theta;<sub>2</sub></td>
    <td style="border:1px solid #aaa; padding:6px;">R3</td>
    <td style="border:1px solid #aaa; padding:6px;">R6</td>
    <td style="border:1px solid #aaa; padding:6px;">R9</td>
  </tr>
</tbody>
</table>
')

## ----four-region-cont, echo = FALSE, results = 'asis'-------------------------
cat('
<table style="border-collapse:collapse; text-align:center; font-size:0.9em;">
<caption>Four-region grid for two-endpoint predictive probability</caption>
<thead>
  <tr>
    <th colspan="2" rowspan="2" style="border:1px solid #aaa; background:
      linear-gradient(to top right, white 49.5%, #aaa 49.5%, #aaa 50.5%, white 50.5%);
      min-width:80px; min-height:60px; padding:4px;">
    </th>
    <th colspan="2" style="border:1px solid #aaa; padding:6px; font-weight:normal;">Endpoint 1</th>
  </tr>
  <tr>
    <th style="border:1px solid #aaa; padding:6px; font-weight:normal;">&theta;<sub>1</sub> &gt; &theta;<sub>NULL1</sub></th>
    <th style="border:1px solid #aaa; padding:6px; font-weight:normal;">&theta;<sub>1</sub> &le; &theta;<sub>NULL1</sub></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td rowspan="2" style="border:1px solid #aaa; padding:6px; writing-mode:vertical-rl;
        transform:rotate(180deg);">Endpoint 2</td>
    <td style="border:1px solid #aaa; padding:6px; text-align:left;">
      &theta;<sub>2</sub> &gt; &theta;<sub>NULL2</sub></td>
    <td style="border:1px solid #aaa; padding:6px;">R1</td>
    <td style="border:1px solid #aaa; padding:6px;">R3</td>
  </tr>
  <tr>
    <td style="border:1px solid #aaa; padding:6px; text-align:left;">
      &theta;<sub>2</sub> &le; &theta;<sub>NULL2</sub></td>
    <td style="border:1px solid #aaa; padding:6px;">R2</td>
    <td style="border:1px solid #aaa; padding:6px;">R4</td>
  </tr>
</tbody>
</table>
')

## ----ctrl-post-vague----------------------------------------------------------
S_t <- matrix(c(18.0, 3.6, 3.6, 9.0), 2, 2)
S_c <- matrix(c(16.0, 2.8, 2.8, 8.5), 2, 2)

set.seed(42)
p_post_vague <- pbayespostpred2cont(
  prob = 'posterior', design = 'controlled', prior = 'vague',
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  n_t = 20L, n_c = 20L,
  ybar_t = c(3.5, 2.1), S_t = S_t,
  ybar_c = c(1.8, 1.0), S_c = S_c,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, nu0_t = NULL, mu0_t = NULL, Lambda0_t = NULL,
  kappa0_c = NULL, nu0_c = NULL, mu0_c = NULL, Lambda0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  nMC = 5000L
)
print(round(p_post_vague, 4))
cat(sprintf(
  "g_Go  = P(R1 | data) = %.4f\n", p_post_vague["R1"]
))
cat(sprintf(
  "g_NoGo = P(R9 | data) = %.4f\n\n", p_post_vague["R9"]
))
cat(sprintf(
  "Go  criterion: g_Go  >= gamma1 (0.80)? %s\n",
  ifelse(p_post_vague["R1"] >= 0.80, "YES", "NO")
))
cat(sprintf(
  "NoGo criterion: g_NoGo >= gamma2 (0.20)? %s\n",
  ifelse(p_post_vague["R9"] >= 0.20, "YES", "NO")
))
cat(sprintf("Decision: %s\n",
  ifelse(p_post_vague["R1"] >= 0.80 & p_post_vague["R9"] <  0.20, "Go",
  ifelse(p_post_vague["R1"] <  0.80 & p_post_vague["R9"] >= 0.20, "NoGo",
  ifelse(p_post_vague["R1"] >= 0.80 & p_post_vague["R9"] >= 0.20, "Miss",
                                                                    "Gray")))
))

## ----ctrl-post-niw------------------------------------------------------------
L0 <- matrix(c(8.0, 0.0, 0.0, 2.0), 2, 2)

set.seed(42)
p_post_niw <- pbayespostpred2cont(
  prob = 'posterior', design = 'controlled', prior = 'N-Inv-Wishart',
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  n_t = 20L, n_c = 20L,
  ybar_t = c(3.5, 2.1), S_t = S_t,
  ybar_c = c(1.8, 1.0), S_c = S_c,
  m_t = NULL, m_c = NULL,
  kappa0_t = 2.0, nu0_t = 5.0, mu0_t = c(2.0, 1.0), Lambda0_t = L0,
  kappa0_c = 2.0, nu0_c = 5.0, mu0_c = c(0.0, 0.0), Lambda0_c = L0,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  nMC = 5000L
)
print(round(p_post_niw, 4))

## ----ctrl-pred----------------------------------------------------------------
set.seed(42)
p_pred <- pbayespostpred2cont(
  prob = 'predictive', design = 'controlled', prior = 'vague',
  theta_TV1 = NULL, theta_MAV1 = NULL,
  theta_TV2 = NULL, theta_MAV2 = NULL,
  theta_NULL1 = 0.5, theta_NULL2 = 0.3,
  n_t = 20L, n_c = 20L,
  ybar_t = c(3.5, 2.1), S_t = S_t,
  ybar_c = c(1.8, 1.0), S_c = S_c,
  m_t = 60L, m_c = 60L,
  kappa0_t = NULL, nu0_t = NULL, mu0_t = NULL, Lambda0_t = NULL,
  kappa0_c = NULL, nu0_c = NULL, mu0_c = NULL, Lambda0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  nMC = 5000L
)
print(round(p_pred, 4))
cat(sprintf(
  "\nGo  region (R1): P = %.4f  >= gamma1 (0.80)? %s\n",
  p_pred["R1"], ifelse(p_pred["R1"] >= 0.80, "YES", "NO")
))
cat(sprintf(
  "NoGo region (R4): P = %.4f  >= gamma2 (0.20)? %s\n",
  p_pred["R4"], ifelse(p_pred["R4"] >= 0.20, "YES", "NO")
))
cat(sprintf("Decision: %s\n",
  ifelse(p_pred["R1"] >= 0.80 & p_pred["R4"] <  0.20, "Go",
  ifelse(p_pred["R1"] <  0.80 & p_pred["R4"] >= 0.20, "NoGo",
  ifelse(p_pred["R1"] >= 0.80 & p_pred["R4"] >= 0.20, "Miss",
                                                        "Gray")))
))

## ----unctrl-post--------------------------------------------------------------
set.seed(1)
p_unctrl <- pbayespostpred2cont(
  prob = 'posterior', design = 'uncontrolled', prior = 'N-Inv-Wishart',
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  n_t = 20L, n_c = NULL,
  ybar_t = c(3.5, 2.1), S_t = S_t,
  ybar_c = NULL,        S_c = NULL,
  m_t = NULL, m_c = NULL,
  kappa0_t = 2.0, nu0_t = 5.0, mu0_t = c(2.0, 1.0), Lambda0_t = L0,
  kappa0_c = NULL, nu0_c = NULL, mu0_c = c(0.0, 0.0), Lambda0_c = NULL,
  r = 1.0,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  nMC = 5000L
)
print(round(p_unctrl, 4))

## ----ext-post-vague-----------------------------------------------------------
S_t     <- matrix(c(18.0, 3.6, 3.6, 9.0), 2, 2)
S_c     <- matrix(c(16.0, 2.8, 2.8, 8.5), 2, 2)
Se2_ext <- matrix(c(15.0, 2.5, 2.5, 7.5), 2, 2)

set.seed(2)
p_ext_vague <- pbayespostpred2cont(
  prob = 'posterior', design = 'external', prior = 'vague',
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  n_t = 20L, n_c = 20L,
  ybar_t = c(3.5, 2.1), S_t = S_t,
  ybar_c = c(1.8, 1.0), S_c = S_c,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, nu0_t = NULL, mu0_t = NULL, Lambda0_t = NULL,
  kappa0_c = NULL, nu0_c = NULL, mu0_c = NULL, Lambda0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = 10L, alpha0e_t = NULL, alpha0e_c = 0.5,
  bar_ye_t = NULL, bar_ye_c = c(1.5, 0.8), se_t = NULL, se_c = Se2_ext,
  nMC = 5000L
)
print(round(p_ext_vague, 4))

## ----ext-post-----------------------------------------------------------------
S_t     <- matrix(c(18.0, 3.6, 3.6, 9.0), 2, 2)
S_c     <- matrix(c(16.0, 2.8, 2.8, 8.5), 2, 2)
L0      <- matrix(c(8.0, 0.0, 0.0, 2.0), 2, 2)
Se2_ext <- matrix(c(15.0, 2.5, 2.5, 7.5), 2, 2)

set.seed(3)
p_ext <- pbayespostpred2cont(
  prob = 'posterior', design = 'external', prior = 'N-Inv-Wishart',
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  n_t = 20L, n_c = 20L,
  ybar_t = c(3.5, 2.1), S_t = S_t,
  ybar_c = c(1.8, 1.0), S_c = S_c,
  m_t = NULL, m_c = NULL,
  kappa0_t = 2.0, nu0_t = 5.0, mu0_t = c(2.0, 1.0), Lambda0_t = L0,
  kappa0_c = 2.0, nu0_c = 5.0, mu0_c = c(0.0, 0.0), Lambda0_c = L0,
  r = NULL,
  ne_t = NULL, ne_c = 10L, alpha0e_t = NULL, alpha0e_c = 0.5,
  bar_ye_t = NULL, bar_ye_c = c(1.5, 0.8), se_t = NULL, se_c = Se2_ext,
  nMC = 5000L
)
print(round(p_ext, 4))

## ----oc-controlled, fig.width = 8, fig.height = 6-----------------------------
Sigma <- matrix(c(4.0, 0.8, 0.8, 1.0), 2, 2)

mu_t1_seq <- seq(0.0, 3.5, by = 0.5)
mu_t2_seq <- seq(0.0, 2.1, by = 0.3)
n_scen    <- length(mu_t1_seq) * length(mu_t2_seq)

oc_ctrl <- pbayesdecisionprob2cont(
  nsim = 100L, prob = 'posterior', design = 'controlled',
  prior = 'vague',
  GoRegions = 1L, NoGoRegions = 9L,
  gamma_go = 0.80, gamma_nogo = 0.20,
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  n_t = 20L, n_c = 20L, m_t = NULL, m_c = NULL,
  mu_t    = cbind(rep(mu_t1_seq, times = length(mu_t2_seq)),
                  rep(mu_t2_seq, each  = length(mu_t1_seq))),
  Sigma_t = Sigma,
  mu_c    = matrix(0, nrow = n_scen, ncol = 2),
  Sigma_c = Sigma,
  kappa0_t = NULL, nu0_t = NULL, mu0_t = NULL, Lambda0_t = NULL,
  kappa0_c = NULL, nu0_c = NULL, mu0_c = NULL, Lambda0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  nMC = 500L, CalcMethod = 'MC',
  error_if_Miss = TRUE, Gray_inc_Miss = FALSE, seed = 42L
)
print(oc_ctrl)
plot(oc_ctrl, base_size = 20)

## ----getgamma-ctrl, fig.width = 8, fig.height = 6-----------------------------
res_gamma <- getgamma2cont(
  nsim = 500L, prob = 'posterior', design = 'controlled',
  prior = 'vague',
  GoRegions = 1L, NoGoRegions = 9L,
  mu_t_go    = c(0.5, 0.3), Sigma_t_go    = Sigma,
  mu_c_go    = c(0.0, 0.0), Sigma_c_go    = Sigma,
  mu_t_nogo  = c(1.0, 0.6), Sigma_t_nogo  = Sigma,
  mu_c_nogo  = c(0.0, 0.0), Sigma_c_nogo  = Sigma,
  target_go = 0.05, target_nogo = 0.20,
  n_t = 20L, n_c = 20L,
  theta_TV1  = 1.5, theta_MAV1 = 0.5,
  theta_TV2  = 1.0, theta_MAV2 = 0.3,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  m_t = NULL, m_c = NULL,
  kappa0_t = NULL, nu0_t = NULL, mu0_t = NULL, Lambda0_t = NULL,
  kappa0_c = NULL, nu0_c = NULL, mu0_c = NULL, Lambda0_c = NULL,
  r = NULL,
  ne_t = NULL, ne_c = NULL, alpha0e_t = NULL, alpha0e_c = NULL,
  bar_ye_t = NULL, bar_ye_c = NULL, se_t = NULL, se_c = NULL,
  nMC = 500L, CalcMethod = 'MC',
  gamma_go_grid   = seq(0.05, 0.95, by = 0.05),
  gamma_nogo_grid = seq(0.05, 0.95, by = 0.05),
  seed = 42L
)
plot(res_gamma, base_size = 20)

