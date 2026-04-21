## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5.5,
  fig.path = "figures/2bin-"
)
library(BayesianQDM)

## ----getjointbin--------------------------------------------------------------
# Convert marginal rates + correlation to cell probabilities
getjointbin(pi1 = 0.30, pi2 = 0.35, rho = 0.20)
getjointbin(pi1 = 0.20, pi2 = 0.20, rho = 0.00)   # independence

## ----nine-region-bin, echo = FALSE, results = 'asis'--------------------------
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

## ----four-region-bin, echo = FALSE, results = 'asis'--------------------------
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

## ----ctrl-post----------------------------------------------------------------
set.seed(42)
p_post_ctrl <- pbayespostpred2bin(
  prob = 'posterior', design = 'controlled',
  theta_TV1 = 0.20, theta_MAV1 = 0.10,
  theta_TV2 = 0.20, theta_MAV2 = 0.10,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  x_t_00 = 1L, x_t_01 = 1L, x_t_10 = 2L, x_t_11 = 3L,
  x_c_00 = 2L, x_c_01 = 1L, x_c_10 = 2L, x_c_11 = 2L,
  a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
  a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
  m_t = NULL, m_c = NULL,
  z00 = NULL, z01 = NULL, z10 = NULL, z11 = NULL,
  xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
  xe_c_00 = NULL, xe_c_01 = NULL, xe_c_10 = NULL, xe_c_11 = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  nMC = 1000L
)
print(round(p_post_ctrl, 4))
cat(sprintf(
  "\nGo  region (R1): P = %.4f  >= gamma_go (0.80)? %s\n",
  p_post_ctrl["R1"], ifelse(p_post_ctrl["R1"] >= 0.80, "YES -> Go", "NO")
))
cat(sprintf(
  "NoGo region (R9): P = %.4f  >= gamma_nogo (0.80)? %s\n",
  p_post_ctrl["R9"], ifelse(p_post_ctrl["R9"] >= 0.80, "YES -> NoGo", "NO")
))

## ----ctrl-pred----------------------------------------------------------------
set.seed(42)
p_pred_ctrl <- pbayespostpred2bin(
  prob = 'predictive', design = 'controlled',
  theta_TV1 = NULL, theta_MAV1 = NULL,
  theta_TV2 = NULL, theta_MAV2 = NULL,
  theta_NULL1 = 0.15, theta_NULL2 = 0.15,
  x_t_00 = 1L, x_t_01 = 1L, x_t_10 = 2L, x_t_11 = 3L,
  x_c_00 = 2L, x_c_01 = 1L, x_c_10 = 2L, x_c_11 = 2L,
  a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
  a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
  m_t = 15L, m_c = 15L,
  z00 = NULL, z01 = NULL, z10 = NULL, z11 = NULL,
  xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
  xe_c_00 = NULL, xe_c_01 = NULL, xe_c_10 = NULL, xe_c_11 = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  nMC = 1000L
)
print(round(p_pred_ctrl, 4))
cat(sprintf(
  "\nGo region (R1): P = %.4f  >= gamma_go (0.80)? %s\n",
  p_pred_ctrl["R1"], ifelse(p_pred_ctrl["R1"] >= 0.80, "YES -> Go", "NO")
))

## ----unctrl-post--------------------------------------------------------------
set.seed(1)
p_unctrl <- pbayespostpred2bin(
  prob = 'posterior', design = 'uncontrolled',
  theta_TV1 = 0.20, theta_MAV1 = 0.10,
  theta_TV2 = 0.20, theta_MAV2 = 0.10,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  x_t_00 = 1L, x_t_01 = 1L, x_t_10 = 2L, x_t_11 = 3L,
  x_c_00 = NULL, x_c_01 = NULL, x_c_10 = NULL, x_c_11 = NULL,
  a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
  a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
  m_t = NULL, m_c = NULL,
  z00 = 2L, z01 = 1L, z10 = 2L, z11 = 1L,
  xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
  xe_c_00 = NULL, xe_c_01 = NULL, xe_c_10 = NULL, xe_c_11 = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  nMC = 1000L
)
print(round(p_unctrl, 4))

## ----ext-post-----------------------------------------------------------------
set.seed(2)
p_ext <- pbayespostpred2bin(
  prob = 'posterior', design = 'external',
  theta_TV1 = 0.20, theta_MAV1 = 0.10,
  theta_TV2 = 0.20, theta_MAV2 = 0.10,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  x_t_00 = 1L, x_t_01 = 1L, x_t_10 = 2L, x_t_11 = 3L,
  x_c_00 = 2L, x_c_01 = 1L, x_c_10 = 2L, x_c_11 = 2L,
  a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
  a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
  m_t = NULL, m_c = NULL,
  z00 = NULL, z01 = NULL, z10 = NULL, z11 = NULL,
  xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
  xe_c_00 = 3L,   xe_c_01 = 1L,   xe_c_10 = 2L,   xe_c_11 = 1L,
  alpha0e_t = NULL, alpha0e_c = 0.5,
  nMC = 1000L
)
print(round(p_ext, 4))

## ----ext-borrowing------------------------------------------------------------
ae_seq <- c(0.01, seq(0.1, 1.0, by = 0.1))
p_ae <- sapply(ae_seq, function(ae) {
  set.seed(99)
  res <- pbayespostpred2bin(
    prob = 'posterior', design = 'external',
    theta_TV1 = 0.20, theta_MAV1 = 0.10,
    theta_TV2 = 0.20, theta_MAV2 = 0.10,
    theta_NULL1 = NULL, theta_NULL2 = NULL,
    x_t_00 = 1L, x_t_01 = 1L, x_t_10 = 2L, x_t_11 = 3L,
    x_c_00 = 2L, x_c_01 = 1L, x_c_10 = 2L, x_c_11 = 2L,
    a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
    a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
    m_t = NULL, m_c = NULL,
    z00 = NULL, z01 = NULL, z10 = NULL, z11 = NULL,
    xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
    xe_c_00 = 3L,   xe_c_01 = 1L,   xe_c_10 = 2L,   xe_c_11 = 1L,
    alpha0e_t = NULL, alpha0e_c = ae, nMC = 500L
  )
  res["R1"]
})
data.frame(alpha0e_c = ae_seq, P_R1 = round(p_ae, 4))

## ----oc-controlled, fig.width = 8, fig.height = 6-----------------------------
pi_t_seq <- seq(0.20, 0.90, by = 0.10)
n_scen   <- length(pi_t_seq)

oc_ctrl <- pbayesdecisionprob2bin(
  prob = 'posterior', design = 'controlled',
  GoRegions = 1L, NoGoRegions = 9L,
  gamma_go = 0.80, gamma_nogo = 0.80,
  pi_t1 = rep(pi_t_seq, each = n_scen),
  pi_t2 = rep(pi_t_seq, times = n_scen),
  rho_t = rep(0.0, n_scen * n_scen),
  pi_c1 = rep(0.20, n_scen * n_scen),
  pi_c2 = rep(0.20, n_scen * n_scen),
  rho_c = rep(0.0,  n_scen * n_scen),
  n_t = 7L, n_c = 7L,
  a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
  a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
  m_t = NULL, m_c = NULL,
  theta_TV1  = 0.20, theta_MAV1 = 0.10,
  theta_TV2  = 0.20, theta_MAV2 = 0.10,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  z00 = NULL, z01 = NULL, z10 = NULL, z11 = NULL,
  xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
  xe_c_00 = NULL, xe_c_01 = NULL, xe_c_10 = NULL, xe_c_11 = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  nMC = 200L, CalcMethod = 'Exact',
  error_if_Miss = TRUE, Gray_inc_Miss = FALSE
)
print(oc_ctrl)
plot(oc_ctrl, base_size = 20)

## ----getgamma-ctrl, fig.width = 8, fig.height = 6-----------------------------
res_gamma <- getgamma2bin(
  prob = 'posterior', design = 'controlled',
  GoRegions = 1L, NoGoRegions = 9L,
  pi_t1_go = 0.20, pi_t2_go = 0.20, rho_t_go = 0.0,
  pi_c1_go = 0.20, pi_c2_go = 0.20, rho_c_go = 0.0,
  pi_t1_nogo = 0.40, pi_t2_nogo = 0.40, rho_t_nogo = 0.0,
  pi_c1_nogo = 0.20, pi_c2_nogo = 0.20, rho_c_nogo = 0.0,
  target_go = 0.05, target_nogo = 0.20,
  n_t = 7L, n_c = 7L,
  a_t_00 = 0.25, a_t_01 = 0.25, a_t_10 = 0.25, a_t_11 = 0.25,
  a_c_00 = 0.25, a_c_01 = 0.25, a_c_10 = 0.25, a_c_11 = 0.25,
  theta_TV1  = 0.20, theta_MAV1 = 0.10,
  theta_TV2  = 0.20, theta_MAV2 = 0.10,
  theta_NULL1 = NULL, theta_NULL2 = NULL,
  m_t = NULL, m_c = NULL,
  z00 = NULL, z01 = NULL, z10 = NULL, z11 = NULL,
  xe_t_00 = NULL, xe_t_01 = NULL, xe_t_10 = NULL, xe_t_11 = NULL,
  xe_c_00 = NULL, xe_c_01 = NULL, xe_c_10 = NULL, xe_c_11 = NULL,
  alpha0e_t = NULL, alpha0e_c = NULL,
  nMC = 200L,
  gamma_go_grid   = seq(0.05, 0.95, by = 0.05),
  gamma_nogo_grid = seq(0.05, 0.95, by = 0.05)
)
plot(res_gamma, base_size = 20)

