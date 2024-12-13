library(tidyverse)
library(readr)
library(estimatr)
library(modelsummary)
library(kableExtra)
pacman::p_load(margins)
pacman::p_load(MASS)
pacman::p_load(marginaleffects)
pacman::p_load(pscl)
pacman::p_load(nnet)
pacman::p_load(sampleSelection)

data8 <- read_csv("piaac.csv")
glimpse(data8)

data8_fem <- data8 |> 
  filter(gender == "Female") |> 
  mutate(emp = ifelse(lfs == "Employed", 1, 0)) |> 
  relocate(emp, .before = lfs)

# 表8-1の再現 -----------------------------------------------------------------

model8_1 <-  lm_robust(emp ~ educ + age + couple + child,
                       data = data8_fem, se_type = "stata")
model8_2 <-  glm(emp ~ educ + age + couple + child,
                 family = binomial(link = "probit"), data = data8_fem)
model8_3 <- margins::margins(model8_2)
model8_4 <- glm(emp ~ educ + age + couple + child,
                family =  binomial(link = "logit"), data = data8_fem)
model8_5 <- margins::margins(model8_4)

models8 <- list(
  "(1)" = model8_1,
  "(2)" = model8_2,
  "(3)" = model8_3,
  "(4)" = model8_4,
  "(5)" = model8_5
)

cm <- c("educ" = "教育年数",
        "age" = "年齢",
        "couple" = "配偶者あり",
        "child" = "子供数",
        "(Intercept)" = "定数項")

glance_custom.lm_robust <- function(x) {
  out <- tibble("r.squared_or_pseudo.r.squared" = x$adj.r.squared)
  return(out)
}

glance_custom.glm <- function(x) {
  r.squared = 1 - (x$deviance / x$null.deviance)
  out <- tibble("r.squared_or_pseudo.r.squared" = r.squared)
  return(out)
}

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "r.squared_or_pseudo.r.squared", "$\\bar{R}^2 / 疑似R^2$", 2,
  "nobs", "$N$", 0
)

rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "推定方法", "OLS", "プロビット", "限界効果", "ロジット", "限界効果"
)

attr(rows, "position") <- 1

modelsummary::modelsummary(
  models = models8,
  coef_map = cm,
  gof_map = gm,
  add_rows = rows,
  estimate = "{estimate}{stars}",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  notes = "* p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001",
  output = "kableExtra"
) |> 
  kableExtra::row_spec(c(1, 13), extra_css = "border-bottom: 1.5px solid")



# 8-2 ---------------------------------------------------------------------

data8_male <- data8 |> 
  filter(gender == "Male") |> 
  mutate(jsrev = factor(js,
                        levels = c("Extremely dissatisfied",
                                   "Dissatisfied",
                                   "Neither satisfied nor dissatisfied",
                                   "Satisfied",
                                   "Extremely satisfied"),
                        ordered = TRUE)) |> 
  relocate(jsrev, .after = "js")

model82 <- MASS::polr(jsrev ~ educ + age + couple + child,
                      data = data8_male, method = "probit")

model82_marginal <- 
  marginaleffects::avg_slopes(model82,
                              df = insight::get_df(model82))

model82_marginal <- 
  modelsummary::modelsummary(
    model82_marginal,
    shape = term ~ group,
    output = "modelsummary_list",
    fmt = 5
)

model82_marginal$tidy <- model82_marginal$tidy |> 
  mutate(estimate = estimate * 100,
         std.error = std.error * 100,
         conf.low = conf.low * 100,
         conf.high - conf.high * 100) |> 
  mutate(group = factor(group,
                        levels = c("Extremely dissatisfied",
                                   "Dissatisfied",
                                   "Neither satisfied nor dissatisfied",
                                   "Satisfied",
                                   "Extremely satisfied"),
                        labels = c("とても不満",
                                   "不満",
                                   "どちらでもない",
                                   "満足",
                                   "とても満足"),
                        ordered = TRUE))

model82_marginal$glance <- NULL

model82_comb <- list(
  "モデル係数" = model82,
  "限界効果（％ポイント変化）" = model82_marginal
)

cm <- c(
  "educ" = "教育年数",
  "age" = "年齢",
  "couple" = "配偶者あり",
  "child" = "子供数",
  "Extremely dissatisfied|Dissatisfied" = "$\\mu_1$",
  "Dissatisfied|Neither satisfied nor dissatisfied" = "$\\mu_2$",
  "Neither satisfied nor dissatisfied|Satisfied" = "$\\mu_3$",
  "Satisfied|Extremely satisfied" = "$\\mu_4$"
)

glance_custom.polr <- function(x) {
  McFadden <- pscl::pR2(x)["McFadden"]
  out <- tibble("pseudo.r.squared" = McFadden)
}

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "pseudo.r.squared", "疑似$R^2$", 3,
  "nobs", "$N$", 0
)

modelsummary::msummary(
  models = model82_comb,
  shape = term ~ group,
  coef_map = cm,
  gof_map = gm,
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  estimate = "{estimate}{stars}"
)



# 8-3 ---------------------------------------------------------------------

data8_fem <- data8_fem |> 
  mutate(empstat_edt = factor(empstat_edt,
                              labels = c("フル", "パート", "不就業")) |> 
         relevel(ref = 3))
  
model83 <- nnet::multinom(empstat_edt ~ educ + age + couple + child,
                          data = data8_fem, trace = FALSE)

model83_marginal <- 
  marginaleffects::avg_slopes(model83, df = insight::get_df(model83))

glance_custom.multinom <- function(x) {
  McFadden <- pscl::pR2(x)["McFadden"]
  out <- tibble("pseudo.r.squared" = McFadden)
  return(out)
}

model83 <- modelsummary(model83, output = "modelsummary_list")
model83$tidy <- model83$tidy |> 
  mutate(group = response)

model83_comb <- list(
  "モデル係数" = model83,
  "限界効果" = model83_marginal
)

cm <- c(
  "educ" = "教育年数",
  "age" = "年齢",
  "couple" = "配偶者あり",
  "child" = "子供数",
  "(Intercept)" = "定数項"
)

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "pseudo.r.squared", "疑似$R^2$", 3,
  "nobs", "$N$", 0
)

# t検定の星の注釈は自動で下部に入っている
# estimateのパラメータも特に必要なし、デフォルトで問題なし
modelsummary::modelsummary(
  model83_comb,
  shape = term ~ group,
  coef_map = cm,
  gof_map = gm,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output = "kableExtra"
)



# 8-4 ---------------------------------------------------------------------

#y = TRUEを追加して、yを算出する 
model84_ols <- lm(hours ~ educ + age + couple + child,
                  data = data8_fem, y = TRUE)

model84_tobit <- AER::tobit(hours ~ educ + age + couple + child,
                            data = data8_fem, left = 0)

model84_comb <- list("(1)" = model84_ols,
                     "(2)" = model84_tobit)

glance_custom.lm <- function(x) {
  out <- tibble("nobs_zero" = sum(x$y == 0))
  return(out)
}

glance_custom.tobit <- function(x) {
  out <- tibble("r.squared" = 1 - x$loglik[2] / x$loglik[1],
                "nobs_zero" = sum(as.character(x$y) == " 0-"))
  return(out)
}

cm <- c( "educ" = "教育年数",
         "age" = "年齢",
         "couple" = "配偶者あり",
         "child" = "子供数",
         "(Intercept)" = "定数項")

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "r.squared", "$R^2 / 疑似R^2$", 3,
  "nobs", "$N$", 0,
  "nobs_zero", "うち0時間", 0
)

rows <- tribble(
  ~term, ~"(1)", ~"(2)",
  "推定方法", "OLS", "トービット"
)

attr(rows, "position") <- 1

modelsummary::msummary(
  models = model84_comb,
  coef_map = cm,
  gof_map = gm,
  add_rows = rows,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output = "kableExtra"
)



# 8-5 ---------------------------------------------------------------------

data8_fem <- data8_fem |> 
  drop_na(educ, age, couple, child) |> 
  mutate(lwage = log(wage),
         exp = age - educ - 6,
         exp2 = exp^2 / 100,
         selected = !is.na(wage))

# OLS
model85_ols <- lm(lwage ~ educ + exp + exp2, data = data8_fem)
model85_ols <- modelsummary(model85_ols, output = "modelsummary_list")
model85_ols$tidy$component <- "賃金式"

# 2段階へキット
model85_2step <- sampleSelection::heckit(
  selected ~ educ + exp + exp2 + couple + child,
  lwage ~ educ + exp + exp2,
  data = data8_fem, method = "2step"
)
model85_2step <- modelsummary(model85_2step, output = "modelsummary_list")
model85_2step$tidy <- model85_2step$tidy |> 
  mutate(component = ifelse(
    component == "selection", "セレクション式", "賃金式"
    ))

# 最尤法へキット
model85_ml <- sampleSelection::heckit(
  selected ~ educ + exp + exp2 + couple + child,
  lwage ~ educ + exp + exp2,
  data = data8_fem, method = "ml"
)
model85_ml <- modelsummary(model85_ml, output = "modelsummary_list")
model85_ml$tidy <- model85_ml$tidy |> 
  mutate(component = ifelse(
    component == "selection", "セレクション式", "最尤法へキット"
  ))

model85_comb <- list(
  "OLS" = model85_ols,
  "2段階へキット" = model85_2step,
  "最尤法へキット" = model85_ml
)

cm <- c(
  "educ" = "教育年数",
  "exp" = "経験年数",
  "exp2" = "経験年数2乗/100",
  "couple" = "配偶者あり",
  "child" = "子供数",
  "(Intercept)" = "定数項",
  "invMillsRatio" = "逆ミルズ比",
  "rho" = "誤差項の相関"
)

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "r.squared", "$\\bar{R}^2/疑似R^2$", 2,
  "nobs", "$N$", 0
)

modelsummary(
  models = model85_comb,
  shape = term ~ component,
  coef_map = cm,
  gof_map = gm,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output = "kableExtra"
) |> 
  kableExtra::row_spec(c(12, 18), 
                       extra_css = "border-bottom: 1.5px solid")











































