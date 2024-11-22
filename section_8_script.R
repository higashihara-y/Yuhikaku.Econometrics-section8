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

# データの読み込み・前処理
data8 <- read_csv("piaac.csv")
data8_felame <- data8 |> 
  filter(gender == "Female") |> 
  mutate(emp = if_else(lfs == "Employed", 1, 0)) |> 
  relocate(emp, .before = "contract")



# 表8-1の再現 -----------------------------------------------------------------

# (1)
model1 <- lm_robust(emp ~ educ + age + couple + child,
                    data = data8_felame, se_type = "stata")
summary(model1)

# (2)
model2 <- glm(emp ~ educ + age + couple + child,
              data = data8_felame,
              family = binomial(link = "probit"))
summary(model2)

# (3)
model3 <- margins::margins(model2)
summary(model3)

# (4)
model4 <- glm(emp ~ educ + age + couple + child,
              data = data8_felame,
              family = binomial(link = "logit"))
summary(model4)

# (5)
model5 <- margins::margins(model4)
summary(model5)


# R2/疑似R2などを追加し、表形式で出力
# modelsummaryによる、既存モデルへのカスタムの情報追加については以下URLを参照(glance_custom / tidy_custom)
# https://modelsummary.com/articles/modelsummary.html#adding-new-information-to-existing-models

models_81 <- list("(1)" = model1,
                  "(2)" = model2,
                  "(3)" = model3,
                  "(4)" = model4,
                  "(5)" = model5)

cm <- c("educ" = "教育年数",
        "age" = "年齢",
        "couple" = "配偶者有り",
        "child" = "子供数",
        "(Intercept)" = "定数項")

glance_custom.lm_robust <- function(x) {
  out <- tibble("adj.r.squared_or_pseudo.r.squared" = x$adj.r.squared)
  return(out)
}

glance_custom.glm <- function(x) {
  pseudo.r.squared <- 1 - (x$deviance / x$null.deviance)
  out <- tibble("adj.r.squared_or_pseudo.r.squared" = pseudo.r.squared)
  return(out)
}
# x$deviance = 全ての説明変数を含んだプロビットモデルの対数尤度の和
# x$null.deviance = 定数項だけを含んだプロビットモデルの対数尤度の和

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "adj.r.squared_or_pseudo.r.squared", "$\\bar{R}^2$/疑似$R^2$", 2,
  "nobs", "$N$", 0)

rows <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`,
  "推定方法", "OLS", "プロビット", "限界効果", "ロジット", "限界効果"
)

attr(rows, 'position') <- 1

modelsummary(models_81,
             coef_map = cm,
             gof_map = gm,
             add_rows = rows,
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             estimate = "{estimate}{stars}",
             output = "kableExtra",
             notes = "* p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001") |>
  row_spec(c(1, 13), extra_css = "border-bottom: 1.5px solid") |> 
  row_spec(11, extra_css = ";border-bottom: 1.5px solid")




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
summary(model82)

model82_marginal <- 
  marginaleffects::avg_slopes(model82,
                              df = insight::get_df(model82))

model82_marginal <- 
  modelsummary(model82_marginal,
               shape = term ~ group,
               output = "modelsummary_list",
               fmt = 5)

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

model82 <- list("モデル係数" = model82,
                "限界効果（％ポイント変化）" = model82_marginal )

cm <- c("educ" = "教育年数",
        "age" = "年齢",
        "couple" = "配偶者有り",
        "child" = "子供数",
        "Extremely dissatisfied|Dissatisfied" = "$\\mu_1$",
        "Dissatisfied|Neither satisfied nor dissatisfied" = "$\\mu_2$",
        "Neither satisfied nor dissatisfied|Satisfied" = "$\\mu_3$",
        "Satisfied|Extremely satisfied" = "$\\mu_4$")

glance_custom.polr <- function(x) {
  capture.output(McFadden <- pscl::pR2(x)["McFadden"])
  out <- tibble("pseudo.r.squared" = McFadden)
  return(out)
}

gm <- tribble(~raw, ~clean, ~fmt,
              "pseudo.r.squared", "疑似$R^2$", 3,
              "nobs", "$N$", 0)

modelsummary(model82,
             shape = term ~ group,
             coef_map = cm,
             gof_map = gm,
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             output = "kableExtra")




# 8-3 ---------------------------------------------------------------------

data8_felame <- data8_felame |> 
  mutate(empstat_edt = factor(empstat_edt,
                              labels = c("フル", "パート", "不就業")) |> 
           relevel(ref = 3))

model83 <- nnet::multinom(empstat_edt ~ educ + age + couple + child,
                          data = data8_felame, trace = FALSE)

model83_marginal <- 
  marginaleffects::avg_slopes(model83,
                              df = insight::get_df(model83))

glance_custom.multinom <- function(x) {
  capture.output(McFadden <- pscl::pR2(x)["McFadden"])
  out <- tibble("pseudo.r.squared" = McFadden)
  return(out)
}

model83 <- modelsummary(model83, output = "modelsummary_list")

model83$tidy <- model83$tidy |> 
  mutate(
    group = response
    )

models83 <- list("モデル係数" = model83,
                "限界効果" = model83_marginal)

cm <- c("educ" = "教育年数",
        "age" = "年齢",
        "couple" = "配偶者有り",
        "child" = "子供数",
        "(Intercept)" = "定数項")

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "pseudo.r.squared", "疑似$R^2$", 3,
  "nobs", "$N$", 0)

modelsummary(models83,
             shape = term ~ group,
             coef_map = cm,
             gof_map = gm,
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             output = "kableExtra")




# 8-4 ---------------------------------------------------------------------

model84_OLS <- lm(hours ~ educ + age + couple + child,
                  data = data8_felame,
                  y = TRUE)

model84_tobit <- AER::tobit(hours ~ educ + age + couple + child,
                            data = data8_felame,
                            left = 0)

models84 <- list("(1)" = model84_OLS,
                 "(2)" = model84_tobit)

rows <- tribble(
  ~term, ~`(1)`, ~ `(2)`,
  "推定方法", "OLS", "トービット"
)

attr(rows, "position") <- 1

cm <- c("educ" = "教育年数",
        "age" = "年齢",
        "couple" = "配偶者有り",
        "child" = "子供数",
        "(Intercept)" = "定数項")

glance_custom.tobit <- function(x) {
  out <- tibble("r.squared" = 1 - x$loglik[2] / x$loglik[1],
                "nobs_zero" = sum(as.character(x$y) == "  0-"))
  return(out)
}

glance_custom.lm <- function(x) {
  out <- tibble("nobs_zero" = sum(x$y == 0))
  return(out)
}

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "r.squared", "$R^2$/疑似$R^2$", 3,
  "nobs", "$N$", 0,
  "nobs_zero", "うち0時間", 0
)

modelsummary(models84,
             coef_map = cm,
             gof_map = gm,
             add_rows = rows,
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             output = "kableExtra")













































