library(tidyverse)
library(readr)
library(estimatr)
library(modelsummary)
library(kableExtra)
pacman::p_load(margins)

# データの読み込み・前処理
data81 <- read_csv("piaac.csv")
data81_felame <- data81 |> 
  filter(gender == "Female") |> 
  mutate(emp = if_else(lfs == "Employed", 1, 0)) |> 
  relocate(emp, .before = "contract")

glimpse(data81_felame)


# 表8-1の再現
# (1)
model1 <- lm_robust(emp ~ educ + age + couple + child,
                    data = data81_felame, se_type = "stata")
summary(model1)

# (2)
model2 <- glm(emp ~ educ + age + couple + child,
              data = data81_felame,
              family = binomial(link = "probit"))
summary(model2)

# (3)
model3 <- margins::margins(model2)
summary(model3)

# (4)
model4 <- glm(emp ~ educ + age + couple + child,
              data = data81_felame,
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


























