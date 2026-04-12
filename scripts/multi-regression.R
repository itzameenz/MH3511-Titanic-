install.packages("ggplot2")
install.packages("dplyr")
install.packages("forcats")
install.packages("stringr")
install.packages("car")
install.packages("broom")

library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(car)
library(broom)

df <- read.csv(
  "C:\\Users\\siddh\\Downloads\\Billionaires Statistics Dataset.csv",
  stringsAsFactors = FALSE
)

df$log_net_worth <- log(df$finalWorth)

df$gdp_country_numeric <- as.numeric(gsub("[\\$, ]", "", df$gdp_country))
df$log_gdp_country <- log(df$gdp_country_numeric)

df$selfMade_label <- ifelse(df$selfMade %in% c(TRUE, "TRUE", "True", 1),
                            "Self-made", "Not self-made")
df$selfMade_label <- factor(df$selfMade_label,
                            levels = c("Not self-made", "Self-made"))

df$gender_label <- ifelse(df$gender == "M", "Male",
                          ifelse(df$gender == "F", "Female", NA))
df$gender_label <- factor(df$gender_label, levels = c("Female", "Male"))

df$category <- as.factor(df$category)

df$country <- as.character(df$country)
top10_country <- names(sort(table(df$country), decreasing = TRUE)[1:10])
df$country_top10 <- ifelse(df$country %in% top10_country, df$country, "Other")
df$country_top10 <- factor(df$country_top10)

model_df <- df %>%
  select(
    log_net_worth,
    age,
    selfMade_label,
    gender_label,
    category,
    country_top10,
    log_gdp_country,
    cpi_country,
    total_tax_rate_country
  ) %>%
  na.omit()

model_df$selfMade_label <- relevel(model_df$selfMade_label, ref = "Not self-made")
model_df$gender_label <- relevel(model_df$gender_label, ref = "Female")
model_df$country_top10 <- relevel(model_df$country_top10, ref = "Other")

model_full <- lm(
  log_net_worth ~ age + selfMade_label + gender_label +
    category + country_top10 + log_gdp_country +
    cpi_country + total_tax_rate_country,
  data = model_df
)

cat("FULL MODEL SUMMARY\n")
print(summary(model_full))

model_final <- step(model_full, direction = "backward", trace = FALSE)

cat("FINAL MODEL SUMMARY\n")
print(summary(model_final))

cat("ANOVA TABLE FOR FINAL MODEL\n")
print(anova(model_final))

cat("VIF VALUES\n")
print(vif(model_final))

tidy_model <- tidy(model_final, conf.int = TRUE)
glance_model <- glance(model_final)

write.csv(tidy_model, "section_4_3_final_model_coefficients.csv", row.names = FALSE)
write.csv(glance_model, "section_4_3_final_model_summary.csv", row.names = FALSE)

aug_df <- augment(model_final)

theme_custom <- function() {
  theme_gray(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90")
    )
}

p_actual_fitted <- ggplot(aug_df, aes(x = .fitted, y = log_net_worth)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Observed versus Fitted Values for Final Regression Model",
    x = "Fitted log-transformed net worth",
    y = "Observed log-transformed net worth"
  ) +
  theme_custom()

ggsave("section_4_3_actual_vs_fitted.png", p_actual_fitted, width = 9, height = 7, dpi = 300)
print(p_actual_fitted)

p_resid_fitted <- ggplot(aug_df, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, method = "loess", color = "black") +
  labs(
    title = "Residuals versus Fitted Values",
    x = "Fitted log-transformed net worth",
    y = "Residuals"
  ) +
  theme_custom()

ggsave("section_4_3_residuals_vs_fitted.png", p_resid_fitted, width = 9, height = 7, dpi = 300)
print(p_resid_fitted)

p_qq <- ggplot(aug_df, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5, size = 1.2) +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Standardized Residuals",
    x = "Theoretical quantiles",
    y = "Standardized residuals"
  ) +
  theme_custom()

ggsave("section_4_3_qqplot.png", p_qq, width = 9, height = 7, dpi = 300)
print(p_qq)

p_scale_loc <- ggplot(aug_df, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(se = FALSE, method = "loess", color = "black") +
  labs(
    title = "Scale-Location Plot",
    x = "Fitted log-transformed net worth",
    y = "Square root of |standardized residuals|"
  ) +
  theme_custom()

ggsave("section_4_3_scale_location.png", p_scale_loc, width = 9, height = 7, dpi = 300)
print(p_scale_loc)

coef_plot_df <- tidy_model %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate))

p_coef <- ggplot(coef_plot_df, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Estimated Coefficients in the Final Regression Model",
    x = "Estimated coefficient (95% confidence interval)",
    y = "Predictor"
  ) +
  theme_custom()

ggsave("section_4_3_coefficient_plot.png", p_coef, width = 10, height = 8, dpi = 300)
print(p_coef)
