install.packages(c("ggplot2", "dplyr", "broom"))

library(ggplot2)
library(dplyr)
library(broom)

df <- read.csv(
  "C:\\Users\\siddh\\Downloads\\Billionaires Statistics Dataset.csv",
  stringsAsFactors = FALSE
)

df$log_net_worth <- log(df$finalWorth)

df$gdp_country_numeric <- as.numeric(gsub("[\\$, ]", "", df$gdp_country))
df$log_gdp_country <- log(df$gdp_country_numeric)

df_age <- df %>%
  select(log_net_worth, age) %>%
  na.omit()
model_age <- lm(log_net_worth ~ age, data = df_age)

df_gdp <- df %>%
  select(log_net_worth, log_gdp_country) %>%
  na.omit()
model_gdp <- lm(log_net_worth ~ log_gdp_country, data = df_gdp)

df_cpi <- df %>%
  select(log_net_worth, cpi_country) %>%
  na.omit()
model_cpi <- lm(log_net_worth ~ cpi_country, data = df_cpi)

df_tax <- df %>%
  select(log_net_worth, total_tax_rate_country) %>%
  na.omit()
model_tax <- lm(log_net_worth ~ total_tax_rate_country, data = df_tax)

theme_custom <- function() {
  theme_gray(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90")
    )
}

aug_age <- augment(model_age)
aug_gdp <- augment(model_gdp)
aug_cpi <- augment(model_cpi)
aug_tax <- augment(model_tax)

make_qq_plot <- function(data, title_text, filename) {
  p <- ggplot(data, aes(sample = .std.resid)) +
    stat_qq(size = 1.2, alpha = 0.5) +
    stat_qq_line() +
    labs(
      title = title_text,
      x = "Theoretical quantiles",
      y = "Standardized residuals"
    ) +
    theme_custom()
  
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  print(p)
}

make_qq_plot(
  data = aug_age,
  title_text = "Q-Q Plot of Residuals: Age Model",
  filename = "section_4_2_6_qq_age.png"
)

make_qq_plot(
  data = aug_gdp,
  title_text = "Q-Q Plot of Residuals: Log GDP Model",
  filename = "section_4_2_6_qq_log_gdp.png"
)

make_qq_plot(
  data = aug_cpi,
  title_text = "Q-Q Plot of Residuals: CPI Model",
  filename = "section_4_2_6_qq_cpi.png"
)

make_qq_plot(
  data = aug_tax,
  title_text = "Q-Q Plot of Residuals: Total Tax Rate Model",
  filename = "section_4_2_6_qq_tax_rate.png"
)

par(mfrow = c(1, 1))
