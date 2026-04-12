install.packages("psych")
install.packages("corrplot")

library(psych)
library(corrplot)

df <- read.csv(
  "C:\\Users\\siddh\\Downloads\\Billionaires Statistics Dataset.csv",
  stringsAsFactors = FALSE
)

df$gdp_country_numeric <- as.numeric(gsub("[\\$, ]", "", df$gdp_country))

df$log_net_worth <- log(df$finalWorth)

corr_data <- df[, c(
  "log_net_worth",
  "age",
  "cpi_country",
  "cpi_change_country",
  "gdp_country_numeric",
  "gross_tertiary_education_enrollment",
  "gross_primary_education_enrollment_country",
  "life_expectancy_country",
  "tax_revenue_country_country",
  "total_tax_rate_country",
  "population_country"
)]

corr_data <- na.omit(corr_data)

colnames(corr_data) <- c(
  "Log Net Worth",
  "Age",
  "CPI",
  "CPI Change",
  "GDP",
  "Tertiary Education",
  "Primary Education",
  "Life Expectancy",
  "Tax Revenue",
  "Total Tax Rate",
  "Population"
)

cor_matrix <- cor(corr_data, method = "pearson")
print(round(cor_matrix, 3))

write.csv(round(cor_matrix, 3), "section_4_1_correlation_matrix.csv", row.names = TRUE)

png(
  filename = "section_4_1_pairs_panels_full.png",
  width = 2400,
  height = 2400,
  res = 220
)

pairs.panels(
  corr_data,
  method = "pearson",
  hist.col = "grey85",
  density = FALSE,
  ellipses = FALSE,
  lm = TRUE,
  stars = FALSE,
  cex.cor = 1.0,
  pch = 21,
  bg = "white",
  gap = 0.4,
  main = "Correlations between Log Net Worth and Other Continuous Variables"
)

dev.off()

pairs.panels(
  corr_data,
  method = "pearson",
  hist.col = "grey85",
  density = FALSE,
  ellipses = FALSE,
  lm = TRUE,
  stars = FALSE,
  cex.cor = 1.0,
  pch = 21,
  bg = "white",
  gap = 0.4,
  main = "Correlations between Log Net Worth and Other Continuous Variables"
)

png(
  filename = "section_4_1_corrplot_full.png",
  width = 2200,
  height = 1800,
  res = 220
)

corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  diag = TRUE,
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.0,
  number.cex = 0.85,
  mar = c(1, 1, 2, 1),
  title = "Correlation Matrix of Log Net Worth and Continuous Variables"
)

dev.off()

corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  diag = TRUE,
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.0,
  number.cex = 0.85,
  mar = c(1, 1, 2, 1),
  title = "Correlation Matrix of Log Net Worth and Continuous Variables"
)

target_correlations <- cor_matrix["Log Net Worth", ]
print(round(target_correlations, 3))

write.csv(
  round(target_correlations, 3),
  "section_4_1_log_net_worth_correlations.csv",
  row.names = TRUE
)

cor.test(corr_data$`Log Net Worth`, corr_data$Age)
cor.test(corr_data$`Log Net Worth`, corr_data$CPI)
cor.test(corr_data$`Log Net Worth`, corr_data$`CPI Change`)
cor.test(corr_data$`Log Net Worth`, corr_data$GDP)
cor.test(corr_data$`Log Net Worth`, corr_data$`Tertiary Education`)
cor.test(corr_data$`Log Net Worth`, corr_data$`Primary Education`)
cor.test(corr_data$`Log Net Worth`, corr_data$`Life Expectancy`)
cor.test(corr_data$`Log Net Worth`, corr_data$`Tax Revenue`)
cor.test(corr_data$`Log Net Worth`, corr_data$`Total Tax Rate`)
cor.test(corr_data$`Log Net Worth`, corr_data$Population)

png(
  filename = "section_4_1_scatterplots_key.png",
  width = 1800,
  height = 1800,
  res = 220
)

par(mfrow = c(2, 2))

plot(corr_data$Age, corr_data$`Log Net Worth`,
     main = "Log Net Worth vs Age",
     xlab = "Age", ylab = "Log Net Worth",
     pch = 19, cex = 0.6)
abline(lm(`Log Net Worth` ~ Age, data = corr_data), lwd = 2)

plot(corr_data$GDP, corr_data$`Log Net Worth`,
     main = "Log Net Worth vs GDP",
     xlab = "GDP", ylab = "Log Net Worth",
     pch = 19, cex = 0.6)
abline(lm(`Log Net Worth` ~ GDP, data = corr_data), lwd = 2)

plot(corr_data$CPI, corr_data$`Log Net Worth`,
     main = "Log Net Worth vs CPI",
     xlab = "CPI", ylab = "Log Net Worth",
     pch = 19, cex = 0.6)
abline(lm(`Log Net Worth` ~ CPI, data = corr_data), lwd = 2)

plot(corr_data$`Total Tax Rate`, corr_data$`Log Net Worth`,
     main = "Log Net Worth vs Total Tax Rate",
     xlab = "Total Tax Rate", ylab = "Log Net Worth",
     pch = 19, cex = 0.6)
abline(lm(`Log Net Worth` ~ `Total Tax Rate`, data = corr_data), lwd = 2)

dev.off()

par(mfrow = c(1, 1))