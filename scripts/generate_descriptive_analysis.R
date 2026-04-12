library(ggplot2)
library(dplyr)

data_path <- file.path("data", "processed", "billionaires_analysis_ready.csv")
data <- read.csv(data_path, stringsAsFactors = FALSE)

out_num <- file.path("output", "descriptive", "numeric")
out_cat <- file.path("output", "descriptive", "categorical")
dir.create(out_num, recursive = TRUE, showWarnings = FALSE)
dir.create(out_cat, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 13))

# ── Helpers ────────────────────────────────────────────────────────────────────

save_plot <- function(p, filename, dir, w = 8, h = 5) {
  ggsave(file.path(dir, filename), plot = p, width = w, height = h, dpi = 150)
}

hist_plot <- function(col, xlabel, title) {
  x <- data[[col]]
  x <- x[is.finite(x)]
  df <- data.frame(x = x)
  ggplot(df, aes(x = x)) +
    geom_histogram(fill = "#5B8FF9", colour = "white", bins = 40) +
    labs(title = title, x = xlabel, y = "Count")
}

box_plot <- function(col, ylabel, title, filename, dir) {
  x <- as.numeric(data[[col]])
  x <- x[is.finite(x)]
  png(file.path(dir, filename), width = 600, height = 600)
  boxplot(x, main = title, ylab = ylabel)
  dev.off()
}

bar_plot <- function(col, top_n = 15, title, xlabel = col) {
  x <- data[[col]]
  x[is.na(x) | x == ""] <- NA
  df <- data.frame(x = x) %>%
    filter(!is.na(x)) %>%
    count(x, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    mutate(x = reorder(x, n))
  ggplot(df, aes(x = n, y = x)) +
    geom_col(fill = "#5B8FF9") +
    labs(title = title, x = "Count", y = xlabel)
}

raw_vs_transformed <- function(raw_col, trans_col, raw_label, trans_label, title) {
  df_raw <- data.frame(value = data[[raw_col]][is.finite(data[[raw_col]])], type = raw_label)
  df_trans <- data.frame(value = data[[trans_col]][is.finite(data[[trans_col]])], type = trans_label)
  df <- bind_rows(df_raw, df_trans)
  ggplot(df, aes(x = value)) +
    geom_histogram(fill = "#5B8FF9", colour = "white", bins = 40) +
    facet_wrap(~type, scales = "free") +
    labs(title = title, x = "Value", y = "Count")
}

# ── Numeric columns ─────────────────────────────────────────────────────────

# log_net_worth
save_plot(hist_plot("log_net_worth", "Log Net Worth", "Distribution of Log Net Worth"),
          "log_net_worth_hist.png", out_num)
box_plot("log_net_worth", "Log Net Worth", "Log Net Worth Boxplot",
         "log_net_worth_box.png", out_num)

# age
save_plot(hist_plot("age", "Age", "Distribution of Age"),
          "age_hist.png", out_num)
box_plot("age", "Age", "Age Distribution",
         "age_box.png", out_num)

# CPI
save_plot(raw_vs_transformed("cpi_country", "log_cpi_country",
          "CPI (Raw)", "CPI (Log)", "CPI: Raw vs Log Transform"),
          "cpi_raw_vs_log.png", out_num, w = 10, h = 5)

# CPI change
save_plot(raw_vs_transformed("cpi_change_country", "log_cpi_change_country",
          "CPI Change (Raw)", "CPI Change (Log)", "CPI Change: Raw vs Log Transform"),
          "cpi_change_raw_vs_log.png", out_num, w = 10, h = 5)

# GDP
save_plot(hist_plot("gdp_country_numeric", "GDP (USD)", "Distribution of Country GDP"),
          "gdp_hist.png", out_num)
box_plot("gdp_country_numeric", "GDP (USD)", "Country GDP Boxplot",
         "gdp_box.png", out_num)

# Tertiary education
save_plot(hist_plot("gross_tertiary_education_enrollment",
          "Gross Tertiary Enrollment (%)", "Tertiary Education Enrollment"),
          "tertiary_edu_hist.png", out_num)

# Primary education
save_plot(raw_vs_transformed("gross_primary_education_enrollment_country",
          "log_gross_primary_education_enrollment_country",
          "Primary Edu (Raw)", "Primary Edu (Log)",
          "Primary Education Enrollment: Raw vs Log"),
          "primary_edu_raw_vs_log.png", out_num, w = 10, h = 5)

# Life expectancy
save_plot(raw_vs_transformed("life_expectancy_country", "exp_life_expectancy_country",
          "Life Expectancy (Raw)", "Life Expectancy (Exp)",
          "Life Expectancy: Raw vs Exp Transform"),
          "life_expectancy_raw_vs_exp.png", out_num, w = 10, h = 5)

# Tax revenue
save_plot(hist_plot("tax_revenue_country_country", "Tax Revenue (% of GDP)",
          "Distribution of Tax Revenue"),
          "tax_revenue_hist.png", out_num)
box_plot("tax_revenue_country_country", "Tax Revenue (% of GDP)", "Tax Revenue Boxplot",
         "tax_revenue_box.png", out_num)

# Total tax rate
save_plot(hist_plot("total_tax_rate_country", "Total Tax Rate (%)",
          "Distribution of Total Tax Rate"),
          "total_tax_rate_hist.png", out_num)
box_plot("total_tax_rate_country", "Total Tax Rate (%)", "Total Tax Rate Boxplot",
         "total_tax_rate_box.png", out_num)

# Population
save_plot(hist_plot("population_country", "Population",
          "Distribution of Country Population"),
          "population_hist.png", out_num)
box_plot("population_country", "Population", "Country Population Boxplot",
         "population_box.png", out_num)

# ── Categorical columns ──────────────────────────────────────────────────────

# gender
gender_df <- data %>%
  filter(!is.na(gender) & gender != "") %>%
  count(gender) %>%
  mutate(pct = n / sum(n) * 100,
         label = paste0(gender, "\n", round(pct, 1), "%"))
p_gender <- ggplot(gender_df, aes(x = "", y = n, fill = gender)) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c("M" = "#5B8FF9", "F" = "#FF6B9D")) +
  labs(title = "Gender Distribution", fill = "Gender") +
  theme_void(base_size = 13) +
  theme(legend.position = "right")
save_plot(p_gender, "gender_pie.png", out_cat, w = 6, h = 5)

# selfMade
self_df <- data %>%
  filter(!is.na(selfMade)) %>%
  mutate(selfMade = ifelse(selfMade, "Self-Made", "Inherited")) %>%
  count(selfMade)
p_self <- ggplot(self_df, aes(x = selfMade, y = n, fill = selfMade)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Self-Made" = "#5B8FF9", "Inherited" = "#FF9F40")) +
  labs(title = "Self-Made vs Inherited Wealth", x = NULL, y = "Count")
save_plot(p_self, "selfmade_bar.png", out_cat, w = 6, h = 5)

# category
save_plot(bar_plot("category", top_n = 18, title = "Billionaires by Category", xlabel = "Category"),
          "category_bar.png", out_cat, w = 8, h = 6)

# country (top 15)
save_plot(bar_plot("country", top_n = 15, title = "Top 15 Countries of Residence", xlabel = "Country"),
          "country_bar.png", out_cat, w = 8, h = 6)

# countryOfCitizenship (top 15)
save_plot(bar_plot("countryOfCitizenship", top_n = 15,
          title = "Top 15 Countries of Citizenship", xlabel = "Country"),
          "citizenship_bar.png", out_cat, w = 8, h = 6)

# residenceStateRegion (top 15)
save_plot(bar_plot("residenceStateRegion", top_n = 15,
          title = "Top 15 Residence State/Regions", xlabel = "Region"),
          "residence_region_bar.png", out_cat, w = 8, h = 6)

# source (top 15)
save_plot(bar_plot("source", top_n = 15, title = "Top 15 Wealth Sources", xlabel = "Source"),
          "source_bar.png", out_cat, w = 9, h = 6)

# organization (top 15)
save_plot(bar_plot("organization", top_n = 15,
          title = "Top 15 Organizations", xlabel = "Organization"),
          "organization_bar.png", out_cat, w = 9, h = 6)

# city (top 15)
save_plot(bar_plot("city", top_n = 15, title = "Top 15 Cities", xlabel = "City"),
          "city_bar.png", out_cat, w = 8, h = 6)

# state (top 15)
save_plot(bar_plot("state", top_n = 15, title = "Top 15 States", xlabel = "State"),
          "state_bar.png", out_cat, w = 8, h = 6)

# title (top 15)
save_plot(bar_plot("title", top_n = 15, title = "Top 15 Titles", xlabel = "Title"),
          "title_bar.png", out_cat, w = 9, h = 6)

cat("Descriptive analysis complete.\n")
cat("Numeric plots saved to:", out_num, "\n")
cat("Categorical plots saved to:", out_cat, "\n")
