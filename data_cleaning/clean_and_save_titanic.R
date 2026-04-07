# =============================================================================
# data_cleaning/clean_and_save_titanic.R
# Reads:  data/Titanic-Dataset.csv  (place raw Kaggle-style CSV here)
# Writes: data/titanic_cleaned.csv
# Run from project root (MH3511 folder):  setwd(".../MH3511"); source("data_cleaning/clean_and_save_titanic.R")
# =============================================================================

ensure_project_root <- function() {
  if (dir.exists("data")) return(invisible(NULL))
  if (dir.exists("../data")) {
    setwd("..")
    return(invisible(NULL))
  }
  stop("Run this script with working directory = MH3511 (folder containing data/).")
}

ensure_project_root()

raw_path <- file.path("data", "Titanic-Dataset.csv")
if (!file.exists(raw_path)) {
  stop(
    "Missing ", raw_path, ". Copy Titanic-Dataset.csv into data/ then re-run.",
    call. = FALSE
  )
}

df <- utils::read.csv(raw_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# --- Keep missing values as NA (no imputation) ---
df$Embarked[df$Embarked == ""] <- NA
df$Embarked <- factor(df$Embarked, levels = c("C", "Q", "S"))

# --- Title from Name (text before first dot after comma) ---
df$Title <- sub("^[^,]+, ([^.]+).*", "\\1", df$Name)
df$Title <- trimws(df$Title)

# --- Family size & solo traveller ---
df$FamilySize <- df$SibSp + df$Parch + 1L
df$IsSolo <- as.integer(df$FamilySize == 1L)

# --- Ticket party size & fare per person (same Ticket = same purchase group) ---
tix_n <- as.data.frame.table(table(df$Ticket), stringsAsFactors = FALSE)
names(tix_n) <- c("Ticket", "Ticket_party_size")
tix_n$Ticket_party_size <- as.integer(tix_n$Ticket_party_size)
df <- merge(df, tix_n, by = "Ticket", all.x = TRUE)
df$Ticket_party_size <- NULL
df$Cabin <- NULL

# --- Useful types for analysis ---
df$Survived <- as.integer(df$Survived)
df$Sex <- factor(df$Sex, levels = c("male", "female"))
df$Pclass <- factor(df$Pclass, levels = c(1, 2, 3), ordered = TRUE)

df <- df[order(df$PassengerId), , drop = FALSE]
rownames(df) <- NULL

out_path <- file.path("data", "titanic_cleaned.csv")
utils::write.csv(df, out_path, row.names = FALSE)

message("Wrote ", nrow(df), " rows to ", normalizePath(out_path, winslash = "/"))
