# Data cleaning — Titanic passenger file

This folder contains the pipeline that transforms the raw CSV into the cleaned CSV used by analysis.
The cleaned dataset has been updated to match the latest rules below.

- Raw input: `data/Titanic-Dataset.csv`
- Cleaned output: `data/titanic_cleaned.csv` (created by `clean_and_save_titanic.R`)

## Current cleaning policy

The script now keeps missing values as missing (`NA`) and does not impute `Age`, `Embarked`, or `Fare`.

### What is done

- Preserve raw missingness for `Age`, `Embarked`, and `Fare`.
- Create `Title` from `Name`.
- Create `FamilySize = SibSp + Parch + 1`.
- Create `IsSolo` (`1` when `FamilySize == 1`, else `0`).
- Keep useful analysis types in memory before export (`Survived`, `Sex`, `Pclass`, `Embarked`).
- Sort rows by `PassengerId` before writing output.

### What is not done

- No median/mean/mode imputation.
- No `Fare_per_person`.
- No `SES_band`.
- No `Deck`.
- No `Cabin`.
- No `Ticket_party_size`.

## Output schema

`data/titanic_cleaned.csv` now contains 14 columns in this order:

1. `Ticket`
2. `PassengerId`
3. `Survived`
4. `Pclass`
5. `Name`
6. `Sex`
7. `Age`
8. `SibSp`
9. `Parch`
10. `Fare`
11. `Embarked`
12. `Title`
13. `FamilySize`
14. `IsSolo`

## How to run

```r
source("data_cleaning/clean_and_save_titanic.R")
```

After running, confirm `data/titanic_cleaned.csv` has 891 rows and the 14 columns listed above.
