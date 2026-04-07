Data files in this folder:

- Titanic-Dataset.csv (raw input)
- titanic_cleaned.csv (cleaned output)

How to regenerate the cleaned dataset:

1) Set R working directory to the MH3511 project root.
2) Run:
   source("data_cleaning/clean_and_save_titanic.R")

Current cleaned dataset policy:
- Keeps missing values as NA (no imputation for Age, Embarked, Fare)
- Drops Cabin and Ticket_party_size from output
- Does not create Fare_per_person, SES_band, or Deck

Current titanic_cleaned.csv columns (14):
Ticket, PassengerId, Survived, Pclass, Name, Sex, Age, SibSp, Parch, Fare, Embarked, Title, FamilySize, IsSolo
