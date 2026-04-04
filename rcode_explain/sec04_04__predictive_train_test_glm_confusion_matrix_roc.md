# Explainer: `R/sec04_04__predictive_train_test_glm_confusion_matrix_roc.R`

## Report section

Supports **§4.4** — simple **prediction**: **train/test split**, **confusion matrix**, **accuracy** at **0.5** threshold.

## In plain English

This is **not** the same goal as **§4.3**. Here the question is: if we **fit** a logistic model on **80%** of passengers and **predict** the held-out **20%**, how often does a **0.5** probability cutoff get survival **right**? **`set.seed(3511)`** makes the random split **reproducible**. The model is **slightly smaller** than §4.3 (**no Embarked**) to keep the teaching script compact—your group can align formulas if the marker wants consistency.

## Before you run

Cleaned CSV; **ggplot2** for heatmap-style confusion matrix.

## What the script does (walkthrough)

**`sample.int`** for training indices, **glm** on **train**, **`predict` type = "response"** on **test**, **`pred = 1` if prob ≥ 0.5**. **`table(Actual, Predicted)`**, **mean(pred == actual)**. ggplot **tile** plot with counts in cells.

## What you will see in the console

**Confusion matrix** and one line **accuracy**. Numbers will match anyone using the same seed and data.

## Figures

- **`output/figures/fig__sec04_04__confusion_matrix.png`**

## For your report / interpretation

Emphasise **limitations** from **`REPORT_README`**: single split, no cross-validation, arbitrary **0.5** threshold, no **ROC/AUC** in-script. **Accuracy** can mislead with **imbalanced** outcomes—optional mention of **sensitivity/specificity** if you compute them from the same table. Distinguish **explanatory** (§4.3) vs **predictive** (§4.4) goals.
