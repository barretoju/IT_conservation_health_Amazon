---
title: "Indigenous Tierritories (IT) Conservation & Health in the Amazon (IT_conservation_health_amazon)"
author: "Julia R Barreto"
date: "`r Sys.Date()`"
output:
  rmdformats::readthedown:
    highlight: kate
    thumbnails: false
    lightbox: true
    gallery: false
editor_options: 
  markdown: 
    wrap: 72
---

# Repository Overview

This repository contains all data and code necessary to reproduce the
analyses presented in the paper, "Indigenous Territories can safeguard
human health depending on the landscape structure and legal status".
The study explores the relationship between indigenous territories (ITs) and 
health outcomes across the Pan-Amazonian region, focusing on fire-related and
zoonotic diseases. The repository includes comprehensive datasets, R
scripts, and model outputs, covering the full research workflow from statistical
analysis to model prediction plots.

## **Folder Structure**

The repository is organized as follows:

| **Folder / File**                                                     | **Description**                                                                                        |
| --------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| `data/`                                                               | Contains the input datasets used for the analyses.                                                     |
| `data/incidence_data.csv`                                             | Main dataset combining health outcomes, land-use variables, fire-related pollution, and demographics.  |
| `outputs/`                                                            | Contains all model outputs and figures.                                                                |
| `outputs/figures/`                                                    | Folder where prediction plots and other figures are saved.                                             |
| `1_models_fire-related.Rmd` to `1.9_models_rickettsia_occurrence.Rmd` | RMarkdown scripts for modeling the relationship between landscape variables and each specific disease. |
| `2_legal_framework.Rmd`                                               | Script describing the legal framework of Indigenous land recognition across Amazonian countries.       |
| `firepollutants_models.R`                                             | Additional modeling script focusing on fire pollutants (PM2.5).                                        |
| `IT_conservation_health_amazon.Rproj`                                 | RStudio project file for workflow management.                                                          |
| `README.md` / `README.html`                                           | Project documentation.                                                                                 |



## Data

A full master data aggregates disease variables used in the study.

```{r}
incidence_data <- read.csv(here("data", "incidence_data.csv"), header = T)
knitr::kable(incidence_data %>% 
  sample_n(30) %>% 
  print(row.names = FALSE), booktabs = TRUE) %>%
kable_styling(latex_options = "scale_down") %>%
  column_spec(1:ncol(incidence_data), width = "auto")
```

| Column Name                            | Description                                                                                            |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| **country**                            | Country code
|
| **COD**                                | Unique code identifying each municipality (e.g., BO - Bolivia, BR - Brazil, CO - Colombia, etc.)                                                              |
| **X**                                  | Longitude coordinate of the municipality centroid                                                      |
| **Y**                                  | Latitude coordinate of the municipality centroid                                                       |
| **year**                               | Year of data collection                                                                                |
| **pop**                                | Population size of the municipality                                                                    |
| **IDH**                                | Human Development Index (HDI)                                                      |
| **health.cases**                       | Number of general health-related cases (aggregated diseases)                                           |
| **health.incidence**                   | Incidence of general health-related cases per population                                               |
| **fire.cases**                         | Number of reported fire-related disease cases                                                          |
| **fire.incidence**                     | Fire-related disease incidence per population                                                          |
| **cardiovascular.cases**               | Number of reported cardiovascular disease cases                                                        |
| **cardiovascular.incidence**           | Cardiovascular disease incidence per population                                                        |
| **respiratory.cases**                  | Number of reported respiratory disease cases                                                           |
| **respiratory.incidence**              | Respiratory disease incidence per population                                                           |
| **zoonotic.cases**                     | Number of reported zoonotic disease cases (aggregate of zoonotic diseases)                             |
| **zoonotic.incidence**                 | Zoonotic disease incidence per population                                                              |
| **malaria.cases**                      | Number of reported malaria cases                                                                       |
| **malaria.incidence**                  | Malaria incidence per population                                                                       |
| **cutaneous\_leishmaniasis.cases**     | Number of reported cutaneous leishmaniasis cases                                                       |
| **cutaneous\_leishmaniasis.incidence** | Cutaneous leishmaniasis incidence per population                                                       |
| **visceral\_leishmaniasis.occurrence** | Presence (1) or absence (0) of visceral leishmaniasis cases                                            |
| **chagas.occurrence**                  | Presence (1) or absence (0) of Chagas disease cases                                                    |
| **hantavirus.cases**                   | Number of reported hantavirus cases                                                                    |
| **rickettsia.occurrence**              | Presence (1) or absence (0) of spotted fever group rickettsiosis (Rickettsia) cases                    |
| **FS\_PLAND**                          | Percentage of forest and savanna land cover in the municipality (regardless of Indigenous Territories) |
| **FS\_noIT**                           | Percentage of forest and savanna land cover outside Indigenous Territories                             |
| **for\_PD**                            | Patch density (number of forest patches per area)                                                      |
| **for\_ED**                            | Forest edge density (length of edges per area)                                                         |
| **for\_AI**                            | Forest aggregation index (degree of clustering of forest patches)                                      |
| **tot\_IT**                            | Percentage of municipality area covered by Indigenous Territories                                      |
| **NOTackn\_IT**                        | Percentage of Indigenous Territories not officially recognized by the government                       |
| **acknlgd\_IT**                        | Percentage of Indigenous Territories officially acknowledged by the government                         |
| **pm25\_SUM**                          | Annual sum of fire-related particulate matter (PM2.5) per municipality                                 |


## Disease Models

### **Modeling Routine**

Each disease script (`1_models_fire-related.Rmd` to `1.9_models_rickettsia_occurrence.Rmd`) follows the same analytical routine:

1. **Data Loading and Preprocessing**
   Load `incidence_data.csv`, filter the relevant variables for each disease, and remove missing data.

2. **Exploratory Data Analysis**

   * Visualize disease incidence distributions.
   * Apply log-transformations where needed (e.g., `log10(x+1)`).
   * Summarize data with `skimr::skim()`.

3. **Model Fitting**
   Use **Generalized Additive Models (GAMs)** with spatial smoothing and year random effects.
   The models test combinations of Indigenous Territories, forest cover, and landscape fragmentation as predictors.

4. **Types of Models Tested**

   | **Model Type**              | **Purpose**                                                              |
   | --------------------------- | ------------------------------------------------------------------------ |
   | **Null Model (Baseline)**   | Intercept + random effects for year and space (no landscape predictors). |
   | **Single-Predictor Models** | Test one variable at a time (e.g., ITs, forest cover, fragmentation).    |
   | **Double Additive Models**  | Combine two predictors (e.g., IT + forest, forest + fragmentation).      |
   | **Triple Additive Models**  | Combine three predictors (IT + forest + fragmentation).                  |
   | **Interaction Models**      | Include interaction terms (e.g., IT × forest, forest × fragmentation).   |

5. **Model Selection**
   Use **AIC comparison** (`AICtab()`) to select the best-fitting model.

6. **Diagnostics**
   Evaluate model fit with residual simulations using **DHARMa**.

7. **Predictions & Visualization**
   Generate predictions across varying levels of Indigenous Territories, forest cover, and fragmentation.
   Visualize disease incidence trends and save figures to `outputs/figures/`.

---

### **Specific Models**

| **Script**                                         | **Focus Disease / Outcome**                                   |
| -------------------------------------------------- | ------------------------------------------------------------- |
| `1_models_fire-related.Rmd`                        | Fire-related diseases (respiratory + cardiovascular combined) |
| `1.2_models_cardiovascular.Rmd`                    | Cardiovascular diseases                                       |
| `1.3_models_respiratory.Rmd`                       | Respiratory diseases                                          |
| `1.4_models_zoonotic.Rmd`                          | Zoonotic diseases (aggregate)                                 |
| `1.5_models_malaria.Rmd`                           | Malaria                                                       |
| `1.6_models_cutaneous leishmaniasis.Rmd`           | Cutaneous leishmaniasis                                       |
| `1.7_models_visceral_leishmaniasis_occurrence.Rmd` | Visceral leishmaniasis (presence/absence)                     |
| `1.8_models_chagas_occurrence.Rmd`                 | Chagas disease (presence/absence)                             |
| `1.9_models_rickettsia_occurrence.Rmd`             | Rickettsiosis (spotted fever group, presence/absence)         |

---

## Approach Summary

The project models how the **extent of Indigenous Territories**, **forest cover (inside and outside ITs)**, and **landscape fragmentation metrics** influence disease incidence across Amazonian municipalities.
Fire-related diseases also include **PM2.5 exposure** as an additional factor.

Each model accounts for:

* **Spatial autocorrelation** (random smoother on coordinates)
* **Temporal autocorrelation** (random smoother on year)
* **Population health baseline** via Human Development Index (HDI) as an offset
