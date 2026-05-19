
# Crop Water Requirements and Irrigation Demand Analysis Using FAO-56 and Remote Sensing

##  Overview
This repository contains the R scripts, workflows, and tools developed for my Master’s thesis in:

**Sustainable Water Management and Governance in Natural and Agricultural Environments (CIHEAM Zaragoza)**.

The main goal of this research is to improve the estimation of **crop water requirements (CWR)** and **irrigation water demand (IWD)** by integrating:

- FAO-56 methodology
- Remote sensing (Sentinel-2 NDVI-based crop coefficients)
- Hybrid FAO + RS approaches
- Climate and meteorological data analysis

---

##  Objectives of the Study

The thesis aims to:

1. Compare different methods for estimating Crop Water Requirements:
   - Standard FAO-56 approach (tabulated Kc + phenology)
   - Remote sensing-based Kc (NDVI-derived)
   - Hybrid approaches combining FAO and RS data

2. Evaluate irrigation water demand curves under different climatic years:
   - Wet year (2018)
   - Intermediate year (2020)
   - Drought year (2023)

3. Simulate irrigation water management scenarios using updated demand curves to assess their impact on water allocation strategies.

---

##  Study Area

The study focuses on two major irrigated areas in the Ebro Valley (Spain):

- Monegros irrigation district
- Zaidín irrigation district

These areas belong to:
- Canal de Aragón y Cataluña
- Riegos del Alto Aragón

The region is characterized by a semi-arid Mediterranean climate with strong interannual variability in precipitation and evapotranspiration.

---

##  Data Used

The analysis integrates multiple datasets:

### Crop and agricultural data
- Crop types (wheat, barley, maize, pea, sunflower)
- Phenological stages (FAO-56 and RS-derived)
- Crop coefficients (Kc-FAO and Kc-NDVI)
- Cultivated areas (PAC database)

### Meteorological data
- Reference evapotranspiration (ETo)
- Precipitation (P)
- Effective precipitation (Pe)
- SIAR weather stations (Ebro Valley network)

### Remote sensing data
- Sentinel-2 imagery
- NDVI time series
- Extraction of crop phenology (SOS, M1, M2, EOS)

---

## Methodology Overview

The workflow includes:

1. Data preprocessing and cleaning
2. Estimation of crop water requirements (CWR)
3. Calculation of irrigation water demand (IWD)
4. Scenario comparison (FAO vs RS vs Hybrid)
5. Statistical analysis (Linear Mixed Models)
6. Irrigation management simulation

---

##  CWR Estimation Scenarios

### Scenario 1: FAO-56 Standard Method
- Tabulated Kc values (FAO-56)
- Fixed phenological stages
- Equation:
  `CWR = (Kc × ETo) − Pe`

---

### Scenario 2: Remote Sensing Method
- NDVI-derived crop coefficients:
  `Kc = 1.25 × NDVI + 0.1`
- Satellite-derived phenological stages
- Dynamic crop development curves

---

### Scenario 3: Hybrid Method
- FAO Kc values combined with RS-derived phenological timing
- Adjusted crop cycles based on Sentinel-2 observations

---

##  Statistical Analysis

A Linear Mixed Model (LMM) was applied to analyze differences in water requirements:

- Fixed effects:
  - Method combination
  - Crop type
  - Sowing timing (early / medium / late)
  - Year (2018, 2020, 2023)

- Random effects:
  - Weather station nested within irrigation zone

Model estimated using:
- `lme4` package (REML)
- Post-hoc comparisons using `emmeans`

---

##  Tools and Software

- R (v4.4+)
- Excel (data preparation)
- R packages:
  - `ggplot2`
  - `dplyr`
  - `trend`
  - `Kendall`
  - `lme4`
  - `emmeans`
  - `shiny`

---

