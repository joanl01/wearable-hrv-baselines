# Defining Normal: Establishing Personalised Physiological Baselines from Wearable Heart Rate Variability Data

This repository contains the analysis code and research outputs associated with the Master of Philosophy thesis:

**Defining Normal: Establishing Personalised Physiological Baselines from Wearable Heart Rate Variability Data**

**Joan Shu Ting Lim**  
Master of Philosophy in Applied Mathematics and Statistics  
Adelaide University, School of Mathematical Science  
2026

## Overview
Heart Rate Variability (HRV) is increasingly used with consumer wearable devices to continuously monitor physiological states. However, HRV varies substantially between individuals, while many commonly used HRV metrics contain overlapping information. 

This project investigates whether stable, and personalised physiological baselines can be derived from wearable beat-to-beat (BBI) data using a reduced set of HRV measures. 

This study uses longitudinal data collected from Garmin wearable devices across 11 participants over approximately 6 months. The dataset contains more than 62 million observations of BBI, which provides a high-resolution longitudinal dataset to investigate physiological patterns that are within participants. 

The analysis addresses 3 main questions:
1. Can BBI derived from wearables distinguish sleep from wakefulness and identify physiologically stable periods for baseline estimation?
2. Which HRV metrics provide stable and representative measures of an individual's physiological baseline?
3. Can a substantially reduced set of HRV metrics retain the information contained in a larger HRV feature set?

## Analysis:
The repository follows the main analytical stages of the thesis:

### 1. BBI and sleep data preperation 

Raw wearable BBI and Garmin sleep-stage data are transformed to a common time resolution and combined to investigate the relationship between BBI and sleep/wakefulness stages. 

### 2. Identifying stable sleep windows

Statsitical and classification approcahes are used to investigate whether BBI can distinguish sleeping from waking periods, including:
- exploratory time series analysis
- linear regression
- ANOVA
- individual-specific logistic regression
- ROC/AUC analysis

The results indicate that BBI contains substantial information about sleep/wake states, while the optimal separation between states varies between individuals. 

### 3. HRV metric calculations and evaluation
HRV measures are calculated across multiple domains:
1. time-domain metrics (HR, SDNN, SDANN, SDNNIDX, pNN50, SDSD, rMSSD,
  IRRR, MADRR, TINN, HRVi)
2. frequency-domain metrics (, HRV,
  ULF, VLF, LF, HF, LFHF)
3. nonlinear HRV metrics (PoincarePlot.SD1, PoincarePlot.SD2,
  REC, RATIO, DET, DIV, Lmax, Lmean,
  LmeanWithoutMain, ENTR, TREND, LAM, Vmax)

The analysis evalustaes how these measures behave across physiological states and evaluates their relationships and redundancy

### 4. Dimensionality reduction 
Principal Component Analysis (PCA), correlation analysis, and hierarchical clustering were utilised to investigate whether the larger collection of HRV metrics contains substantial redundant information. 

The clustering analyses consistently identified groups of strongly related HRV measures, suggesting that many metrics provide overlapiing rather than independent information. 

### 5. Reduced HRV feature set
Three representative measures from the 3 major structures identified within the HRV space were selected:
- HR - Heart rate
- HF - High-frequency power
- ULF - Ultra-low-frequency power


### 6. Evaluation of personalised baseline physiology
The final analysis evaluates whether the reduced feature set can distinguish resting from non-resting physiological state using mixed-effects logistic regression with participant as the random effect. 

The reduced set of HRV metrics (HR, HF, ULF) provided a substantially lower BIC compared to the full HRV model, which supports an accurate representation of the underlying physiological signal. 

By calculating the Intra-class Correlation Coefficient (ICC), The analysis further identified that the last moment of deep sleep as a particularly stable baseline window for the reduced set of metrics across all participants.

## Key findings
The main findings of this thesis are:
- BBI differs systematically between sleep and wakefulness
- Sleep provides a relatively stable physiological window for investigating individual baseline physiology
- Physiological baselines differ substantially between individuals, supporting personalised rather than reference values at a population level
- Many commonly used HRV metrics are strongly correlated and contain overlapping information
- A reduced set of metrics (HR, HF, ULF) provide a compact representation of the dominant HRV signal across the HRV metrics. 
- The last moment of deep sleep is a promisign candidate window for establishing an individual's physiological baseline. 


## Reproducibiltiy
The analysis was conducted in **R 4.5.2**. HRV calculations included measures derived using the `RHRV` package. Where possible, this repository provides:
- data processing scripts
- statistical analysis scripts
- HRV calculation procedures
- model-fitting code
- figure generation code

The aim is to make the analytical workflow transparent and reproducible from raw or appropriately prepared data through to the reported results.


## Data availability and participant privacy

The underlying wearable data were provided for the research project *“What causes low back pain to flare: Has a major opportunity to understand back pain been missed?”* and contain longitudinal measurements from human participants.

**Participant-level raw data should not be redistributed through this repository unless the appropriate permissions and data-sharing approvals have been obtained.**

The repository contains the analysis code and documentation required to understand the workflow, while restricted data and/or participant-level outputs are excluded or provided only through an approved access mechanism.
