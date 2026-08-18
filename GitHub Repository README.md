# Defining Normal: Establishing Personalised Physiological Baselines from Wearable Heart Rate Variability Data

This repository contains the analysis code and research outputs associated with the Master of Philosophy thesis:

**Defining Normal: Establishing Personalised Physiological Baselines from Wearable Heart Rate Variability Data**

**Joan Shu Ting Lim**  
Master of Philosophy in Applied Mathematics and Statistics  
Adelaide University, School of Mathematical Science  
2026

## Overview

Heart rate variability (HRV) is increasingly used with consumer wearable devices to monitor physiological state. However, HRV varies substantially between individuals, while many commonly used HRV metrics contain overlapping information.

This project investigates whether stable, personalised physiological baselines can be derived from wearable beat-to-beat interval (BBI) data using a reduced set of HRV measures.

The study uses longitudinal data collected from Garmin wearable devices across 11 participants over approximately six months. The dataset contains more than 62 million BBI observations, providing a high-resolution longitudinal dataset for investigating within-participant physiological patterns.

The analysis addresses three main questions:

1. Can wearable-derived BBI distinguish sleep from wakefulness and identify physiologically stable periods suitable for baseline estimation?
2. Which HRV metrics provide stable and representative measures of an individual's physiological baseline?
3. Can a substantially reduced set of HRV metrics retain the information contained in a larger HRV feature set?

## Analysis

The repository follows the main analytical stages of the thesis:

### 1. BBI and sleep data preparation

Raw wearable BBI and Garmin sleep-stage data are transformed to a common time resolution and combined to investigate the relationship between beat-to-beat intervals and sleep/wake states.

### 2. Identifying stable sleep windows

Statistical and classification approaches are used to investigate whether BBI can distinguish sleeping from waking periods, including:

- exploratory time-series analysis
- linear regression
- ANOVA
- individual-specific logistic regression
- confusion matrices
- ROC/AUC analysis

The results indicate that BBI contains substantial information about sleep/wake state, while the optimal separation between states varies between individuals.

### 3. HRV metric calculation and evaluation

HRV measures are calculated across multiple domains:

- time-domain metrics
- frequency-domain metrics
- nonlinear HRV metrics

The analysis examines how these measures behave across physiological states and evaluates their relationships and redundancy.

### 4. HRV dimensionality reduction

Principal Component Analysis (PCA), correlation analysis, and hierarchical clustering are used to investigate whether the large collection of HRV metrics contains substantial redundant information.

The clustering analyses consistently identify groups of strongly related HRV measures, suggesting that many metrics provide overlapping rather than independent information.

### 5. Reduced HRV feature set

Three representative measures were selected:

- **HR — Heart Rate**
- **HF — High-Frequency Power**
- **ULF — Ultra-Low-Frequency Power**

These measures were selected as representatives of the major structures identified within the HRV feature space.

### 6. Evaluation of personalised baseline physiology

The final analysis evaluates whether the reduced feature set can distinguish resting from non-resting physiological states using mixed-effects logistic regression with participant-level random effects.

The reduced model using HR, HF, and ULF achieved an **AUC of 0.912**, compared with **0.977** for the full HRV model. The reduced model also provided substantially lower BIC, supporting a more parsimonious representation of the underlying physiological signal.

The analysis further identified the **last moment of deep sleep** as a particularly stable baseline window for HR and HF across participants.

## Key findings

The main findings of the thesis are:

- BBI differs systematically between sleep and wakefulness.
- Sleep provides a relatively stable physiological window for investigating individual baseline physiology.
- Physiological baselines differ substantially between individuals, supporting personalised rather than population-level reference values.
- Many commonly used HRV metrics are strongly correlated and contain overlapping information.
- HR, HF, and ULF provide a compact representation of the dominant structure identified across the HRV metrics.
- The reduced HRV model retains strong discriminative performance while substantially reducing model complexity.
- The final moment of deep sleep provides a promising candidate window for establishing an individual's physiological baseline.

The thesis reports AUC values above 0.84 for individual BBI-based sleep/wake models across participants, while the final reduced mixed-effects model achieved an AUC of 0.912.

## Repository structure

A suggested repository structure is:

```text
.
├── README.md
├── LICENSE
├── renv.lock
│
├── data/
│   ├── README.md
│   └── ...
│
├── R/
│   ├── data_preparation/
│   ├── bbi_analysis/
│   ├── hrv_analysis/
│   ├── baseline_analysis/
│   └── functions/
│
├── scripts/
│   ├── 01_prepare_data.R
│   ├── 02_bbi_sleep_analysis.R
│   ├── 03_hrv_metrics.R
│   ├── 04_sleep_wake_classification.R
│   ├── 05_hrv_reduction.R
│   └── 06_baseline_evaluation.R
│
├── results/
│   ├── tables/
│   ├── figures/
│   └── participant_results/
│
└── thesis/
    └── thesis.pdf
```

The exact structure can be adjusted to match the organisation of the code actually used in the thesis.

## Reproducibility

The analysis was conducted in **R 4.5.2**. HRV calculations included measures derived using the `RHRV` package. 
Where possible, the repository provides:

- data-processing scripts
- statistical analysis scripts
- HRV calculation procedures
- model-fitting code
- figure-generation code
- analysis outputs
- package/environment information

The aim is to make the analytical workflow transparent and reproducible from raw or appropriately prepared data through to the reported results.

## Data availability and participant privacy

The underlying wearable data were provided for the research project *“What causes low back pain to flare: Has a major opportunity to understand back pain been missed?”* and contain longitudinal measurements from human participants.

**Participant-level raw data should not be redistributed through this repository unless the appropriate permissions and data-sharing approvals have been obtained.**

If the underlying participant data cannot be publicly shared, the repository contains the analysis code and documentation required to understand the workflow, while restricted data and/or participant-level outputs are excluded or provided only through an approved access mechanism.

## Thesis

The full thesis is:

**Lim, J. S. T. (2026). _Defining Normal: Establishing Personalised Physiological Baselines from Wearable Heart Rate Variability Data_. Master of Philosophy thesis, Adelaide University.**

The thesis describes the methodology, statistical analyses, results, and limitations in detail.

## Citation

If you use this repository or build upon this work, please cite the thesis:

```bibtex
@mastersthesis{lim2026defining,
  author  = {Lim, Joan Shu Ting},
  title   = {Defining Normal: Establishing Personalised Physiological Baselines from Wearable Heart Rate Variability Data},
  school  = {Adelaide University},
  year    = {2026},
  type    = {Master of Philosophy thesis},
  address = {Adelaide, Australia}
}
```

## Limitations and future work

The findings should be interpreted in the context of the available wearable data and study population. In particular, the thesis identifies the need for broader participant samples and further validation of wearable-derived HRV measures against ECG and other wearable devices.

Future work proposed in the thesis includes comparisons between Garmin-derived HRV and ECG measurements, as well as comparisons across devices such as Oura, WHOOP, and Apple Watch.

## Author

**Joan Shu Ting Lim**  
Master of Philosophy in Applied Mathematics and Statistics  
Adelaide University  
2026