# Evaluation of synthetic data generated from NHANES mortality data
The scripts in this repository perform a start-to-finish demonstration of the key features of synthetic data generation with the _synthpop_ package, authored by B. Nowok, G. Raab, and C. Dibben (see publication linked below). I presented this analysis at R/Pharma Conference 2025, and the slides are accessible in the file ```Furlow_RPharma2025_slides.pdf```. I used R 4.4.0.

## Dataset
The .XPT and .dat files in the ```Data``` were downloaded from the CDC's National Health And Nutrition Examination Survey (NHANES) and National Center for Health Statistics (NCHS) websites, linked below. From NHANES we get basic demographics, diagnoses, and lab measurements. From NCHS we get mortality information that will serve as the target variable in downstream analysis. To build the dataset, download all the data files in the ```Data``` folder and run ```build_data.R```, which will create the dataset ```nhanes_mort.csv``` to be read in by the ```analysis.R``` script. For ease, my ```nhanes_mort.csv``` is already in the ```Data``` folder. 

Our ```nhanes_mort.csv``` dataset includes roughly 16,000 adult subjects who participated in NHANES in 1999-2004.

## Analysis
The bulk of the demonstration is housed in ```analysis.R```. This script sources ```lookups.R``` and ```nn_synth.R```, which supply lookup tables for data processing and a custom neural network synthesizer to be used as input for _synthpop's_ ```syn``` function. After data is processed, training (n = ~11k) and holdout (n = ~5k) sets are split. The holdout set is set aside and not touched by any synthesis or modeling until evaluation at the end of the script. 

We define rules provided for synthesis and create synthetic datasets using four generative methods: Classification and Regression Trees (CART), Parametric (lets _synthpop_ choose synthesis method for each variable), Random Forest (RF), and a Neural Network (NN). The four synthesis methods are compared in terms of their Utility and Privacy with metrics commonly used in synthetic data evaluation, namely propensity Mean Squared Error (pMSE) for utility and Dorig and DiSCO for privacy. See _synthpop's_ utility and disclosure vignettes for detailed explanations of each metric. 

We train basic logistic regression models on synthetic datasets produced by each generative method. We evaluate performance of each trained model on the real holdout set that we set aside earlier.

# References
**Synthpop website**: https://www.synthpop.org.uk/

**NHANES website**: https://www.cdc.gov/nchs/nhanes/about/

**NCHS mortality data**: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm

**Original synthpop publication**: https://www.jstatsoft.org/article/view/v074i11


