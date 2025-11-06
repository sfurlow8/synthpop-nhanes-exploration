## ---------------------------------------------
## 
## Script: lookups.R
## 
## Author: Sophie Furlow
## Date created: October 22, 2025
##
## Description: Contains lookup lists that code numeric values of 
##              some NHANES variables to their character names. 
## Sources: https://www.cdc.gov/nchs/nhanes/about/
##          https://www.cdc.gov/nchs/data-linkage/mortality-public.htm
##
## ---------------------------------------------

# lookup lists to recode meaning
# see NHANES website and mortality codebook for sources

eth_lookup <- c(
  `1` = 'Non-Hispanic White',
  `2` = 'Non-Hispanic Black',
  `3` = 'Mexican American',
  `4` = 'Other Race',
  `5` = 'Other Hispanic'
)

educ_lookup <- c(
  `1` = 'Less Than 9th Grade',
  `2` = '9-11th Grade',
  `3` = 'High School Grad/GED or Equivalent',
  `4` = 'Some College or AA Degree',
  `5` = 'College Graduate or Above',
  `7` = 'Refused',
  `8` = 'Flu & Pneumonia',
  `9` = "Don't Know"
)

mar_lookup <- c(
  `1` = 'Married',
  `2` = 'Widowed',
  `3` = 'Divorced',
  `4` = 'Separated',
  `5` = 'Never Married',
  `6` = 'Living with partner', 
  `77` = 'Refused',
  `99` = "Dont't know"
)

inc_lookup <- c(
  `1` = '$0-4999',
  `2` = '$5000-9999',
  `3` = '$10000-14999',
  `4` = '$15000-19999',
  `5` = '$20000-24999',
  `6` = '$25000-34999', 
  `7` = '$35000-44999',
  `8` = '$45000-54999',
  `9` = '$55000-64999',
  `10` = '$65000-74999',
  `11` = '$75000 and Over',
  `12` = 'Over $20000',
  `13` = 'Under $20000',
  `77` = 'Refused',
  `99` = "Don't know"
)

cod_lookup <- c(
  `1` = 'Cardiovascular',
  `2` = 'Malignant Neoplasms',
  `3` = 'Chronic Respiratory',
  `4` = 'Accident',
  `5` = 'Cerebrovascular',
  `6` = 'Alzheimers', 
  `7` = 'Diabetes',
  `8` = 'Flu & Pneumonia',
  `9` = 'Nephritis & Nephrosis',
  `10` = 'All Other Causes'
)