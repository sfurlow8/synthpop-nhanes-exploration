## ---------------------------------------------
## 
## Script: build_data.R
## 
## Author: Sophie Furlow
## Date created: October 22, 2025
##
## Description: Build dataset nhanes_mort.csv from XPT files from NHANES
##              and NCHS mortality websites. 
## Contents: Setup, Collect NHANES Data, Join NHANES
##           Data, Collect Mort Data, Join NHANES & Mort
## Sources: https://www.cdc.gov/nchs/nhanes/about/
##          https://www.cdc.gov/nchs/data-linkage/mortality-public.htm
##
## ---------------------------------------------

# --------------------------- SETUP ---------------------------

library(haven)
library(dplyr)
library(readr)

# --------------------------- COLLECT NHANES DATA ---------------------------

############# Laboratory data ############# 

troponin <- read_xpt(paste0("SSTROP_A.XPT")) %>% # troponin
  select(SEQN, SSTNIA)

bnp <- read_xpt(paste0("SSBNP_A.XPT")) %>% # NtPro
  select(SEQN, SSBNP)

#LBXSCR - Creatinine (mg/dL) and LBDSCR - Creatinine (mg/dL)
egfr_A <-  read_xpt(paste0("LAB18.XPT"))  %>%
  select(SEQN, LBXSCR)
egfr_B <- read_xpt(paste0("L40_B.XPT")) %>%
  select(SEQN, LBDSCR) %>%
  rename(LBXSCR = LBDSCR)
egfr_C <- read_xpt(paste0("L40_C.XPT")) %>%
  select(SEQN, LBXSCR)
egfr <- rbind(egfr_A, egfr_B, egfr_C)

############# Demographic data ############# 
#RIAGENDR - Gender
#RIDAGEYR - Age at Screening - Individuals 85 and over are topcoded at 85 years of age
#RIDAGEEX - Exam Age in Months - Best age in months at date of examination for individuals under 85 years of age at screening.
#RIDRETH2 - Race/Ethnicity 
#RIDEXPRG - Pregnancy Status at Exam

demo_A <- read_xpt(paste0("DEMO.XPT")) %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDAGEEX, RIDRETH2, RIDEXPRG,
            DMDEDUC2, DMDMARTL, INDHHINC) %>%
  mutate(cohort = "1999-2000") 
demo_B <- read_xpt(paste0("DEMO_B.XPT"))%>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDAGEEX, RIDRETH2, RIDEXPRG,
            DMDEDUC2, DMDMARTL, INDHHINC) %>%
  mutate(cohort = "2001-2002") 
demo_C <- read_xpt(paste0("DEMO_C.XPT"))%>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDAGEEX, RIDRETH2, RIDEXPRG,
            DMDEDUC2, DMDMARTL, INDHHINC) %>%
  mutate(cohort = "2003-2004") 
demo <- rbind(demo_A, demo_B, demo_C)

############# Questionnaire data ############# 

##### Medical Conditions
#A selfreported history of cardiovascular disease was defined as any report of coronary heart disease (MCQ160C), angina (MCQ160D), heart attack (MCQ160E), stroke (MCQ160F), or heart failure (MCQ160B).
hearth_A <-  read_xpt(paste0("MCQ.XPT")) %>%
  select(SEQN, MCQ160C,MCQ160D, MCQ160E, MCQ160F, MCQ160B)
hearth_B <- read_xpt(paste0("MCQ_B.XPT")) %>%
  select(SEQN, MCQ160C,MCQ160D, MCQ160E, MCQ160F, MCQ160B)
hearth_C <- read_xpt(paste0("MCQ_C.XPT")) %>%
  select(SEQN, MCQ160C,MCQ160D, MCQ160E, MCQ160F, MCQ160B)
hearth <- rbind(hearth_A, hearth_B, hearth_C)

##### Pregnancy Test - Urine   
#URXPREG - Pregnancy test result
preg_A <-  read_xpt(paste0("UC.XPT"))
preg_B <- read_xpt(paste0("UC_B.XPT"))
preg_C <- read_xpt(paste0("UC_C.XPT"))
preg <- rbind(preg_A, preg_B, preg_C)

##### Diabetes
#DIQ010 - Doctor told you have diabetes
dia_A <-  read_xpt(paste0("DIQ.XPT"))  %>%
  select(SEQN, DIQ010)
dia_B <- read_xpt(paste0("DIQ_B.XPT")) %>%
  select(SEQN, DIQ010)
dia_C <- read_xpt(paste0("DIQ_C.XPT")) %>%
  select(SEQN, DIQ010)
dia <- rbind(dia_A, dia_B, dia_C)

##### Smoking
#SMQ040 - Do you now smoke cigarettes
smoke_A <-  read_xpt(paste0("SMQ.XPT"))  %>%
  select(SEQN, SMQ040)
smoke_B <- read_xpt(paste0("SMQ_B.XPT")) %>%
  select(SEQN, SMQ040)
smoke_C <- read_xpt(paste0("SMQ_C.XPT")) %>%
  select(SEQN, SMQ040)
smoke <- rbind(smoke_A, smoke_B, smoke_C)


############# Examination data ############# 

##### Body measures
#BMXBMI - Body Mass Index (kg/m**2)
body_A <-  read_xpt(paste0("BMX.XPT"))  %>%
  select(SEQN, BMXBMI)
body_B <- read_xpt(paste0("BMX_B.XPT")) %>%
  select(SEQN, BMXBMI)
body_C <- read_xpt(paste0("BMX_C.XPT")) %>%
  select(SEQN, BMXBMI)
body <- rbind(body_A, body_B, body_C)

##### Blood pressure
#BPXSY1 - Systolic: Blood pres (1st rdg) mm Hg
#BPXDI1 - Diastolic: Blood pres (1st rdg) mm Hg
hyperten_A <-  read_xpt(paste0("BPX.XPT"))  %>%
  select(SEQN, BPXSY1, BPXDI1)
hyperten_B <- read_xpt(paste0("BPX_B.XPT")) %>%
  select(SEQN, BPXSY1, BPXDI1)
hyperten_C <- read_xpt(paste0("BPX_C.XPT")) %>%
  select(SEQN, BPXSY1, BPXDI1)
hyperten <- rbind(hyperten_A, hyperten_B, hyperten_C)


# --------------------------- JOIN NHANES DATA ---------------------------

dfs_list <- list(troponin, bnp, egfr, hearth, preg, dia, smoke, body, hyperten)
nhanes <- demo # use BNP data as base, join everything else to it
for (df in dfs_list) {
    nhanes <- left_join(nhanes, df, by=c('SEQN'))
}

nhanes <- nhanes %>%
  filter(RIDAGEYR >= 18) %>%
  filter(URXPREG != 1 | is.na(URXPREG))

write.csv(nhanes, 'nhanes.csv')


# --------------------------- COLLECT MORT DATA ---------------------------

# read in the fixed-width format ASCII files

list_df_names <- c('1999_2000', '2001_2002', '2003_2004')
full_dsn <- data.frame()
for (df_name in list_df_names) {
  srvyin <- paste0("NHANES_", df_name, "_MORT_2019_PUBLIC.dat")
  curr_df <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
    )
  full_dsn <- rbind(full_dsn, curr_df)
}


# --------------------------- JOIN NHANES & MORT ---------------------------

full_dsn <- full_dsn %>% 
  mutate(SEQN = seqn) %>%
  select(-seqn, eligstat, mortstat, ucod_leading)

joined <- left_join(nhanes, full_dsn, by='SEQN')

write.csv(joined, 'nhanes_mort.csv')
