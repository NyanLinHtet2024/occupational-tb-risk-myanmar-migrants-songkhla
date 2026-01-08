## -------------------------------------------------
## Occupational TB Risk Analysis
## Data cleaning and variable recoding
## Note: Raw data are not included in this repository
## -------------------------------------------------

rm(list = ls())

library(epicalc)
library(dplyr)

# Load data (not included due to ethical restrictions)
dat <- read.csv("data/data_entered_20251024_CLEAN.csv")

## ----------------------------------
## Check ID and basic variables
## ----------------------------------
tab1(dat$id)
tab1(dat$a1)

## ----------------------------------
## Gender minority status
## ----------------------------------
dat$gender_minority <- ifelse(
  dat$a1 == 1 & dat$a2 == 1, "1 - Cisgender male",
  ifelse(dat$a1 == 2 & dat$a2 == 2, "2 - Cisgender female",
         "3 - Minority")
)

dat$gender_minority <- ifelse(
  dat$a1 == 99 | dat$a2 == 99,
  "9 - Incomplete info",
  dat$gender_minority
)

tab1(dat$gender_minority)

## ----------------------------------
## Age (recode 999 as NA)
## ----------------------------------
dat$age <- ifelse(dat$a3 == 999, NA, dat$a3)
summ(dat$age)

## ----------------------------------
## Socio-demographic variables
## ----------------------------------
tab1(dat$a5)   # Marital status
tab1(dat$a8)   # Education
tab1(dat$a6)   # Religion
tab1(dat$a12)  # Monthly income

## ----------------------------------
## Occupational risk factors
## ----------------------------------
tab1(dat$c4)  # Ventilation
tab1(dat$c5)  # Fan / AC use
tab1(dat$c6)  # Air movement
tab1(dat$c7)  # Visible dust
tab1(dat$c8)  # Crowding

## ----------------------------------
## TB exposure among co-workers
## ----------------------------------
dat$tb_diag <- ifelse(dat$c9a == 88, 0, dat$c9a)
tab1(dat$tb_diag)

dat$tb_sameroom <- dat$c9b
dat$tb_sameroom[is.na(dat$tb_sameroom)] <- 0
dat$tb_sameroom <- ifelse(dat$tb_sameroom == 88, 0, dat$tb_sameroom)
dat$tb_sameroom <- ifelse(dat$tb_diag == 99, 99, dat$tb_sameroom)

tab1(dat$tb_sameroom)
table(dat$tb_diag, dat$tb_sameroom)

## ----------------------------------
## Combined TB exposure status
## ----------------------------------
dat$tb_status <- ifelse(
  dat$tb_diag == 0, "1 - No",
  "NOT YET CODED"
)

dat$tb_status <- ifelse(
  dat$tb_diag == 1 & dat$tb_sameroom == 0,
  "2 - Diagnosed, not same room",
  dat$tb_status
)

dat$tb_status <- ifelse(
  dat$tb_diag == 1 & dat$tb_sameroom == 1,
  "3 - Diagnosed, same room",
  dat$tb_status
)

dat$tb_status <- ifelse(
  dat$tb_diag == 99 | dat$tb_sameroom == 99,
  "9 - Incomplete info",
  dat$tb_status
)

tab1(dat$tb_status)
