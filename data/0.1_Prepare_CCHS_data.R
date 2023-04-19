#' ---
#' title: "Coding Club workshop CCHS data preparation"
#' author: "Didier Brassard"
#' date: "`r Sys.Date()`"
#' output:
#'  html_document:
#'    code_folding: "show"
#' ---

# NOTE: the purpose of this code is to track how the demonstration sample was obtained.
# It is not part of the workshop.

# *********************************************************************** #
#                                                                         #
#           Load raw data and prepare sample for demonstration            #
#                                                                         #
# Date: 2023-04-19                                                        #
# Author: Didier Brassard                                                 #
#                                                                         #
# *********************************************************************** #

# external drive with CCHS 2015 - Nutrition files
  cchs_dir <- file.path("","Volumes","SANDISK DID","CCHS_Nutrition_2015_PUMF","Data_Donnee")

# load library
  ## data
  library(haven)
  library(dplyr)
  library(tidylog)
  library(readxl)
  library(labelled)

  ## project
  library(here)

# *********************************************************************** #
#                    Load HS and dietary intake data                      #
# *********************************************************************** #

  # HS: "General health and summary data for 24-hour dietary recall and nutritional supplements"
  
hs <-
  haven::read_sas(data_file = file.path(cchs_dir,"hs.sas7bdat")) |>
  # keep only respondents aged 19 to 70 years
  filter(DHHDDRI>=8 & DHHDDRI<=13) |>
  mutate(
  # recoding of some variables
    ## 24hr
      r24_weekend = ifelse(ADMFW==1,1,0),
      r24_season  = case_when(
        ADM_MOI %in% c(12,1,2) ~ "Winter",
        ADM_MOI %in% c(3,4,5) ~ "Spring",
        ADM_MOI %in% c(6,7,8) ~ "Summer",
        ADM_MOI %in% c(9,10,11) ~ "Fall"
      ),
    ## education
      education = ifelse(EDUG21==9,NA,EDUG21),
    ## income
      income = ifelse(INCGHH==99, NA, INCGHH),
    ## marital status
      marital_status = ifelse(DHHGMS>5, NA, DHHGMS),
    ## physical activity
      phys_act_mod = ifelse(PHSGAPA>14, NA, PHSGAPA),
    ## food security
      food_security = ifelse(FSCDHFS2>2, NA, FSCDHFS2),
    ## smoking status
    smoking = case_when(
      SMK_202 == 3 ~ 0,
      SMK_202 == 2 ~ 1,
      SMK_202 == 1 ~ 2 ),
    ## height, weight,body mass index
      bodyweight = ifelse(MHWGWTK>=150, NA, MHWGWTK),
      height     = ifelse(MHWGHTM>=2, NA, MHWGHTM), 
      bmi        = ifelse(MHWGBMI>= 70, NA, MHWGBMI),
    ## supplements
      calcium_supp = 
        case_when(
          VSDFCAL == 1 ~ 1,
          VSDFCAL == 2 ~ 0,
          VSDFDMG == 6 ~ 0,
          .default = NA),
    vit_d_supp = 
      case_when(
        VSDFDMG == 1 ~ 1,
        VSDFDMG == 2 ~ 0,
        VSDFDMG == 6 ~ 0,
        .default = NA),
    ## pseudo-outcomes
    self_rated_health = ifelse(GENDHDI>4, NA, GENDHDI),
    self_reported_bp  =
        case_when(
          CCC_071 == 1 ~ 1,
          CCC_071 == 2 ~ 0,
          .default = NA),
    self_reported_db  =
      case_when(
        CCC_101 == 1 ~ 1,
        CCC_101 == 2 ~ 0,
        .default = NA),
    self_reported_chd  =
      case_when(
        CCC_121 == 1 ~ 1,
        CCC_121 == 2 ~ 0,
        .default = NA),
    self_reported_cancer  =
      case_when(
        CCC_131 == 1 ~ 1,
        CCC_131 == 2 ~ 0,
        .default = NA),
    self_reported_osteo  =
      case_when(
        CCC_401 == 1 ~ 1,
        CCC_401 == 2 ~ 0,
        .default = NA),
  ) |>
  select(
    # id
    ADM_RNO, WTS_P,
    # 24hr
    r24_season, r24_weekend, R24DCNT, # ADMDD
    # characteristics
    GEO_PRV, DHH_AGE, DHH_SEX, DHHDDRI, education, income, marital_status,
    smoking, phys_act_mod,
    food_security, bodyweight, height, bmi, 
    # pseudo-outcomes
    starts_with("self"),
    # dietary intakes
    FSDDWTG, FSDDEKC, FSDDSOD,
    FSDDFAT, FSDDFAS, FSDDFAM, FSDDFAP,
    FSDDCAR, FSDDFI, FSDDPRO, FSDDALC,
    FSDDCAL, FSDDDMG,
    calcium_supp, vit_d_supp
  ) |>
  rename(
    participantid=ADM_RNO,
    r24_nfoods=R24DCNT, # r24_day=ADMDD, r24_month=ADM_MOI, r24_weekend=ADMFW
    province=GEO_PRV, sex=DHH_SEX, age=DHH_AGE, drig=DHHDDRI,
    foodwgt=FSDDWTG, energy=FSDDEKC, sodium=FSDDSOD,
    fat=FSDDFAT, sfa=FSDDFAS, mufa=FSDDFAM, pufa=FSDDFAP,
    cho=FSDDCAR, fibers=FSDDFI, protein=FSDDPRO, roh_g =FSDDALC,
    vit_d= FSDDDMG, calcium =FSDDCAL
  ) |>
  arrange(participantid) 


# *********************************************************************** #
#                  Add metadata (labels, factors, ...)                    #
# *********************************************************************** #

# Factor / description
hs$sex <- 
  factor(hs$sex,
         levels = c(1,2),
         labels = c(
           "Male",
           "Female"))

hs$marital_status <- 
  factor(hs$marital_status,
         levels = c(1,2,3,4),
         labels = c(
           "Married",
           "Living common-law",
           "Widowed/Divorced/Separated",
           "Single, never married")
  )

hs$province <- 
  factor(hs$province,
         levels = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59),
         labels = c(
           "Newfoundland and Labrador",
           "Prince Edward Island",
           "Nova Scotia",
           "New Brunswick",
           "Quebec",
           "Ontario",
           "Manitoba",
           "Saskatchewan",
           "Alberta",
           "British Columbia")
  )

# Labels
labelled::var_label(hs) <-
  list(participantid = "Unique identifier",
       WTS_P = "Sampling weight",
       #r24_day = "Day of the week (24-h dietary recall)",
       r24_season = "Season (24-h dietary recall)",
       r24_weekend = "Weekend (24-h dietary recall)",
       r24_nfoods  = "Number of items reported (24-h dietary recall)",
       province = "Province of residence",
       sex = "Sex",
       age = "Age, years",
       drig = "Dietary Reference Intake age/sex group",
       education = "Education level",
       income = "Total household income",
       marital_status = "Marital status",
       smoking = "Smoking status",
       phys_act_mod = "Moderate or vigorous physical activity, hours/week",
       food_security = "Household Food Security Status",
       height = "Height, meters",
       bodyweight = "Bodyweight, kg",
       bmi = "Body mass index, kg/m2",
       self_rated_health = "Self-rated health",
       self_reported_bp  = "Self-reported high blood pressure",
       self_reported_db  = "Self-reported diabetes",
       self_reported_chd = "Self-reported heart disease",
       self_reported_cancer = "Self-reported heart cancer",
       self_reported_osteo = "Self-reported heart osteoporosis",
       foodwgt = "Amount of food, g/d",
       energy  = "Energy intake, kcal/d",
       sodium  = "Sodium intake, mg/d", 
       fat     = "Fat intake, g/d",
       sfa     = "Saturated fat intake, g/d",
       mufa    = "Monounsaturated fat intake, g/d",
       pufa    = "Polyunsaturated fat intake, g/d",
       cho     = "Carbohydrate intake, g/d",
       fibers  = "Fibers intake, g/d",
       protein = "Protein intake, g/d",
       roh_g   = "Alcohol intake, g/d",
       calcium = "Calcium intake, mg/d",
       vit_d   = "Vitamin D intake, g/d",
       calcium_supp = "Took a supplement containing calcium",
       vit_d_supp  = "Took a supplement containing vitamin D"
       )


# *********************************************************************** #
#                 Select random subsample of respondents                  #
# *********************************************************************** #

# select participant | WTS_P

set.seed(123)
  
cchs2015_demonstration <-
  hs |>
  filter(energy>100) |> # keep those with least 100 kcal
  slice_sample(n=1000, weight_by=WTS_P) |>
  select(-WTS_P)

# overview
library(gtsummary)

cchs2015_demonstration |>
  gtsummary::tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}-{max}]")
  ) |>
  modify_caption("Data dictionnary for <cchs2015_demonstration>")

# *********************************************************************** #
#                    Output data for further analysis                     #
# *********************************************************************** #

# save 
save(cchs2015_demonstration,
     file = here::here("data","processed","cchs2015_demonstration.rdata"))

# csv file for transportability
write.csv(cchs2015_demonstration,
          file = here::here("data","processed","cchs2015_demonstration.csv"),
          row.names = FALSE)
