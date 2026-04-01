#####################################################
###### Project smoking exposure & dental caries #####
#####################################################

# Project: Association of Maternal and Household Smoking with Childhood & Adolescent Dental Caries (in the Generation R Study cohort)

# Author: Anna Suleri 

# Date: 23-03-26 

#------------------------------------------------------------#
####---------------Step 1: Create dataframe---------------####
#------------------------------------------------------------#

## Clear environment 
rm(list=ls()) 

## Set seed for reproducibility 
set.seed(2026) 

## Set working directory
setwd("set_path_to_data_folder")
wd <- getwd()

## Load libraries 
libraries <- c("foreign", "haven", "dplyr", "mice", "xlsx", "corrplot", "ggplot2", "ggpubr","ggplot2", "RColorBrewer", "broom.mixed", "lattice", 'tidyverse', 'writexl', 'stringi', 'miceadds', 'mitools', 'CBPS', 'survey', 'survival', 'psych', 'plotly', 'sjPlot', 'lme4', 'lattice', 'car', 'variancePartition', 'viridis')

invisible(lapply(libraries, require, character.only = T))

# ============================================================================ #

## Load data and select variables of interest
dataframes_folder <- wd

load_files <- function(folder, pattern, read_function) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  
  for (file in files) {
    object_name <- tools::file_path_sans_ext(basename(file))
    assign(object_name, read_function(file), envir = .GlobalEnv)
  }
}

load_files(dataframes_folder, "\\.sav$", function(file) read.spss(file, to.data.frame = TRUE))

# Select exposures (maternal + household smoking during pregnancy, and at child's age 2 months, 6 months, 5 years, 9 years, 13 years) & merge into one df
df_smoking_pregnancy <- select(MATERNALSMOKING_22112016, IDM = idm, m_smoking_pregnancy = SMOKE_ALL, m_avg_smoking_tri1 = smk1, m_avg_smoking_tri2 = smk2, m_avg_smoking_tri3 = smk3)
df_smoking_pregnancy <- merge(df_smoking_pregnancy, `IDC-IDM-MOTHER`, by = 'IDM')

df_smoking_2months <- select(`GR1019-C1-5_01072012`, IDC, AGE2M = AGE2M_M, smoke_home_2m = C0100119, m_avg_cigarettes_2m = C0200219, m_avg_cigars_2m = C0200319, m_avg_pipes_2m = C0200419, p_avg_cigarettes_2m = C0300219, p_avg_cigars_2m = C0300319, p_avg_pipes_2m = C0300419, others_avg_cigarettes_2m = C0400219, others_avg_cigars_2m = C0400319, others_avg_pipes_2m = C0400419)

df_smoking_6months <- select(`GR1024-D9-15_01072012`, IDC, AGE6M = AGE6M_M, m_smoke_home_6m = D0900124, m_avg_smoking_total_6m = D0900224, others_smoke_home_6m = D1000124, others_avg_cigarettes_6m = D1000224, others_avg_cigars_6m = D1000324, others_avg_pipes_6m = D1000424)

df_smoking_5years <- select(`GR1076-E345_28032012`, IDC, AGE5Y = agechildGR1076, smoke_home_5y = E0300176, m_smoke_home_5y = E0400176, p_smoke_home_5y = E0500176) # number of cigs not available (data not cleaned in genr)

df_smoking_9years <- select(GR1081_C1_08082016, IDC, AGE9Y = AgechildGR1081, m_smoke_home_9y = C0100181_cleaned, m_avg_smoking_total_9y = C0100581_cleaned)
df_smoking_9years$AGE9Y <- df_smoking_9years$AGE9Y * 12 # transform to months in line with other age vars

df_smoking_13years <- select(GR1093_C_04102021, IDC, m_smoke_home_13y = C001001R9301_cleaned, m_avg_smoking_total_13y = C001002R9301_cleaned)

df_smoking <- merge(df_smoking_pregnancy, df_smoking_2months, by = 'IDC', all.x = T)
df_smoking <- merge(df_smoking, df_smoking_6months, by = 'IDC', all.x = T)
df_smoking <- merge(df_smoking, df_smoking_5years, by = 'IDC', all.x = T)
df_smoking <- merge(df_smoking, df_smoking_9years, by = 'IDC', all.x = T)
df_smoking <- merge(df_smoking, df_smoking_13years, by = 'IDC', all.x = T)

# Select covariates (maternal alcohol use during pregnancy, maternal education, ethnic status, net household income, sex at birth, nocturnal bottle feeding at 6 months, tooth brushing at age 5 and 13 years, dental visits in the past year at age 5 and 13 years, intake of sugar containing products [sweets, chocolates, soft drinks] at age 5 and 13 years + additionally adding gestational age at birth)
df_remove_ids <- select(`CHILD-ALLGENERALDATA_24102022`, IDC, IDM, RemoveData)
covars1 <- select(`FETALPERIOD-ALLGENERALDATA_24102022`, IDM, AGE_M_v2, AGE_P_v2, ETHNMv2, ETHNFv2, ETHNP, EDUCM, EDUCM, EDUCP, INCOME, BMI_0)
covars2 <- select(feeding_6m, IDC, A0300125)
covars3 <- select(`GR1075-F1-9_17072015`, IDC, brushing_5y = F0400575, dental_visit_5y = F0400375)
covars4 <- select(`oral habits_13`, IDC, brushing_13y = brushing_13, dental_visit_13y = visit_13)
covars5 <- select(calorie_5y, IDC, calorierijk_tussendoortje_weekdag_5y = E0300175, calorierijk_tussendoortje_weekend_5y = E0300275)
covars6 <- select(GR1094_C8_22072022, IDC, snoep_of_chocola_13y = C008003R9401_cleaned, frisdrank_13y = C008004R9401_cleaned) # options: nooit, minder dan 1 x p/w, 1 x p/w, 2-4 d p/w, 5-6 d p/w, 1 dd, > 1 dd
covars7 <- select(MATERNALALCOHOL_22112016, IDM = idm, m_alcohol_use_pregnancy = alc0)
covars8 <- select(Covariaten_Anna, IDC, GESTBIR, GENDER)

df_covars <- merge(df_remove_ids, covars1, by = 'IDM', all.x = T)
df_covars <- merge(df_covars, covars2, by = 'IDC', all.x = T)
df_covars <- merge(df_covars, covars3, by = 'IDC', all.x = T)
df_covars <- merge(df_covars, covars4, by = 'IDC', all.x = T)
df_covars <- merge(df_covars, covars5, by = 'IDC', all.x = T)
df_covars <- merge(df_covars, covars6, by = 'IDC', all.x = T)
df_covars <- merge(df_covars, covars7, by = 'IDM', all.x = T)
df_covars <- merge(df_covars, covars8, by = 'IDC', all.x = T)

# Select outcomes (dental caries at 5 and 13 years using decayed, missing and filled teeth index [(DMFT) index]) & merge into one df
df_teeth5 <- select(Teeth_5, IDC, dmft_5y = dmft)
df_teeth13 <- select(Teeth_13, IDC, dmft_13y = DMFTF13, orthodontie_13y = orthodontie)

df_teeth <- merge(df_teeth5, df_teeth13, by = "IDC", all.x = T)

# ============================================================================ #

## Create combined dataframe 
df_combined <- merge(df_smoking, df_teeth, by = 'IDC', all.x = T)
full_df <- merge(df_combined, df_covars, by = c('IDC', 'IDM'), all.x = T)

# ============================================================================ #

## Rename / restructure variables in dataframe if needed 
str(full_df)

# Exposure
full_df$m_smoking_pregnancy_bin <- as.factor(ifelse(full_df$m_smoking_pregnancy == "continued smoking in pregnancy", 'yes', ifelse(full_df$m_smoking_pregnancy %in% c("never smoked during pregnancy", "smoked until pregnancy was known"), 'no', NA)))

full_df$smoke_home_2m <- factor(full_df$smoke_home_2m, levels = c("No", "Yes"), labels = c("no", "yes"))

full_df$smoke_home_5y_combined <- as.factor(ifelse(full_df$m_smoke_home_5y == "Yes" | full_df$p_smoke_home_5y == "Yes", "yes", ifelse(full_df$m_smoke_home_5y == "No" | full_df$p_smoke_home_5y == "No", "no", NA)))

full_df$m_smoke_home_9y_bin <- as.factor(ifelse(full_df$m_smoke_home_9y %in% c('Yes, but I do not smoke anymore', 'No, I have never smoked'), 'no', ifelse(full_df$m_smoke_home_9y == 'Yes, I still smoke now', 'yes', NA))) # questionnaire q about past smoking: 'did you ever smoke' 

full_df$m_smoke_home_13y_bin <- as.factor(ifelse(full_df$m_smoke_home_13y %in% c('No, never smoked', 'Yes, but no longer smokes'), 'no', ifelse(full_df$m_smoke_home_13y == 'Yes, still smokes', 'yes', NA))) # questionnaire q about past smoking: 'did you ever smoke' 

# Covars
full_df$m_education <- as.factor(ifelse(full_df$EDUCM == "no education finished" | full_df$EDUCM == "primary", "Low", ifelse(full_df$EDUCM == "secondary, phase 1" | full_df$EDUCM == "secondary, phase 2", "Middle", ifelse(full_df$EDUCM == "higher, phase 1" | full_df$EDUCM =="higher, phase 2", "High", NA))))

full_df$p_education <- as.factor(ifelse(full_df$EDUCP == "no education finished" | full_df$EDUCP == "primary", "Low", ifelse(full_df$EDUCP == "secondary, phase 1" | full_df$EDUCP == "secondary, phase 2", "Middle", ifelse(full_df$EDUCP == "higher, phase 1" | full_df$EDUCP =="higher, phase 2", "High", NA))))

full_df$household_income <- as.factor(ifelse(full_df$INCOME == 'less than 450' | full_df$INCOME == '450-600 euro' | full_df$INCOME == '600-700 euro' | full_df$INCOME == '700-800 euro' | full_df$INCOME == '800-900 euro' | full_df$INCOME == '900-1200 euro' | full_df$INCOME == '1200-1400 euro' | full_df$INCOME == '1400-1600 euro' | full_df$INCOME == '1600-1800 euro' | full_df$INCOME == '1800-2000 euro' | full_df$INCOME == '2000-2200 euro', '<2000', ifelse(is.na(full_df$INCOME), NA, '>2000')))

full_df$ethnicity_child <- as.factor(ifelse(full_df$ETHNFv2 == 'Dutch', 'Dutch', ifelse(full_df$ETHNFv2 == 'Indonesian' | full_df$ETHNFv2 == 'Cape Verdian' | full_df$ETHNFv2 == 'Moroccan' | full_df$ETHNFv2 == 'Dutch Antilles' | full_df$ETHNFv2 == 'Surinamese' | full_df$ETHNFv2 == 'Turkish' | full_df$ETHNFv2 == 'African' | full_df$ETHNFv2 == 'American, western' | full_df$ETHNFv2 == 'American, non western' | full_df$ETHNFv2 == 'Asian, western' | full_df$ETHNFv2 == 'Asian, non western' | full_df$ETHNFv2 == 'European' | full_df$ETHNFv2 == 'Oceanie', 'Not Dutch', NA))) 

full_df$breastfeeding <- as.factor(ifelse(full_df$A0300125 == 'I no longer breastfeed', "no", ifelse(full_df$A0300125 %in% c('1 to 2 times a day', '2 to 3 times a day', ' 3 to 5 times a day', '5 to 7 times a day', 'more than 7 times a day'), 'yes', NA)))

full_df$sugar_snacks_5y_weekend <- as.factor(ifelse(full_df$calorierijk_tussendoortje_weekend_5y == 'Never or less than once per day', 'no', ifelse(full_df$calorierijk_tussendoortje_weekend_5y %in% c('Once per day', '2 or 3 times per day', '4 to 6 times per day', '7 or more times per day'), 'yes', NA)))

full_df$sugar_snacks_5y_weekday <- as.factor(ifelse(full_df$calorierijk_tussendoortje_weekdag_5y == 'Never or less than once per day', 'no', ifelse(full_df$calorierijk_tussendoortje_weekdag_5y %in% c('Once per day', '2 or 3 times per day', '4 to 6 times per day', '7 or more times per day'), 'yes', NA)))

full_df$sugar_snacks_combined_5y <- as.factor(ifelse(full_df$sugar_snacks_5y_weekday == 'yes' | full_df$sugar_snacks_5y_weekend == 'yes', 'yes', ifelse(full_df$sugar_snacks_5y_weekday == 'no' | full_df$sugar_snacks_5y_weekend == 'no', 'no', NA)))

full_df$sugar_snacks_13y <- as.factor(ifelse(full_df$snoep_of_chocola_13y == 1, 'no', ifelse(full_df$snoep_of_chocola_13y > 1, 'yes', NA)))

full_df$sugar_drinks_13y <- as.factor(ifelse(full_df$frisdrank_13y == 1, 'no', ifelse(full_df$frisdrank_13y > 1, 'yes', NA)))

full_df$sugar_snacks_combined_13y <- as.factor(ifelse(full_df$sugar_drinks_13y == 'yes' | full_df$sugar_snacks_13y == 'yes', 'yes', ifelse(full_df$sugar_drinks_13y == 'no' | full_df$sugar_drinks_13y == 'no', 'no', NA)))

# Outcome
full_df$orthodontie_13y <- as.character(full_df$orthodontie_13y)
full_df$orthodontie_13y[full_df$orthodontie_13y %in% c("2", "20")] <- NA # unknown categories, set these 8 participants to NA
full_df$orthodontie_13y <- factor(full_df$orthodontie_13y)

full_df$dmft_5y_bin <- as.factor(ifelse(full_df$dmft_5y == 0, 'no', ifelse(full_df$dmft_5y > 0, 'yes', NA)))
full_df$dmft_13y_bin <- as.factor(ifelse(full_df$dmft_13y == 0, 'no', ifelse(full_df$dmft_13y > 0, 'yes', NA)))

#\

#------------------------------------------------------------------------#
####------------------Step 2: Create inclusion variable---------------####
#------------------------------------------------------------------------#

## Apply inclusion & exclusion criteria 
# First remove ids that no longer want to participate in GenR (n = 9897)
full_df <- full_df %>% filter(is.na(RemoveData) | RemoveData != "Set all data as missing")

# Remove participants with missing data at one of the exposure time points (n = 9260)
inclu1 <- subset(full_df, complete.cases(m_smoking_pregnancy) | complete.cases(smoke_home_2m) | complete.cases(m_smoke_home_6m) | complete.cases(m_smoke_home_5y) | complete.cases(m_smoke_home_9y) | complete.cases(m_smoke_home_13y))

# Remove participants with missing data at one of the outcome time points (n = 5936)
inclu2 <- subset(inclu1, complete.cases(dmft_5y) | complete.cases(dmft_13y))

# Remove one pair of sibling/twins based on least available data (n = 5530)
inclu3 <- inclu2[sample(nrow(inclu2)),]
inclu3$na_count <- apply(inclu3, 1, function(x) sum(is.na(x)))
inclu3 <- inclu3[order(inclu3$na_count),]
inclu3 <- inclu3[!duplicated(inclu3$MOTHER, fromLast = T),]

## Create inclusion variable
# Create list of ids of participants that should be included based on criteria applied above
ids_include <- merge(inclu1, inclu2, by = c(1:76))
ids_include <- merge(ids_include, inclu3, by = c(1:76))
ids_include <- select(ids_include, 'IDC')

# Create inclusion/exclusion variable in full dataset (full_df) based on the children who are left after applying above inclusion/exclusion criteria 
full_df$include <- as.factor(ifelse(full_df$IDC %in% ids_include$IDC, 1, 0))

#\

#-----------------------------------------------------------------------#
####---------------Step 3: Create descriptive figures---------------####
#----------------------------------------------------------------------#

## Select participants that should be included for downstream analyses 
df_descriptives <- subset(full_df, include == 1)

## Create baseline table
# For total sample (n = 5530)
baselinevars <- c('AGE_M_v2', 'BMI_0','m_alcohol_use_pregnancy', 'm_smoking_pregnancy', 'm_avg_smoking_tri1', 'm_avg_smoking_tri2', 'm_avg_smoking_tri3', 'smoke_home_2m', 'm_smoke_home_6m', 'smoke_home_5y_combined', 'm_smoke_home_9y', 'm_smoke_home_13y', 'm_education', 'p_education', 'household_income', 'dmft_5y', 'dmft_13y',  'GESTBIR', 'GENDER', 'ethnicity_child', 'breastfeeding', 'sugar_snacks_combined_5y', 'sugar_snacks_combined_13y', 'brushing_5y', 'dental_visit_5y' ,'brushing_13y', 'dental_visit_13y')

baseline_table <- function(df, baselinevars) {
  
  # function for continuous variables
  summary_continuous <- function(x) {
    standev <- sd(x, na.rm = TRUE)
    meanvar <- mean(x, na.rm = TRUE)
    return(paste0(round(meanvar, 1), " (", round(standev, 1), ")"))
  }
  
  # function for categorical variables
  summary_categorical <- function(x) {
    tab1 <- prop.table(table(x, useNA = "always"))
    tab2 <- table(x, useNA = "always")
    
    perc <- paste0(round(tab1 * 100, 1), "% ", names(tab1), collapse = ", ")
    counts <- paste(tab2, names(tab2), collapse = ", ")
    
    return(list(percentages = perc, counts = counts))
  }
  
  # loop over variables
  for (i in baselinevars) {
    
    # extract column
    x <- df[[i]]
    
    # show variable name
    message("\nVariable: ", i)
    
    # apply correct summary
    if (is.numeric(x)) {
      result <- summary_continuous(x)
      print(result)
    } else {
      result <- summary_categorical(x)
      print(result$percentages)
      print(result$counts)
    }
  }
}

baseline_table(df_descriptives, baselinevars)

# For outcome f5 sample (n = 4851)
df_descriptives_f5 <- subset(df_descriptives, complete.cases(dmft_5y))
baseline_table(df_descriptives_f5, baselinevars)

# For outcome f13 sample (n = 3459)
df_descriptives_f13 <- subset(df_descriptives, complete.cases(dmft_13y))
baseline_table(df_descriptives_f13, baselinevars)

# Subsample DFMT <0 for f5 (n = 3324)
baselinevars2 <- c('m_smoking_pregnancy', 'smoke_home_2m', 'm_smoke_home_6m', 'smoke_home_5y_combined', 'm_smoke_home_9y', 'm_smoke_home_13y')

df_descriptives_dfmt0_f5 <- subset(df_descriptives_f5, dmft_5y_bin == 'no')
baseline_table(df_descriptives_dfmt0_f5, baselinevars2)

# Subsample DFMT >0 for f5 (n = 1527)
df_descriptives_dfmt1_f5 <- subset(df_descriptives_f5, dmft_5y_bin == 'yes')
baseline_table(df_descriptives_dfmt1_f5, baselinevars2)

# Subsample DFMT <0 for f14 (n = 2168)
df_descriptives_dfmt0_f13 <- subset(df_descriptives_f13, dmft_13y_bin == 'no')
baseline_table(df_descriptives_dfmt0_f13, baselinevars2)

# Subsample DFMT >0 for f14 (n = 1291)
df_descriptives_dfmt1_f13 <- subset(df_descriptives_f13, dmft_13y_bin == 'yes')
baseline_table(df_descriptives_dfmt1_f13, baselinevars2)

# ============================================================================ #

## Create descriptive plot
# Correlation plot 
corr_vars <- select(df_descriptives, c('m_smoking_pregnancy', 'smoke_home_2m', 'm_smoke_home_6m', 'smoke_home_5y_combined', 'm_smoke_home_9y', 'm_smoke_home_13y', 'dmft_5y', 'dmft_13y', 'BMI_0', 'brushing_5y', 'brushing_13y', 'dental_visit_5y', 'dental_visit_13y', 'm_alcohol_use_pregnancy', 'GESTBIR', 'GENDER', 'm_education', 'p_education', 'household_income', 'ethnicity_child', 'breastfeeding', 'sugar_snacks_combined_5y','sugar_snacks_13y', 'AGE_M_v2', 'orthodontie_13y'))

corr_vars2 <- rename(corr_vars,
                     'Maternal smoking during pregnancy' = m_smoking_pregnancy,
                     'Household smoking at child 2 months' = smoke_home_2m,
                     'Household smoking at child 6 months' = m_smoke_home_6m,
                     'Household smoking at child 6 years' = smoke_home_5y_combined,
                     'Household smoking at child 10 years' = m_smoke_home_9y,
                     'Household smoking at child 14 years' = m_smoke_home_13y,
                     'DMFT index at child 5 years' = dmft_5y,
                     'DMFT index at child 14 years' = dmft_13y,
                     'Maternal age' = AGE_M_v2,
                     'Pre-pregnancy BMI' = BMI_0,
                     'Teeth brushing at child 6 years' = brushing_5y,
                     'Teeth brushing at child 14 years' = brushing_13y,
                     'Dental visit at child 6 years' = dental_visit_5y,
                     'Dental visit at child 14 years' = dental_visit_13y,
                     'Maternal alcohol use during pregnancy' = m_alcohol_use_pregnancy,
                     'Gestational age at birth' = GESTBIR,
                     'Biological sex at birth' = GENDER,
                     'Maternal education level' = m_education,
                     'Paternal education level' = p_education,
                     'Household income' = household_income,
                     'Child national background' = ethnicity_child,
                     'Breastfeeding' = breastfeeding,
                     'Sugar snacks at child 6 years' = sugar_snacks_combined_5y,
                     'Sugar snacks at child 14 years' = sugar_snacks_13y,
                     'Orthodontics at child 14 years' = orthodontie_13y)

corr_vars2[] <- lapply(corr_vars2, as.numeric)

correlation <- cor(corr_vars2, use="pairwise.complete.obs")

corrplot(correlation, method = 'color', addCoef.col = "black", number.cex=0.3, order = 'FPC', type = 'lower', diag = F, tl.col = 'black', tl.cex = 0.4, col = colorRampPalette(c("midnightblue","white","darkred"))(100),tl.srt = 45)

#\

#---------------------------------------------------------------#
####---------------Step 4: Multiple imputation---------------####
#---------------------------------------------------------------#

## Check missing values per variable
# Function to calculate missingness 
miss_values_function <- function(df){
  missvalues <- cbind("# NA" = sort(colSums(is.na(df))),
                      "% NA" = round(sort(colMeans(is.na(df))) * 100, 2))
  print(missvalues)
}

# Remove unnecessary covars
full_df_short <- select(full_df, -c('RemoveData', 'm_avg_cigars_2m', 'm_avg_pipes_2m', 'p_avg_cigars_2m', 'p_avg_pipes_2m', 'others_avg_cigarettes_2m' , 'others_avg_cigars_2m', 'others_avg_pipes_2m', 'others_smoke_home_6m', 'others_avg_cigarettes_6m', 'others_avg_cigars_6m', 'others_avg_pipes_6m', 'smoke_home_5y', 'ETHNFv2', 'ETHNP', 'ETHNMv2', 'EDUCP', 'EDUCM', 'INCOME', 'A0300125', 'calorierijk_tussendoortje_weekdag_5y', 'calorierijk_tussendoortje_weekend_5y', 'snoep_of_chocola_13y', 'frisdrank_13y', 'sugar_snacks_5y_weekend', 'sugar_snacks_5y_weekday', 'sugar_drinks_13y', 'sugar_snacks_13y'))

# Calculate missiginess per var (range missingness in covariates 1.6% to 67.2% in full df and 0.6% to 60.4% in included sample)
miss_values_function(full_df_short)
miss_values_function(df_descriptives)
                         
## Impute missing data in covariates (30 datasets, 50 iterations) on whole dataset for better quality imputation, later select only included participants
# Function
imputation_function <- function(data, exclude_imp_vars, exclude_predictors, method = "default") {
  
  # Running setup imputation run
  if (method == "rf") {  
    imp0 <- mice(data, maxit = 0, method = "rf")
    
  } else {  
    imp0 <- mice(data, maxit = 0)
  }
  
  # Method martix
  meth <- imp0$method
  
  # Apply PMM if selected
  if (method == "pmm") {
    is_num <- sapply(data, is.numeric)
    meth[is_num & meth != ""] <- "pmm"
  }
  
  # Imputation method matrix
  meth[exclude_imp_vars] <- ""
  
  # Predictor matrix
  pred <- imp0$predictorMatrix
  pred[exclude_predictors] <- 0
  
  # Visit sequence
  visSeq <- imp0$visitSequence
  
  # Performing the imputation
  imp.test <- mice(data, method = meth, predictorMatrix = pred, visitSequence = visSeq, maxit = 30, m = 50, printFlag = TRUE, seed = 2026)  
  
  #get the dataframe name 
  dataName <- deparse(substitute(data))
  
  #assigning a dynamic name to the imp.test object
  imputedDataName <- paste0("imputedData_", dataName)
  assign(imputedDataName, imp.test)
  
  # Saving the imputed dataset as .RDS file
  saveRDS(imp.test, file = paste0(imputedDataName, ".rds"))
  
  # Output 
  return(list(imp0 = imp0, imp.test = imp.test))
}

# Apply imputation
imputation_function(data = full_df_short,exclude_imp_vars = c(1:3, 4:24, 27, 36:39, 41, 47:49), exclude_predictors = c(1:3, 4:24, 27, 36:39, 41, 47:49))

## Load imputed data and check convergence 
imputed_df <- readRDS('imputedData_full_df_short.RDS')

# Check convergence
plot(imputed_df)

#\ 

#------------------------------------------------------------#
####-----------Step 5: Logistic regression models-----------####
#------------------------------------------------------------#

## Select only included sample 
imp_mids_final <- filter(imputed_df, get("include") == 1)

## Check model assumptions on single imputed df
check_model_assumptions <- function(model) {
  
  # Check multicollinearity using VIF
  vif_values <- car::vif(model)
  print("Variance Inflation Factors (VIF):")
  print(vif_values)
  
  # Distribution of residuals
  res <- resid(model)
  par(mfrow = c(2, 2))  
  qqnorm(res)
  qqline(res)
  plot(density(res), main = "Density Plot of Residuals")
  
  # Heteroscedasticity assumption
  plot(fitted(model), res, main = "Residuals vs. Fitted Values")
  abline(h = 0, lty = 2, col = "red")
}

# Specify glm for outcome at 6 years and pregnancy smoking exposure (for single imputed df & complete cases df)
single_df <- complete(imp_mids_final, 30) # single imputed df 

complete_cases_df_f5 <- subset(df_descriptives, complete.cases(dmft_5y_bin) & complete.cases(m_smoking_pregnancy) & complete.cases(AGE_M_v2) & complete.cases(brushing_5y) & complete.cases(dental_visit_5y) & complete.cases(m_alcohol_use_pregnancy) & complete.cases(GESTBIR) & complete.cases(GENDER) & complete.cases(m_education) & complete.cases(household_income) & complete.cases(ethnicity_child) & complete.cases(breastfeeding) & complete.cases(sugar_snacks_combined_5y)) # complete cases df as sanity check 

fit_assumptions_imputed_df <- glm(dmft_5y_bin ~  m_smoking_pregnancy + AGE_M_v2 + brushing_5y + dental_visit_5y + m_alcohol_use_pregnancy + GESTBIR + GENDER + m_education + household_income + ethnicity_child + breastfeeding + sugar_snacks_combined_5y, data = single_df, family = binomial(link = 'logit')) # on single imputed df 

summary(glm(dmft_5y_bin ~ m_smoking_pregnancy + AGE_M_v2 + brushing_5y + dental_visit_5y + m_alcohol_use_pregnancy + GESTBIR + GENDER + m_education + household_income + ethnicity_child + breastfeeding + sugar_snacks_combined_5y, data = complete_cases_df_f5, family = binomial)) # on complete cases df 

# Check model assumptions ()
check_model_assumptions(fit_assumptions_imputed_df) 
influencePlot(fit_assumptions_imputed_df)

# ============================================================================ #

### Apply logistic regression models for crude models on imputed dataset
## Apply logistic regression models for F5 (crude)

# Create vector of exposures to loop over
exposure_vars_glm <- c('m_smoking_pregnancy', 'm_smoking_pregnancy_bin', 'm_avg_smoking_tri1', 'm_avg_smoking_tri2', 'm_avg_smoking_tri3', 'smoke_home_2m', 'm_smoke_home_6m', 'smoke_home_5y_combined', 'm_smoke_home_9y' ,'m_smoke_home_9y_bin', 'm_smoke_home_13y' ,'m_smoke_home_13y_bin')

# Create empty dataframe to store results in
results_glm_f5_crude <- data.frame()

# Loop over exposures
for (exposure in exposure_vars_glm) {
  
  # Formula
  formula_str <- paste0(
    'dmft_5y_bin ~ ', exposure
  )
  
  # Unweighted model
  model_unweighted <- with(imp_mids_final,
                           glm(as.formula(formula_str),
                               family = binomial))
  pooled_unweighted <- pool(model_unweighted)
  summary_unweighted <- summary(pooled_unweighted, conf.int = TRUE)
  
  # Filter rows that relate to this model 
  exposure_rows <- summary_unweighted %>% 
    filter(grepl(exposure, term)) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(`2.5 %`),
      CI_upper = exp(`97.5 %`),
      exposure = exposure,
      model_type = "unweighted"
    )
  
  # Add to results dataframe
  results_glm_f5_crude <- bind_rows(results_glm_f5_crude, exposure_rows)
  
}

# Check results
results_glm_f5_crude <- results_glm_f5_crude %>% select(exposure, term, OR, CI_lower, CI_upper, p.value)

# Clean up tables to 3 decimals
results_glm_f5_crude[, 3:5] <- round(results_glm_f5_crude[, 3:5], digits = 3)

# Apply FDR correction
add_fdr_pvalue <- function(data_frame, pvalue_column_index) {
  pval <- unlist(data_frame[, pvalue_column_index])
  data_frame$fdr_pvalue <- p.adjust(pval, method = 'fdr')
  return(data_frame)
}

results_glm_f5_crude_fdr <- add_fdr_pvalue(results_glm_f5_crude, 6) 

# Save dataframes to excel 
setwd("set_path_to_results_folder")
write_xlsx(results_glm_f5_crude_fdr, 'crude_results_glm_model_dental_caries_f5__project.xlsx')

#\ 

## Apply logistic regression models for f13 (crude) on imputed dataset
# Create empty dataframe to store results in
results_glm_f13_crude <- data.frame()

# Loop over expososures
for (exposure in exposure_vars_glm) {
  
  # Formula
  formula_str <- paste0(
    'dmft_13y_bin ~ ', exposure
  )
  
  # Unweighted model
  model_unweighted <- with(imp_mids_final,
                           glm(as.formula(formula_str),
                               family = binomial))
  pooled_unweighted <- pool(model_unweighted)
  summary_unweighted <- summary(pooled_unweighted, conf.int = TRUE)
  
  # Filter rows that relate to this model 
  exposure_rows <- summary_unweighted %>% 
    filter(grepl(exposure, term)) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(`2.5 %`),
      CI_upper = exp(`97.5 %`),
      exposure = exposure,
      model_type = "unweighted"
    )
  
  # Add to results dataframe
  results_glm_f13_crude <- bind_rows(results_glm_f13_crude, exposure_rows)
  
}

# Check results
results_glm_f13_crude <- results_glm_f13_crude %>% select(exposure, term, OR, CI_lower, CI_upper, p.value)

# Clean up tables to 3 decimals
results_glm_f13_crude[, 3:5] <- round(results_glm_f13_crude[, 3:5], digits = 3)

# Apply FDR correction
results_glm_f13_crude_fdr <- add_fdr_pvalue(results_glm_f13_crude, 6) 

# Save dataframes to excel 
write_xlsx(results_glm_f13_crude_fdr, 'crude_results_glm_model_dental_caries_f13__project.xlsx')

#\ 

### Apply logistic regression models for fully adjusted models 
## Apply logistic regression models for F5 on imputed dataset
# Create empty dataframe to store results in
results_glm_f5 <- data.frame()

# Loop over expososures
for (exposure in exposure_vars_glm) {
  
  # Formula
  formula_str <- paste0(
    'dmft_5y_bin ~ ', exposure,
    ' + AGE_M_v2 + brushing_5y + dental_visit_5y + m_alcohol_use_pregnancy',
    ' + GESTBIR + GENDER + m_education + household_income + ethnicity_child + breastfeeding',
    ' + sugar_snacks_combined_5y'
  )
  
  # Unweighted model
  model_unweighted <- with(imp_mids_final,
                           glm(as.formula(formula_str),
                               family = binomial))
  pooled_unweighted <- pool(model_unweighted)
  summary_unweighted <- summary(pooled_unweighted, conf.int = TRUE)
  
  # Filter rows that relate to this model 
  exposure_rows <- summary_unweighted %>% 
    filter(grepl(exposure, term)) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(`2.5 %`),
      CI_upper = exp(`97.5 %`),
      exposure = exposure,
      model_type = "unweighted"
    )
  
  # Add to results dataframe
  results_glm_f5 <- bind_rows(results_glm_f5, exposure_rows)
  
}

# Check results
results_glm_f5 <- results_glm_f5 %>% select(exposure, term, OR, CI_lower, CI_upper, p.value)

# Clean up tables to 3 decimals
results_glm_f5[, 3:5] <- round(results_glm_f5[, 3:5], digits = 3)

# Apply FDR correction
results_glm_f5_fdr <- add_fdr_pvalue(results_glm_f5, 6) 

# Save dataframes to excel 
write_xlsx(results_glm_f5_fdr, 'results_glm_model_dental_caries_f5__project.xlsx')

# ============================================================================ #

## Apply logistic regression models for F13 on imputed dataset
# Create empty dataframe to store results in
results_glm_f13 <- data.frame()

# Loop over expososures
for (exposure in exposure_vars_glm) {
  
  # Formula
  formula_str <- paste0(
    'dmft_13y_bin ~ ', exposure,
    ' + AGE_M_v2 + brushing_13y + dental_visit_13y + m_alcohol_use_pregnancy',
    ' + GESTBIR + GENDER + orthodontie_13y + m_education + household_income + ethnicity_child + 
    breastfeeding',
    ' + sugar_snacks_combined_13y'
  )
  
  # Unweighted model
  model_unweighted <- with(imp_mids_final,
                           glm(as.formula(formula_str),
                               family = binomial))
  pooled_unweighted <- pool(model_unweighted)
  summary_unweighted <- summary(pooled_unweighted, conf.int = TRUE)
  
  # Filter rows that relate to this model 
  exposure_rows <- summary_unweighted %>% 
    filter(grepl(exposure, term)) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(`2.5 %`),
      CI_upper = exp(`97.5 %`),
      exposure = exposure,
      model_type = "unweighted"
    )
  
  # Add to results dataframe
  results_glm_f13 <- bind_rows(results_glm_f13, exposure_rows)
  
}

# Check results
results_glm_f13 <- results_glm_f13 %>% select(exposure, term, OR, CI_lower, CI_upper, p.value)

# Clean up tables to 3 decimals
results_glm_f13[, 3:5] <- round(results_glm_f13[, 3:5], digits = 3)

# Apply FDR correction
results_glm_f13_fdr <- add_fdr_pvalue(results_glm_f13, 6) 

# Save dataframes to excel 
write_xlsx(results_glm_f13_fdr, 'results_glm_model_dental_caries_f13__project.xlsx')

#\

#------------------------------------------------------------#
####-------------Step 6: Mixed-effects models-------------####
#------------------------------------------------------------#

## Transform to long dataset for downstream LMER models (time varying var = smoking exposure)
imp_list <- vector("list", 31)   

for (i in 0:30) {
  
  temp_data <- complete(imp_mids_final, i)   
  
  reshape_imp.data <- reshape(
    temp_data,
    idvar = "IDC",
    timevar = "timepoint",
    times = c('Pregnancy', '2m', '6m', '6y', '10y', '14y'),
    varying = list(
      c("m_smoking_pregnancy_bin", 'smoke_home_2m', 'm_smoke_home_6m',
        'smoke_home_5y_combined', 'm_smoke_home_9y_bin', 'm_smoke_home_13y_bin')
    ),
    v.names = "smoking",
    direction = "long"
  )
  
  imp_list[[i + 1]] <- reshape_imp.data
}

reshape_imp.data <- datalist2mids(imp_list)

saveRDS(reshape_imp.data, 'reshape_imp.data_long.rds')

#\

# ============================================================================ #

### Apply generalized mixed-effects models 
setwd("set_path_to_data_folder")

## Load long format data 
reshape_imp.data_long <- readRDS('reshape_imp.data_long.rds')

## Transform mids object to long dataframe
imp_long <- complete(reshape_imp.data_long, action = 'long', include = T)

## Select complete cases for f13 outcome
imp_long2 <- subset(imp_long, complete.cases(dmft_13y))

## Save back to mids object 
reshape_imp.data_long_f13 <- as.mids(imp_long2)

## Apply glmer for f13 outcome (main effect)
glmer_model_f13 <- with(reshape_imp.data_long_f13, glmer(dmft_13y_bin ~ smoking + timepoint + scale(AGE_M_v2) + brushing_13y + dental_visit_13y + m_alcohol_use_pregnancy + scale(GESTBIR) + GENDER + orthodontie_13y + m_education + household_income + ethnicity_child +  breastfeeding + sugar_snacks_combined_13y + (1 | IDC), family = binomial,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))))

# Pool results across imputed dfs
pooled_glmer_model_f13 <- pool(glmer_model_f13)

# Obtain summary including 95% CI 
summary_glmer_model_f13 <- summary(pooled_glmer_model_f13, conf.int = TRUE)

# Create dataframe with OR and exponentiated 95% CI
glmer_model_f13_results_OR <- summary_glmer_model_f13 %>%
  mutate(
    OR = exp(estimate),
    CI_lower = exp(`2.5 %`),
    CI_upper = exp(`97.5 %`)
  ) %>%
  select(term, OR, CI_lower, CI_upper) %>%
  rename(Variable = term)

glmer_model_f13_results_OR$p_value <- summary_glmer_model_f13$p.value

# Apply FDR correction
glmer_model_f13_results_OR_fdr <- add_fdr_pvalue(glmer_model_f13_results_OR, 5) 

# Save dataframes to excel 
setwd("set_path_to_results_folder")
write_xlsx(glmer_model_f13_results_OR_fdr, 'results_glmer_model_dental_caries_f13_project.xlsx')

## Apply glmer for f13 outcome (interaction effect with time + interaction effect with child sex)
glmer_model_f13_int <- with(reshape_imp.data_long_f13, glmer(dmft_13y_bin ~ smoking*timepoint + scale(AGE_M_v2) + brushing_13y + dental_visit_13y + m_alcohol_use_pregnancy + scale(GESTBIR) + GENDER + orthodontie_13y + m_education + household_income + ethnicity_child +  breastfeeding + sugar_snacks_combined_13y + (1 | IDC), family = binomial,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))))

glmer_model_f13_sex_int <- with(reshape_imp.data_long_f13, glmer(dmft_13y_bin ~ smoking*GENDER + timepoint + scale(AGE_M_v2) + brushing_13y + dental_visit_13y + m_alcohol_use_pregnancy + scale(GESTBIR) + orthodontie_13y + m_education + household_income + ethnicity_child +  breastfeeding + sugar_snacks_combined_13y + (1 | IDC), family = binomial,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))))

# Pool results
pooled_glmer_model_f13_int <- pool(glmer_model_f13_int)
pooled_glmer_model_f13_sex_int <- pool(glmer_model_f13_sex_int)

# Obtain summary
summary_pooled_glmer_model_f13_int <- summary(pooled_glmer_model_f13_int, conf.int = TRUE)
summary_pooled_glmer_model_f13_sex_int <- summary(pooled_glmer_model_f13_sex_int, conf.int = TRUE)

# Create DF with ORs
glmer_model_f13_results_OR_int <- summary_pooled_glmer_model_f13_int %>% mutate(OR = exp(estimate), CI_lower = exp(`2.5 %`), CI_upper = exp(`97.5 %`)) %>% select(term, OR, CI_lower, CI_upper) %>% rename(Variable = term)
glmer_model_f13_results_OR_int$p_value <- summary_pooled_glmer_model_f13_int$p.value

glmer_model_f13_results_OR_sex_int <- summary_pooled_glmer_model_f13_sex_int %>% mutate(OR = exp(estimate), CI_lower = exp(`2.5 %`), CI_upper = exp(`97.5 %`)) %>% select(term, OR, CI_lower, CI_upper) %>% rename(Variable = term)
glmer_model_f13_results_OR_sex_int$p_value <- summary_pooled_glmer_model_f13_sex_int$p.value

# Apply FDR
glmer_model_f13_results_OR_int_fdr <- add_fdr_pvalue(glmer_model_f13_results_OR_int, 5) 
glmer_model_f13_results_OR_sex_int_fdr <- add_fdr_pvalue(glmer_model_f13_results_OR_sex_int, 5) 

# Save df to excel 
write_xlsx(glmer_model_f13_results_OR_int_fdr, 'results_glmer_int_model_dental_caries_f13_project.xlsx')
write_xlsx(glmer_model_f13_results_OR_sex_int_fdr, 'results_glmer_int_sex_model_dental_caries_f13_project.xlsx')

#\ 

#------------------------------------------------------------#
####-------------------Step 7: Figures-------------------####
#------------------------------------------------------------#

## Prevalence plot exposures & outcomes
# Select + rename vars / levels for plot 
full_df_short2 <- full_df_short %>% rename(
  'Maternal smoking during pregnancy' = "m_smoking_pregnancy",
  'Household smoking at child age 2 months' = "smoke_home_2m",
  'Household smoking at child age 6 months' = "m_smoke_home_6m",
  'Household smoking at child age 6 years' = "smoke_home_5y_combined",
  'Household smoking at child age 10 years' = "m_smoke_home_9y",
  'Household smoking at child age 14 years' = "m_smoke_home_13y",
  'DMFT index at child age 6 years' = "dmft_5y_bin",
  'DMFT index at child age 14 years' = "dmft_13y_bin")

full_df_short2$`Maternal smoking during pregnancy` <- factor(full_df_short2$`Maternal smoking during pregnancy`, levels = c('never smoked during pregnancy', 'smoked until pregnancy was known', 'continued smoking in pregnancy'), labels = c('Never smoked during pregnancy', 'Smoked until pregnancy was known', 'Continued smoking in pregnancy'))
full_df_short2$`Household smoking at child age 2 months` <- factor(full_df_short2$`Household smoking at child age 2 months` , levels = c('no', 'yes'), labels = c('No', 'Yes'))
full_df_short2$`Household smoking at child age 6 months` <- factor(full_df_short2$`Household smoking at child age 6 months` , levels = c('no', 'yes'), labels = c('No', 'Yes'))
full_df_short2$`Household smoking at child age 6 years` <- factor(full_df_short2$`Household smoking at child age 6 years` , levels = c('no', 'yes'), labels = c('No', 'Yes'))
full_df_short2$`DMFT index at child age 6 years` <- factor(full_df_short2$`DMFT index at child age 6 years` , levels = c('no', 'yes'), labels = c('Caries free', 'Caries'))
full_df_short2$`DMFT index at child age 14 years` <- factor(full_df_short2$`DMFT index at child age 14 years` , levels = c('no', 'yes'), labels = c('Caries free', 'Caries'))

full_df_short3 <- subset(full_df_short2, include == 1)

cat_vars <- full_df_short3 %>%
  select('Maternal smoking during pregnancy',  'Household smoking at child age 2 months', 'Household smoking at child age 6 months', 'Household smoking at child age 6 years', 'Household smoking at child age 10 years', 'Household smoking at child age 14 years',  'DMFT index at child age 6 years',  'DMFT index at child age 14 years') %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c(
    "Maternal smoking during pregnancy",
    "Household smoking at child age 2 months",
    "Household smoking at child age 6 months",
    "Household smoking at child age 6 years",
    "Household smoking at child age 10 years",
    "Household smoking at child age 14 years",
    "DMFT index at child age 6 years",
    "DMFT index at child age 14 years"
  ))) %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(perc = n / sum(n) * 100,
         type = "categorical")

# Create prevalence plot
ggplot(filter(cat_vars, !is.na(value)),
       aes(x = value, y = perc, fill = variable)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free") +
  scale_fill_brewer(palette = "GnBu") +
  labs(
    x = "Category",
    y = "Prevalence (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "royalblue4", color = "black"),
    strip.text = element_text(face = "bold", color = 'white'),
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

## Forest plot of logistic regression model results for F5 and F13
# Create df f5
forest_plot_df_f5 <- data.frame(
  Exposure = c('Maternal smoking during pregnancy', 'Household smoking at child age 2 months', 'Household smoking at child age 6 months', 'Household smoking at child age 6 years', 'Household smoking at child age 10 years', 'Household smoking at child age 14 years'),
  OR = c(1.81, 1.83, 1.42, 1.21, NA, NA),
  CI_lower = c(1.18, 1.18, 0.97, 0.73, NA, NA),
  CI_upper = c(2.76, 3.14, 2.09, 1.99, NA, NA)
)

forest_plot_df_f5$Outcome <- 'Dental caries at child age 6 years'

# Create df f13
forest_plot_df_f13 <- data.frame(
  Exposure = c('Maternal smoking during pregnancy', 'Household smoking at child age 2 months', 'Household smoking at child age 6 months', 'Household smoking at child age 6 years', 'Household smoking at child age 10 years', 'Household smoking at child age 14 years'),
  OR = c(0.79, 1.19, 0.78, 1.03, 0.97, 1.23),
  CI_lower = c(0.48, 0.69, 0.51, 0.59, 0.62, 0.73),
  CI_upper = c(1.31, 2.06, 1.19, 1.79, 1.52, 2.07)
)

forest_plot_df_f13$Outcome <- 'Dental caries at child age 14 years'

# Combine dfs 
df_forest_plot <- rbind(forest_plot_df_f5, forest_plot_df_f13)

# Set order
df_forest_plot$Exposure <- factor(df_forest_plot$Exposure,
                                  levels = rev(c(
                                    'Maternal smoking during pregnancy',
                                    'Household smoking at child age 2 months',
                                    'Household smoking at child age 6 months',
                                    'Household smoking at child age 6 years',
                                    'Household smoking at child age 10 years',
                                    'Household smoking at child age 14 years'
                                  )))

# Create forest plot 
ggplot(df_forest_plot, aes(x = OR, y = Exposure, color = Outcome)) +
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), 
                 height = 0.2, position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Dental caries at child age 6 years" = "#1B9E77",
                                "Dental caries at child age 14 years" = "#0072B2")) +  
  labs(x = "Odds ratio (OR)", y = "Exposure") +
  theme_bw(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

## Longitudinal plot
# Select single df
df_figure_long <- complete(reshape_imp.data_long, 30)
df_figure_long$timepoint <- as.factor(df_figure_long$timepoint)
df_figure_long$timepoint <- as.numeric(df_figure_long$timepoint)

# Specify model
figure_glmer_f13 <- glmer(dmft_13y ~ smoking+timepoint + AGE_M_v2 + brushing_13y + dental_visit_13y + m_alcohol_use_pregnancy + GESTBIR + GENDER + orthodontie_13y + m_education + household_income + ethnicity_child +  breastfeeding + sugar_snacks_combined_13y + (1 | IDC), family = binomial, data = df_figure_long)

# Create longitudinal plot (in line with results: no change over time)
plot_model(figure_glmer_f13, type = "pred", terms = c("timepoint", "smoking")) +
  labs(
    x = "Timepoint",
    y = "Probability of dmft occurrence",  
    title = ""
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

#\ End of script 
