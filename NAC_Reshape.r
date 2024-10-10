library(readxl);library(tidyverse);library(fs);library(tidyverse);library(lubridate);library(gtsummary);library(psych);
library(ggplot2);library(showtext);library(dplyr);library(tidyr);library(gtsummary);library(webshot);library(magrittr);
library(tableone); library(openxlsx);library(survival);library(lubridate);library(ggsurvfit);
library(tidycmprsk);library(condSURV);library(survminer);library(gt);library(anytime);
library(webshot2);library(knitr); library(Hmisc);
library(writexl)

#Note: cross-sectional EZ-measurements;
# | Subject ID | Eye | EZ Measurement (micrometers) |
# |------------|-----|-----------------------------|
# | 1          | OD  | 250                         |
# | 1          | OS  | 240                         |
# | 2          | OD  | 235                         |
# | 2          | OS  | 230                         |
# | 3          | OD  | 260                         |
# | 3          | OS  | 255                         |
# | ...        | ... | ...                         |


##################################################################################################
#######  DATA DOWNLOAOD       ####################################################################
# Redcap My Reports & Exports Selected instruments and/or events (all records), 
# Select one or more instruments/events below for all records.
# Choose export format CSV / R Statistical Software THEN click export data;
# Click icon(s) to download: Both R and DATA CSV;

#Instructions: Run all of the R code from line 1 to line 92 for data pre-process; 
################################################################################################
# Set the working directory to the "~/Downloads" folder.

# Source the R script file named "NACAttackPrime_R_2023-08-16_1046.r".
# This will execute the R commands within the sourced file in the current R session.
#source("NACAttackPrime_R_2023-08-23_1523.r") # Screening ONLY
# R script file already Setting Factors for most of variables(created new variable for factors),
# it converts certain columns in the data dataframe into categorical factors with predefined levels. 
#Factors are used to represent categories. For instance, it creates factors for event names, instruments, data access groups, language, gender, and race. 
#The levels assigned to each factor represent different categories within those attributes, such as event types or language options.
today <- Sys.Date()
##################################################################################################
#######  Define Functions   ####################################################################
#########################################################################################

# several records with “89_test_clinic” as their redcap_data_access_group. Whenever you download data, the first thing to do is filter out these records. They are test patients and are not real.

# Define a function that filters data based on an event name and assigns event name as the dataframe name;
filter_data <- function(data,event_name) {
  # Filter the data based on event_name
  filtered_data <- data %>%
    filter(`redcap_event_name.factor` == event_name, !redcap_data_access_group %in% c("89_test_clinic"))
  # Assign the filtered data to a variable with the provided event_name in the global environment
  assign(event_name, filtered_data, envir = .GlobalEnv)
}


# Define a function that shape race columns and user can input a new data name; 
race_reshape <- function(new_data_name = new_data_name) {
  filtered_data <- data %>%
    dplyr::select(patient_id, ends_with(".factor")) %>%
    filter(`redcap_event_name.factor` == "Screening")
  
  race_data <- filtered_data %>%
    dplyr::select(patient_id, starts_with("d_race")) %>% 
    gather(key = "variable", value = "count", starts_with("d_race___")) %>% 
    filter(count != "Unchecked" & count != "") %>% 
    mutate(Race = recode(as.character(variable),
                         "d_race___1.factor" = "American Indian or Alaska Native",
                         "d_race___2.factor" = "Asian",
                         "d_race___3.factor" = "Black or African American",
                         "d_race___4.factor" = "Hispanic or Latino",
                         "d_race___5.factor" = "Native Hawaiian or Other Pacific Islander",
                         "d_race___6.factor" = "White",
                         "d_race___7.factor" = "Other",
                         "d_race____99.factor" = "Unknown/Not Reported/Refused")) %>% 
    dplyr::select(patient_id, Race) %>% 
    arrange(patient_id)
  
  duplicate_patient_ids <- race_data$patient_id[duplicated(race_data$patient_id)]
  duplicate_rows <- subset(race_data, patient_id %in% duplicate_patient_ids)
  
  recoded_race <- duplicate_rows  %>%
    group_by(patient_id) %>%
    summarise(Race = paste(Race, collapse = "; "))
  
  unique_rows <- subset(race_data, !patient_id %in% duplicate_patient_ids)
  race_clean <- rbind(unique_rows, recoded_race)
  #for empty values replace with the first value and merge back with the original data, now original_data_with_new_race contains only one race columns with all multiple choice combined 
  new_data <- data %>% 
    full_join(race_clean, by = "patient_id") %>% 
    dplyr::select(-starts_with("d_race")) %>%
    mutate(
      Age = ave(d_age, cumsum(!is.na(d_age)), FUN = function(x) x[1]),
      Sex = ave(d_sex.factor, cumsum(!is.na(d_sex.factor)), FUN = function(x) x[1])
    ) %>% 
    filter(!redcap_data_access_group %in% c("89_test_clinic"))
  
  #fix the lable to indicate left and right eye; the sconce R script file did not list the difference for Left or Right. 
  label(new_data$va_sphere_od) = "Sphere OD"
  label(new_data$va_sphere_os) = "Sphere OS"
  # Assign the modified data to the new_data_name variable
  assign(new_data_name, new_data, envir = .GlobalEnv)
}




TableSummary <- function(variables, strata, data) {
  continuous_vars <- c("Age", "va_sphere_od", "va_sphere_os", "va_etdrs_od", "va_etdrs_os", "mps_sens_od", "mps_sens_os", "oct_cst_od", "oct_cst_os","month_to_expire")
  type_args <- setNames(rep("continuous", length(continuous_vars)), continuous_vars)
  table_summary <- data %>%
    dplyr::select(all_of(variables)) %>%
    tbl_summary(by = strata,
                statistic = list(all_continuous() ~ "{mean} ({sd}),{min}~{max}"),
                type = type_args) %>%
    # add_p(test = everything() ~ "kruskal.test") %>%
    add_overall() %>%
    bold_labels()
  return(table_summary)
}


calculate_correlation_with_ci <- function(x, y) {
  # Calculate the correlation coefficient and 95% CI
  correlation_test <- cor.test(x, y, method = "pearson",use = "pairwise.complete.obs")
  # Extract correlation coefficient and CI bounds
  correlation <- correlation_test$estimate
  lower_bound <- correlation_test$conf.int[1]
  upper_bound <- correlation_test$conf.int[2]
  # Print the results
  cat("Correlation Coefficient:", correlation, "\n")
  cat("95% Confidence Interval - Lower Bound:", lower_bound, "\n")
  cat("95% Confidence Interval - Upper Bound:", upper_bound, "\n")
  # Return the results as a data frame 
  result <- tibble(
    correlation = correlation,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
  return(result)
}




general_AAC_calculation <- function(data, t) {
  aac_values <- numeric(nrow(data))  # Initialize a vector to store AAC values for each row
  for (row_index in 1:nrow(data)) {
    y <- unlist(data[row_index, ])  # Extract a list of row values
    k <- length(y)  # k is the total visits
    aac <- 0  # Initialize aac to zero
    y0 <- y[[1]]  # First value in the y
    y1 <- y[[2]]  # Second value in the y
    #if only one visit, we use this equation;
    if (k == 2) {
      aac <- 0.5 * abs(y0 - y1) * t * sign(y0 - y1) #only one visit;
    } else if (k > 2) { # more than one follow up visit,check if it is Case 1 or Case 2 then calculate aac for different case condition;
      for (i in 3:k) {
        if (any((y0 - y[(i - 1)]) * (y0 - y[i]) > 0)) {
          # Case 1: a trapezoid
          area_trap <- 1/2 * (abs(y0 - y[(i - 1)]) + abs(y0 - y[i])) * t * sign(y0 - y[i])
          aac <- aac + area_trap  # Add the area for loss to aac
        } else {
          # Case 2: not a trapezoid, one area gain 
          area_gain <- t/2 * ((abs(y0 - y[(i - 1)])^2 * sign(y0 - y[(i - 1)]))) / ((abs(y0 - y[(i - 1)]) + abs(y0 - y[i]))) +  
            t/2 * ((abs(y0 - y[i])^2 * sign(y0 - y[i])) )/ ((abs(y0 - y[(i - 1)]) + abs(y0 - y[i])))
          aac <- aac + area_gain  # Add the area for gain to aac
        }
      }
    }
    aac_values[row_index] <- aac  # Store AAC value for the current row
  }
  return(aac_values)
}

#### email from Kurt on 1/29/2024,
# A patient can only withdraw if they are randomized. If they are never randomized in this trial, there is nothing for them to withdraw from. 4502 is a screening failure… I realize this nomenclature is a bit misleading because they became ineligible at the baseline visit, but in summary, they are not eligible to be randomized. There are many steps in this complicated process where a patient may become ineligible. These are the following fields you need to look at:
# 
# Screening:
# en_site_preeligible en_site_eligible er_eligible ec_eligible (assuming that ec_staffid2 is not null)

# Baseline:
# rd_pi rd_rz_accept rd_assignment
# 
# When looking for patient withdrawal, you should look at the fields in NAC58, specifically [ex_reason] = ‘4’. Note that this “visit” is called “M45/Early Termination” because it is the last visit a patient will have. In most cases this will be during the M45 time window, but if the patient withdrew/died/disappeared, it may be on other dates.

# Note: use 'rd_pi_no'   notes to identify withdrawal patients bases on the meeting discussion on 1/25/2024.  But Kurt sent me email TO USE : 
# based on Kurt email on 1/29/2024 listed above, we selected the targeted variables 

# var<- c("en_site_preeligible.factor","en_site_eligible.factor","er_eligible.factor","ec_eligible.factor","ec_staffid2",
#         "rd_pi.factor","rd_rz_accept.factor","rd_assignment.factor")
