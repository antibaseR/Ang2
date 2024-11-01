library(tidyverse)
library(readxl)

VMI_biomarker_raw = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/mass spec analysis 090617 combo.xlsx")
VMI_fulldata = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/ProCESS-full dataset-Gomez.xlsx")

# remove the healthy volunteers for the statistical analysis but keep the patients in case
healthy_vulunteers_data = VMI_biomarker_raw[!(VMI_biomarker_raw$PatientID %in% VMI_fulldata$stnum),]

# Count the number of occurrences for each combination of PatientID and hour
duplicate_counts = VMI_biomarker_raw %>%
  group_by(PatientID, hour) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Filter for combinations that have more than one occurrence (duplicates)
duplicates = duplicate_counts %>%
  filter(count > 1)

# subset the data to only the duplicates
duplicate_data = merge(duplicates, VMI_biomarker_raw, all.x = T)
duplicate_remove = duplicate_data %>%
  filter(is.na(vesselLength))

VMI_biomarker_raw_no_dup = anti_join(VMI_biomarker_raw, duplicate_remove)
write.csv(VMI_biomarker_raw_no_dup, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/VMI_biomarker.csv", row.names = F)

# mass spec as the mass dataset
## include pAKI, mortality and other outcomes
VMI_renal_variable = c("Stnum",
                       "Platelet_Count0",
                       "Platelet_Count.6",
                       "Platelet_Count.24",
                       "Platelet_Count72",
                       "Glucose0",
                       "Glucose.6",
                       "Glucose.24",
                       "Glucose.48",
                       "Glucose.72",
                       "IL10Val0",
                       "IL10Val6",
                       "IL10Val24",
                       "IL10Val72",
                       "ref_Creatinine",
                       "APACHEIII_baseline",
                       "Charlson",
                       "death_30days",
                       "death_90days",
                       "death", 
                       "day_alive", 
                       "day_alive_cens800", 
                       "RRT_30days",
                       # "TIMP2_0",
                       # "IGFBP7_0",
                       # "KIM1Val0",
                       # "KIM1Val6",
                       # "KIM1Val24",
                       # "KIM1Val72",
                       # "NGALVal0",
                       # "NGALVal6",
                       # "NGALVal24",
                       # "NGALVal72",
                       "Age_At_Enrollment",
                       "Sex_Scr",
                       "Inr0",
                       "INR.6",
                       "INR.24",
                       "Inr72",
                       "Hepatic",
                       "Vasopressin_1St_24Hrs_Yn",
                       "BASELINE__SOFA_Cardiac",
                       "Fluids_Sum_1St24Hrs_Ivvol",
                       "DCF_renad")

VMI_renal_time_variant_variable = c("PatientID",
                                    "Platelet_Count0",
                                    "Platelet_Count6",
                                    "Platelet_Count24",
                                    "Platelet_Count72",
                                    "Glucose0",
                                    "Glucose6",
                                    "Glucose24",
                                    "Glucose48",
                                    "Glucose72",
                                    "IL10Val0",
                                    "IL10Val6",
                                    "IL10Val24",
                                    "IL10Val72",
                                    # "KIM1Val0",
                                    # "KIM1Val6",
                                    # "KIM1Val24",
                                    # "KIM1Val72",
                                    # "NGALVal0",
                                    # "NGALVal6",
                                    # "NGALVal24",
                                    # "NGALVal72",
                                    "INR0",
                                    "INR6",
                                    "INR24",
                                    "INR72")

VMI_renal_time_invariant_variable = c("PatientID", 
                                      "APACHEIII_baseline",
                                      "Charlson",
                                      "ref_Creatinine",
                                      "death_30days",
                                      "death_90days",
                                      "death", 
                                      "day_alive", 
                                      "day_alive_cens800", 
                                      "RRT_30days",
                                      # "TIMP2_0",
                                      # "IGFBP7_0",
                                      "Age_At_Enrollment",
                                      "Sex_Scr",
                                      "Hepatic",
                                      "Vasopressin_1St_24Hrs_Yn",
                                      "BASELINE__SOFA_Cardiac",
                                      "Fluids_Sum_1St24Hrs_Ivvol",
                                      "DCF_renad")

VMI_biomarker_variable = c("PatientID",
                           "hour",
                           # "total_HS",
                           # "PBR5_25",
                           # "PBR5_9",
                           # "PBR10_19",
                           # "PBR20_25",
                           # "MfiSmall_n",
                           # "HeteroIndexSmall",
                           # "SummaryTVD_Small",
                           # "SummaryPPV_Small",
                           # "SummaryPVD_Small",
                           # "DeBackerScore",
                           "Ang_2Val",
                           "ICAMVal",
                           "VCAMVal",
                           "E_SelectinVal",
                           "P_SelectinVal",
                           "IL6Val",
                           "TNFVal",
                           "VEGFVal",
                           "vWFVal",
                           "CRPVal",
                           # "Rolling",
                           # "Adhered",
                           "X__24_hours_Total_Fluids",
                           "D_DimerVal")

VMI_renal = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/SepsisAKI_Molinari_02112021.xlsx") %>%
  select(all_of(VMI_renal_variable))
VMI_biomarker = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/VMI_biomarker.csv") %>%
  select(all_of(VMI_biomarker_variable))
VMI_fulldata = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/Final_Complete_ProCESS_Dataset_03062019.xlsx") %>%
  select(Stnum_all, Enrdte)

# rename columns
names(VMI_renal)[names(VMI_renal) == "Platelet_Count.6"] = "Platelet_Count6"
names(VMI_renal)[names(VMI_renal) == "Platelet_Count.24"] = "Platelet_Count24"
names(VMI_renal)[names(VMI_renal) == "Glucose.6"] = "Glucose6"
names(VMI_renal)[names(VMI_renal) == "Glucose.24"] = "Glucose24"
names(VMI_renal)[names(VMI_renal) == "Glucose.48"] = "Glucose48"
names(VMI_renal)[names(VMI_renal) == "Glucose.72"] = "Glucose72"
names(VMI_renal)[names(VMI_renal) == "Inr0"] = "INR0"
names(VMI_renal)[names(VMI_renal) == "INR.6"] = "INR6"
names(VMI_renal)[names(VMI_renal) == "INR.24"] = "INR24"
names(VMI_renal)[names(VMI_renal) == "Inr72"] = "INR72"
names(VMI_fulldata)[names(VMI_fulldata) == "Stnum_all"] = c("PatientID")
names(VMI_renal)[names(VMI_renal) == "Stnum"] = "PatientID"

# arrange data
VMI_renal = VMI_renal %>% arrange(PatientID)
VMI_biomarker = VMI_biomarker %>% arrange(PatientID, hour)

# convert wide to long format
VMI_renal_wide = VMI_renal %>%
  select(all_of(VMI_renal_time_variant_variable))

VMI_renal_long = pivot_longer(VMI_renal_wide, 
                              cols = -PatientID,  # All columns except 'id'
                              names_to = c(".value", "hour"),  # Split into 'variable' and 'time'
                              names_pattern = "(.*)(0|6|24|72)$") %>%  # Capture the variable name and the time points
  mutate(hour = as.numeric(hour))

VMI_renal_rest = VMI_renal %>%
  select(all_of(VMI_renal_time_invariant_variable))

data = merge(merge(merge(VMI_biomarker, VMI_renal_long, by = c("PatientID", "hour")), VMI_renal_rest, by = "PatientID"), VMI_fulldata, by = "PatientID") %>%
  select(PatientID, hour, Enrdte, everything()) %>%
  arrange(PatientID, hour)

data = merge(merge(merge(VMI_biomarker, VMI_renal_rest, by = c("PatientID")), VMI_fulldata, by = c("PatientID")), VMI_renal_long, by = c("PatientID", "hour"))

write.csv(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/merged_data.csv", row.names = F)


# entire ProCESS data (either Molinari data or the final complete ProCESS)
## descriptive and missingness and the method of imputation




