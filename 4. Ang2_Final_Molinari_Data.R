library(tidyverse)
library(readxl)

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
                       "Ang_2Val0",  
                       "Ang_2Val6",  
                       "Ang_2Val24", 
                       "Ang_2Val72",
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
                       "D_DimerVal0",
                       "D_DimerVal6",
                       "D_DimerVal24",
                       "D_DimerVal72",
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
                                    "Ang_2Val0",  
                                    "Ang_2Val6",  
                                    "Ang_2Val24", 
                                    "Ang_2Val72",
                                    "INR0",
                                    "INR6",
                                    "INR24",
                                    "INR72",
                                    "D_DimerVal0",
                                    "D_DimerVal6",
                                    "D_DimerVal24",
                                    "D_DimerVal72")

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

# load the data with AKI flag and the previously merged data
VMI_renal = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/VMI/SepsisAKI_Molinari_02112021.xlsx") %>%
  select(all_of(VMI_renal_variable))
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
names(VMI_renal)[names(VMI_renal) == "Stnum"] = "PatientID"

# arrange data
VMI_renal = VMI_renal %>% arrange(PatientID)
Molinari_AKI_flag = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/AKI_Flag_Molinari_Data.csv")
Molinari_MAKE_flag = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/MAKE30_Flag_Molinari_Data.csv")

# merge the AKI flag into the data
data = merge(merge(VMI_renal, Molinari_AKI_flag, by = c("PatientID")), Molinari_MAKE_flag, by = c("PatientID"))
# create a AKI Category variable
data = data %>%
  mutate(AKI_Category = ifelse(Persistent_Severe_AKI == 1, 2,
                               ifelse(Transient_AKI == 1, 1, 0)))

data = data %>%
  arrange(PatientID)
write.csv(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/Ang2_Complete_Molinari_Data.csv", row.names = F)

library(haven)
write_dta(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/Ang2_Complete_Molinari_Data.dta")
