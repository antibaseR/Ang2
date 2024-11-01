library(tidyverse)

# load the data with AKI flag and the previously merged data
Molinari_AKI_flag = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/AKI_Flag_Molinari_Data.csv")
Molinari_MAKE_flag = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/MAKE30_Flag_Molinari_Data.csv")
merged_data = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/merged_data.csv")
# merge the AKI flag into the data
data = merge(merge(merged_data, Molinari_AKI_flag, by = c("PatientID")), Molinari_MAKE_flag, by = c("PatientID"))
# create a AKI Category variable
data = data %>%
  mutate(AKI_Category = ifelse(Persistent_Severe_AKI == 1, 2,
                               ifelse(Transient_AKI == 1, 1, 0)))

data = data %>%
  arrange(PatientID, hour)
write.csv(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/Ang2_Complete_Data.csv", row.names = F)

library(haven)
write_dta(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/Ang2_Complete_Data.dta")
