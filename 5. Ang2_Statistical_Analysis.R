library(tidyverse)
library(survival)
library(patchwork)
library(survminer)


data <- read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Data/Ang2_Complete_Molinari_Data.csv")

quantile(data$Ang_2Val0, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)

data$Ang_2Val0_gr = ifelse(data$Ang_2Val0 <= quantile(data$Ang_2Val0, probs = c(0.25), na.rm = T), 1,
                           ifelse(data$Ang_2Val0 > quantile(data$Ang_2Val0, probs = c(0.25), na.rm = T) & data$Ang_2Val0 <= quantile(data$Ang_2Val0, probs = c(0.5), na.rm = T), 2,
                                  ifelse(data$Ang_2Val0 > quantile(data$Ang_2Val0, probs = c(0.5), na.rm = T) & data$Ang_2Val0 <= quantile(data$Ang_2Val0, probs = c(0.75), na.rm = T), 3, 4)))


km_data = data %>%
  select(PatientID, FIRST_AKI_DATE, AKI, Ang_2Val0_gr, day_alive, death_30days) %>%
  mutate(time = ifelse(AKI==0, day_alive, FIRST_AKI_DATE)) %>%
  mutate(time = ifelse(is.na(time), 28, time)) %>%
  mutate(time = ifelse(time>28, 28, time)) %>%
  filter(!is.na(Ang_2Val0_gr))

km_fit <- survfit(Surv(time, AKI) ~ Ang_2Val0_gr, data = km_data)

grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

p = km_fit %>%
  ggsurvplot(
    data = km_data,
    fun = "pct",
    #linetype = "strata", # Change line type by groups
    pval = TRUE,
    conf.int = TRUE,
    risk.table = TRUE,
    fontsize = 4, # used in risk table
    #censor.shape = 3,       # Adjust symbol, e.g., 3 is a +
    censor.size = 8,  
    font.x = c(16),  # Font size, style, and color for x-axis
    font.y = c(16),   # Font size, style, and color for y-axis
    font.title = c(16), # Font settings for plot title
    font.legend = c(14),
    #surv.median.line = "hv", # median horizontal and vertical ref lines
    ggtheme = theme_light(),
    palette = c("goldenrod", "sienna", "tomato", "firebrick"),
    title = "Kaplan-Meier Survival Function Estimate",
    legend.title = "",
    legend.labs = c("Ang2 (Min - Q1)", "Ang2 (Q1 - Q2)", "Ang2 (Q2 - Q3)", "Ang2 (Q3 - Max)"),
  )

ggsave("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/Ang2/Result/KM_Curve.png", p, width = 10, height = 8)


