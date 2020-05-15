### Dana Godrich
## Indexing NP vars and creating clin vars
# Created 05/12/2020
# Last updated 05/15/2020

library(tidyverse)
library(readxl)
library(dummies)
library(dyplyr)
rm(list=ls())

data <- read_excel("NACC_NP_data_dump_2019July.xlsx")
data <- data %>% filter(!is.na(FORMVER))  ## must have a UDS present
data <- data %>% filter(NPFORMVER %in% c(9,10) )   ## neuropath form version 9 or 10

multivisit <- filter(data, data$NACCNVST != 1) #filter for only multi-visit patients
ones <- filter(multivisit, multivisit$NACCVNUM == 1) #filter first, or baseline, visits
lasts <- filter(multivisit, multivisit$NACCVNUM == multivisit$NACCNVST) #filter last visits

## manipulating data -- assigning index values of 0, 0.40, 1, or NA
df2 <- lasts %>%
         select(NACCBRAA, NACCNEUR, NACCDIFF, NACCAMY, NACCARTE, NACCLEWY, NACCMICR, 
                NACCINF, NACCHEM, NPART, NPWMR, NPHIPSCL, NPSCL, SEX, NACCYOD) %>% 
         mutate_at(vars(NACCNEUR, NACCDIFF, NACCAMY, NACCARTE, NACCLEWY, NACCBRAA,
                        NACCINF, NACCHEM, NACCMICR, NPWMR, NPHIPSCL, NPART, NPSCL), ~ifelse(. == 8 | . == 9, NA, .)) %>% #missing
         mutate_at(vars(NACCNEUR, NACCDIFF, NACCAMY, NACCARTE, NACCBRAA), ~ifelse(. == 0 | . == 1, 0, .)) %>% #mild
         mutate_at(vars(NACCNEUR, NACCDIFF, NACCAMY, NACCARTE), ~ifelse(. == 2, 0.40, .)) %>% #moderate 
         mutate_at(vars(NACCNEUR, NACCDIFF, NACCAMY, NACCARTE), ~ifelse(. == 3, 1, .)) %>% #severe
         mutate_at(vars(NACCINF, NACCHEM, NACCMICR, NACCLEWY, NPART, NPSCL, NPWMR, NPHIPSCL), ~ifelse(. == 1, 1, .)) %>% #severe
         mutate_at(vars(NACCINF, NACCHEM, NACCMICR, NACCLEWY, NPWMR, NPHIPSCL), ~ifelse(. == 0, 0, .)) %>% #mild
         mutate_at(vars(NACCBRAA), ~ifelse(. == 7, NA, .)) %>% #missing
         mutate_at(vars(NACCBRAA), ~ifelse(. == 5 | . == 6 , 1, .)) %>% #severe 
         mutate_at(vars(NACCBRAA), ~ifelse(. == 3 | . == 4 , 0.40, .)) %>% #moderate 
         mutate_at(vars(NACCBRAA, NPSCL, NPART), ~ifelse(. == 2, 0, .)) %>% #mild 
         mutate_at(vars(NACCLEWY), ~ifelse(. == 2 | . == 3, 0.40, .)) %>% #moderate 
         mutate_at(vars(NACCLEWY), ~ifelse(. == 4, NA, .)) %>% #missing 
         mutate_at(vars(NPART, NPSCL, NPWMR, NPHIPSCL), ~ifelse(. == -4, NA, .)) %>% #missing 
         mutate_at(vars(NPART, NPSCL), ~ifelse(. == 3, NA, .)) %>% #missing
         mutate_at(vars(NPWMR, NPHIPSCL), ~ifelse(. == 2 | . == 3, 1, .)) %>% #severe
         unite("WMR", NPART:NPWMR, sep = "", na.rm = TRUE, remove = TRUE) %>% #unite
         unite("HIPSCL", NPHIPSCL:NPSCL, sep = "", na.rm = TRUE, remove = TRUE)  %>% #unite
         mutate_at(vars(WMR, HIPSCL), ~ifelse(. == '0NA' | . == 'NA0' | . == '00', 0, .)) %>% #absent
         mutate_at(vars(WMR, HIPSCL), ~ifelse(. == '1NA' | . == 'NA1', 1, .)) %>% #present
         mutate_at(vars(WMR, HIPSCL), ~ifelse(. == 'NANA', NA, .)) %>% #missing
         mutate_at(vars(WMR, HIPSCL), ~ifelse(. == 1 | . == 0 , as.double(.), .)) #convert to numeric
  

impdf <- mice(df2, method = "pmm", m = 5, printFlag = F, seed = 123)
compdf <- complete(impdf, 1)     

## add a composite score column at the end 
compScores <- compdf %>% 
                mutate(composite = select(., 1:11) %>% rowSums())


## plot a hist of complete data (compdf)
plt <- ggplot(data = compScores, aes(composite)) + 
  geom_histogram(binwidth = 0.50, col = "black", fill = "lightgrey", alpha = 0.75) +
  labs(x="composite score", y="Number of Individuals", title = "Distribution of Composite Scores",
       subtitle = "n = 3,850") +
  stat_bin(aes(y=..count.., label=..count..), binwidth = 0.50, geom="text", vjust=-1) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  ylim(0,400) +
  theme_light() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.50),
        axis.title.y = element_text(size = 14, margin = margin(r = 10)),
        axis.title.x = element_text(size = 14, margin = margin(r = 20), vjust = -0.5),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12))
plt


### Creating clinical vars 

# MMSE -- 1. (raw_mmse_final - raw_mmse_first) / (time_final - time_first) ... n = 2,931
mDat_ones <- ones %>% select(NACCID, NACCDAYS, SEX, EDUC, NACCAGEB, NACCMMSE) %>%
                  mutate_at(vars(NACCMMSE), ~ifelse(. == -4 | . == 88 |. == 95 |. == 96 |. == 97 | . == 98, NA, .))
mDat_lasts <- lasts %>% select(NACCID, NACCAGE, NACCDAYS, NACCMMSE) %>% 
                  mutate_at(vars(NACCMMSE), ~ifelse(. == -4 | . == 88 |. == 95 |. == 96 |. == 97 | . == 98, NA, .))
mmse_raw <- cbind(mDat_ones, mDat_lasts$NACCAGE, mDat_lasts$NACCMMSE)
names(mmse_raw) <- c("id","days", "sex", "educ", "age_first","mmse_first", "age_last","mmse_last")
mmse_raw <- mmse_raw %>%
  mutate(mmsediff = mmse_first - mmse_last) %>% #change in mmse raw score
  mutate(time = round(days / 365.25, 3)) %>% #change in time
  mutate(rawchange = mmsediff / time) #change in mmse raw over time ... outcome column 1 (some missing data here)

# MMSE -- 2. (z_mmse_final - z_mmse_first) / (time_final - time_first) ... n = 2,931
mmse_norm <- mmse_raw %>%
  mutate(normed1 = mmse_first-28.40921584 + (-0.48003363*sex) + (-0.02015242*age_first) + (0.14331069*educ)/(1.239254)) %>%
  mutate(normed2 = mmse_last-28.40921584 + (-0.48003363*sex) + (-0.02015242*age_last) + (0.14331069*educ)/(1.239254)) %>%
  mutate(normdiff = normed1 - normed2) %>%
  mutate(normchange = normdiff / time) # change in normed mmse over time... outcome column 2 (some missing data here)

# CDR -- 1. (cdr_final - cdr_first) / (time_final - time_first) ... n = 3,850
cDat_ones <- ones %>% select(NACCID, NACCDAYS, CDRSUM) 
cDat_lasts <- lasts %>% select(NACCID, NACCDAYS, CDRSUM)
cdr <- cbind(cDat_ones, cDat_lasts$CDRSUM)
names(cdr) <- c("id","days","cdr_first","cdr_last")
cdr <- cdr %>%
        mutate(cdrdiff = cdr_last - cdr_first) %>% #change in cdr score
        mutate(time = round(days / 365.25, 3)) %>% #change in time
        mutate(change = cdrdiff / time) #change in cdr over time ... outcome column 3 (no missing data here)
 
# AOO -- 1. age of onset for AD only ... n = 2,263 
aoo <- lasts %>% 
          select(NACCID, NACCETPR, DECAGE) %>%
          filter(NACCETPR == 1) %>% # n alz = 2,317
          mutate_at(vars(DECAGE), ~ifelse(. == 888 | . == 999, NA, .)) # n missing = 54

hist(aoo$DECAGE, main = "aoo for ad patients", xlab = "aoo", labels = T, ylim = c(0,450)) #look at data

  