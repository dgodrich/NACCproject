###CDR and variable indexing 04/06/2020 ... no MMSE (add in)
library(tidyverse)
library(readxl)
library(dummies)
library(nnet)
library(neuralnet)
library(RSNNS)
library(deepnet)

setwd("/Users/doonoogoodrooch/Documents/lab/code")
data <- read_excel("NACC_NP_data_dump_2019July.xlsx")
data <- data %>% filter(!is.na(FORMVER))  ## must have a UDS present
data <- data %>% filter(NPFORMVER %in% c(9,10) )   ## neuropath form version 9 or 10

multivisit <- filter(data, data$NACCNVST != 1) #filter for only multi-visit patients
ones <- filter(multivisit, multivisit$NACCVNUM == 1) #filter first, or baseline, visits
lasts <- filter(multivisit, multivisit$NACCVNUM == multivisit$NACCNVST) #filter last visits

##########################################
######## Finding characteristics #########
##########################################
##########################################

##age at baseline exam
table(ones$NACCAGE, useNA = "always") #look at the data
summary(ones$NACCAGE) #get mean
sd(ones$NACCAGE) #get sd

##age at final exam
table(lasts$NACCAGE, useNA = "always") #look at the data
summary(lasts$NACCAGE) #get mean
sd(lasts$NACCAGE) #get sd

##education
table(ones$EDUC, useNA = "always") #look at the data
  #code missing data as NA (type: 99)
  educDF <- ones
  educDF$EDUC[educDF$EDUC == 99] <- NA
  table(educDF$EDUC, useNA = 'always')

summary(educDF$EDUC) #get mean
sd(educDF$EDUC, na.rm = T) #get sd

##age at death 
ageatdeath <- c()
for (i in 1:nrow(ones)){
  ageatdeath[i] <- ones$NACCYOD[i] - ones$BIRTHYR[i]
}
summary(ageatdeath)
sd(ageatdeath)

##years last exam --> death
deathyear <- lasts$NACCYOD 
lastvisit <- lasts$VISITYR
diff <- c()
for (i in 1:length(lastvisit)){
  diff[i] <- deathyear[i]-lastvisit[i]
}
summary(diff)
sd(diff)

##########################################
#########Cog Impairment Scaling###########
#########Assigning 0, +, ++ ##############
##########################################

##CDR at baseline
table(ones$CDRSUM, useNA = 'always')
summary(ones$CDRSUM)
sd(ones$CDRSUM)

##CDR at final
table(lasts$CDRSUM, useNA = 'always')
summary(lasts$CDRSUM)
sd(lasts$CDRSUM)

##baseline CDR cog impairment classifications
tot <- nrow(ones)
baseCDR_plusplus <- ones[which(ones$CDRSUM > 12),]
nrow(baseCDR_plusplus)
nrow(baseCDR_plusplus)/tot
baseCDR_plus <- ones[which(ones$CDRSUM > 6 & ones$CDRSUM <= 12),]
nrow(baseCDR_plus)
nrow(baseCDR_plus)/tot
baseCDR_zero <- ones[which(ones$CDRSUM <= 6),]
nrow(baseCDR_zero)
nrow(baseCDR_zero)/tot

##final CDR cog impairment classifications
tot <- nrow(lasts)
finalCDR_plusplus <- lasts[which(lasts$CDRSUM > 12),]
nrow(finalCDR_plusplus)
nrow(finalCDR_plusplus)/tot
finalCDR_plus <- lasts[which(lasts$CDRSUM > 6 & lasts$CDRSUM <= 12),]
nrow(finalCDR_plus)
nrow(finalCDR_plus)/tot
finalCDR_zero <- lasts[which(lasts$CDRSUM <= 6),]
nrow(finalCDR_zero)
nrow(finalCDR_zero)/tot

##########################################
######### Neuropath Indexing #############
######### Assigning 0, 0.40, 1 ###########
##########################################

####
#could potentially make a function for this part, but will revisit if needed
###

#######NACCBRAA
table(lasts$NACCBRAA, useNA = "always") #look at the data
  #code missing data as NA (type: 99)
  braak <- lasts
  braak$NACCBRAA[braak$NACCBRAA == 7 | braak$NACCBRAA == 8 | braak$NACCBRAA == 9] <- NA
  table(braak$NACCBRAA, useNA = 'always')

###############index 1 
braak_sev <- length(which(braak$NACCBRAA >= 5))
braak_sev
braak_sev/tot
braak_set1 <- braak[which(braak$NACCBRAA >= 5),]
  ##plusplus
  plusplus_braak_sev <- length(which(braak_set1$CDRSUM > 12))
  plusplus_braak_sev
  plusplus_braak_sev/braak_sev
  
  pp_braak_index1 <- braak_set1[which(braak_set1$CDRSUM > 12),]
 
  ##plus
  plus_braak_sev <- length(which(braak_set1$CDRSUM > 6 & braak_set1$CDRSUM <= 12))
  plus_braak_sev
  plus_braak_sev/braak_sev
  
  p_braak_index0.4 <- braak_set1[which(braak_set1$CDRSUM > 6 & braak_set1$CDRSUM <= 12),]
  
  ##zero
  zero_braak_sev <- length(which(braak_set1$CDRSUM <= 6))
  zero_braak_sev
  zero_braak_sev/braak_sev

###############index 0.40 
braak_mod <- length(which(braak$NACCBRAA == 3 | braak$NACCBRAA == 4))
braak_mod
braak_mod/tot
braak_set2 <- braak[which(braak$NACCBRAA == 3 | braak$NACCBRAA == 4),]
  ##plusplus
  plusplus_braak_mod <- length(which(braak_set2$CDRSUM > 12))
  plusplus_braak_mod
  plusplus_braak_mod/braak_mod
  
  pp_braak_index1 <- braak_set1[which(braak_set2$CDRSUM > 12),]
  
  ##plus
  plus_braak_mod <- length(which(braak_set2$CDRSUM > 6 & braak_set2$CDRSUM <= 12))
  plus_braak_mod
  plus_braak_mod/braak_mod
  
  p_braak_index0.4 <- braak_set1[which(braak_set2$CDRSUM > 6 & braak_set2$CDRSUM <= 12),]
  
  ##zero
  zero_braak_mod <- length(which(braak_set2$CDRSUM <= 6))
  zero_braak_mod
  zero_braak_mod/braak_mod
  
###############index 0
braak_mild <- length(which(braak$NACCBRAA < 3))
braak_mild
braak_mild/tot
braak_set3 <- braak[which(braak$NACCBRAA < 3),]
  ##plusplus
  plusplus_braak_mild <- length(which(braak_set3$CDRSUM > 12))
  plusplus_braak_mild
  plusplus_braak_mild/braak_mild
  
  pp_braak_index1 <- braak_set1[which(braak_set3$CDRSUM > 12),]
  
  ##plus
  plus_braak_mild <- length(which(braak_set3$CDRSUM > 6 & braak_set3$CDRSUM <= 12))
  plus_braak_mild
  plus_braak_mild/braak_mild
  
  p_braak_index0.4 <- braak_set1[which(braak_set3$CDRSUM > 6 & braak_set3$CDRSUM <= 12),]
  
  ##zero
  zero_braak_mild <- length(which(braak_set3$CDRSUM <= 6))
  zero_braak_mild
  zero_braak_mild/braak_mild

  

#######NACCNEUR
table(lasts$NACCNEUR, useNA = "always") #look at the data
#code missing data as NA (type: 99)
neur <- lasts
neur$NACCNEUR[neur$NACCNEUR == 8 | neur$NACCNEUR == 9] <- NA
table(neur$NACCNEUR, useNA = 'always')

###############index 1 
neur_sev <- length(which(neur$NACCNEUR == 3))
neur_sev
neur_sev/tot
neur_set1 <- neur[which(neur$NACCNEUR == 3),]
    ##plusplus
    plusplus_neur_sev <- length(which(neur_set1$CDRSUM > 12))
    plusplus_neur_sev
    plusplus_neur_sev/neur_sev
    
    pp_neur_index1 <- neur_set1[which(neur_set1$CDRSUM > 12),]
    
    ##plus
    plus_neur_sev <- length(which(neur_set1$CDRSUM > 6 & neur_set1$CDRSUM <= 12))
    plus_neur_sev
    plus_neur_sev/neur_sev
    
    p_neur_index0.4 <- neur_set1[which(neur_set1$CDRSUM > 6 & neur_set1$CDRSUM <= 12),]
    
    ##zero
    zero_neur_sev <- length(which(neur_set1$CDRSUM <= 6))
    zero_neur_sev
    zero_neur_sev/neur_sev

###############index 0.40 
neur_mod <- length(which(neur$NACCNEUR == 2))
neur_mod
neur_mod/tot
neur_set2 <- neur[which(neur$NACCNEUR == 2),]
    ##plusplus
    plusplus_neur_mod <- length(which(neur_set2$CDRSUM > 12))
    plusplus_neur_mod
    plusplus_neur_mod/neur_mod
    
    pp_neur_index1 <- neur_set1[which(neur_set2$CDRSUM > 12),]
    
    ##plus
    plus_neur_mod <- length(which(neur_set2$CDRSUM > 6 & neur_set2$CDRSUM <= 12))
    plus_neur_mod
    plus_neur_mod/neur_mod
    
    p_neur_index0.4 <- neur_set1[which(neur_set2$CDRSUM > 6 & neur_set2$CDRSUM <= 12),]
    
    ##zero
    zero_neur_mod <- length(which(neur_set2$CDRSUM <= 6))
    zero_neur_mod
    zero_neur_mod/neur_mod

###############index 0
neur_mild <- length(which(neur$NACCNEUR < 2))
neur_mild
neur_mild/tot
neur_set3 <- neur[which(neur$NACCNEUR < 2),]
    ##plusplus
    plusplus_neur_mild <- length(which(neur_set3$CDRSUM > 12))
    plusplus_neur_mild
    plusplus_neur_mild/neur_mild
    
    pp_neur_index1 <- neur_set1[which(neur_set3$CDRSUM > 12),]
    
    ##plus
    plus_neur_mild <- length(which(neur_set3$CDRSUM > 6 & neur_set3$CDRSUM <= 12))
    plus_neur_mild
    plus_neur_mild/neur_mild
    
    p_neur_index0.4 <- neur_set1[which(neur_set3$CDRSUM > 6 & neur_set3$CDRSUM <= 12),]
    
    ##zero
    zero_neur_mild <- length(which(neur_set3$CDRSUM <= 6))
    zero_neur_mild
    zero_neur_mild/neur_mild


#######NACCDIFF
table(lasts$NACCDIFF, useNA = "always") #look at the data
#code missing data as NA
diff <- lasts
diff$NACCDIFF[diff$NACCDIFF == 8 | diff$NACCDIFF == 9] <- NA
table(diff$NACCDIFF, useNA = 'always')

###############index 1 
diff_sev <- length(which(diff$NACCDIFF == 3))
diff_sev
diff_sev/tot
diff_set1 <- diff[which(diff$NACCDIFF == 3),]
    ##plusplus
    plusplus_diff_sev <- length(which(diff_set1$CDRSUM > 12))
    plusplus_diff_sev
    plusplus_diff_sev/diff_sev
    
    pp_diff_index1 <- diff_set1[which(diff_set1$CDRSUM > 12),]
    
    ##plus
    plus_diff_sev <- length(which(diff_set1$CDRSUM > 6 & diff_set1$CDRSUM <= 12))
    plus_diff_sev
    plus_diff_sev/diff_sev
    
    p_diff_index0.4 <- diff_set1[which(diff_set1$CDRSUM > 6 & diff_set1$CDRSUM <= 12),]
    
    ##zero
    zero_diff_sev <- length(which(diff_set1$CDRSUM <= 6))
    zero_diff_sev
    zero_diff_sev/diff_sev

###############index 0.40 
diff_mod <- length(which(diff$NACCDIFF == 2))
diff_mod
diff_mod/tot
diff_set2 <- diff[which(diff$NACCDIFF == 2),]
    ##plusplus
    plusplus_diff_mod <- length(which(diff_set2$CDRSUM > 12))
    plusplus_diff_mod
    plusplus_diff_mod/diff_mod
    
    pp_diff_index1 <- diff_set1[which(diff_set2$CDRSUM > 12),]
    
    ##plus
    plus_diff_mod <- length(which(diff_set2$CDRSUM > 6 & diff_set2$CDRSUM <= 12))
    plus_diff_mod
    plus_diff_mod/diff_mod
    
    p_diff_index0.4 <- diff_set1[which(diff_set2$CDRSUM > 6 & diff_set2$CDRSUM <= 12),]
    
    ##zero
    zero_diff_mod <- length(which(diff_set2$CDRSUM <= 6))
    zero_diff_mod
    zero_diff_mod/diff_mod

###############index 0
diff_mild <- length(which(diff$NACCDIFF < 2))
diff_mild
diff_mild/tot
diff_set3 <- diff[which(diff$NACCDIFF < 2),]
    ##plusplus
    plusplus_diff_mild <- length(which(diff_set3$CDRSUM > 12))
    plusplus_diff_mild
    plusplus_diff_mild/diff_mild
    
    pp_diff_index1 <- diff_set1[which(diff_set3$CDRSUM > 12),]
    
    ##plus
    plus_diff_mild <- length(which(diff_set3$CDRSUM > 6 & diff_set3$CDRSUM <= 12))
    plus_diff_mild
    plus_diff_mild/diff_mild
    
    p_diff_index0.4 <- diff_set1[which(diff_set3$CDRSUM > 6 & diff_set3$CDRSUM <= 12),]
    
    ##zero
    zero_diff_mild <- length(which(diff_set3$CDRSUM <= 6))
    zero_diff_mild
    zero_diff_mild/diff_mild


    #######NACCNEUR
    table(lasts$NACCNEUR, useNA = "always") #look at the data
    #code missing data as NA (type: 99)
    neur <- lasts
    neur$NACCNEUR[neur$NACCNEUR == 8 | neur$NACCNEUR == 9] <- NA
    table(neur$NACCNEUR, useNA = 'always')
    
    ###############index 1 
    neur_sev <- length(which(neur$NACCNEUR == 3))
    neur_sev
    neur_sev/tot
    neur_set1 <- neur[which(neur$NACCNEUR == 3),]
    ##plusplus
    plusplus_neur_sev <- length(which(neur_set1$CDRSUM > 12))
    plusplus_neur_sev
    plusplus_neur_sev/neur_sev
    
    pp_neur_index1 <- neur_set1[which(neur_set1$CDRSUM > 12),]
    
    ##plus
    plus_neur_sev <- length(which(neur_set1$CDRSUM > 6 & neur_set1$CDRSUM <= 12))
    plus_neur_sev
    plus_neur_sev/neur_sev
    
    p_neur_index0.4 <- neur_set1[which(neur_set1$CDRSUM > 6 & neur_set1$CDRSUM <= 12),]
    
    ##zero
    zero_neur_sev <- length(which(neur_set1$CDRSUM <= 6))
    zero_neur_sev
    zero_neur_sev/neur_sev
    
    ###############index 0.40 
    neur_mod <- length(which(neur$NACCNEUR == 2))
    neur_mod
    neur_mod/tot
    neur_set2 <- neur[which(neur$NACCNEUR == 2),]
    ##plusplus
    plusplus_neur_mod <- length(which(neur_set2$CDRSUM > 12))
    plusplus_neur_mod
    plusplus_neur_mod/neur_mod
    
    pp_neur_index1 <- neur_set1[which(neur_set2$CDRSUM > 12),]
    
    ##plus
    plus_neur_mod <- length(which(neur_set2$CDRSUM > 6 & neur_set2$CDRSUM <= 12))
    plus_neur_mod
    plus_neur_mod/neur_mod
    
    p_neur_index0.4 <- neur_set1[which(neur_set2$CDRSUM > 6 & neur_set2$CDRSUM <= 12),]
    
    ##zero
    zero_neur_mod <- length(which(neur_set2$CDRSUM <= 6))
    zero_neur_mod
    zero_neur_mod/neur_mod
    
    ###############index 0
    neur_mild <- length(which(neur$NACCNEUR < 2))
    neur_mild
    neur_mild/tot
    neur_set3 <- neur[which(neur$NACCNEUR < 2),]
    ##plusplus
    plusplus_neur_mild <- length(which(neur_set3$CDRSUM > 12))
    plusplus_neur_mild
    plusplus_neur_mild/neur_mild
    
    pp_neur_index1 <- neur_set1[which(neur_set3$CDRSUM > 12),]
    
    ##plus
    plus_neur_mild <- length(which(neur_set3$CDRSUM > 6 & neur_set3$CDRSUM <= 12))
    plus_neur_mild
    plus_neur_mild/neur_mild
    
    p_neur_index0.4 <- neur_set1[which(neur_set3$CDRSUM > 6 & neur_set3$CDRSUM <= 12),]
    
    ##zero
    zero_neur_mild <- length(which(neur_set3$CDRSUM <= 6))
    zero_neur_mild
    zero_neur_mild/neur_mild
    
    
#######NACCLEWY
table(lasts$NACCLEWY, useNA = "always") #look at the data
#code missing data as NA
lewy <- lasts
lewy$NACCLEWY[lewy$NACCLEWY == 8 | lewy$NACCLEWY == 9 | lewy$NACCLEWY == 4] <- NA
table(lewy$NACCLEWY, useNA = 'always')

###############index 1 
lewy_sev <- length(which(lewy$NACCLEWY == 1))
lewy_sev
lewy_sev/tot
lewy_set1 <- lewy[which(lewy$NACCLEWY == 1),]
    ##plusplus
    plusplus_lewy_sev <- length(which(lewy_set1$CDRSUM > 12))
    plusplus_lewy_sev
    plusplus_lewy_sev/lewy_sev
    
    pp_lewy_index1 <- lewy_set1[which(lewy_set1$CDRSUM > 12),]
    
    ##plus
    plus_lewy_sev <- length(which(lewy_set1$CDRSUM > 6 & lewy_set1$CDRSUM <= 12))
    plus_lewy_sev
    plus_lewy_sev/lewy_sev
    
    p_lewy_index0.4 <- lewy_set1[which(lewy_set1$CDRSUM > 6 & lewy_set1$CDRSUM <= 12),]
    
    ##zero
    zero_lewy_sev <- length(which(lewy_set1$CDRSUM <= 6))
    zero_lewy_sev
    zero_lewy_sev/lewy_sev

###############index 0.40 
lewy_mod <- length(which(lewy$NACCLEWY == 2 |lewy$NACCLEWY == 3))
lewy_mod
lewy_mod/tot
lewy_set2 <- lewy[which(lewy$NACCLEWY == 2 |lewy$NACCLEWY == 3),]
    ##plusplus
    plusplus_lewy_mod <- length(which(lewy_set2$CDRSUM > 12))
    plusplus_lewy_mod
    plusplus_lewy_mod/lewy_mod
    
    pp_lewy_index1 <- lewy_set1[which(lewy_set2$CDRSUM > 12),]
    
    ##plus
    plus_lewy_mod <- length(which(lewy_set2$CDRSUM > 6 & lewy_set2$CDRSUM <= 12))
    plus_lewy_mod
    plus_lewy_mod/lewy_mod
    
    p_lewy_index0.4 <- lewy_set1[which(lewy_set2$CDRSUM > 6 & lewy_set2$CDRSUM <= 12),]
    
    ##zero
    zero_lewy_mod <- length(which(lewy_set2$CDRSUM <= 6))
    zero_lewy_mod
    zero_lewy_mod/lewy_mod

###############index 0
lewy_mild <- length(which(lewy$NACCLEWY == 0))
lewy_mild
lewy_mild/tot
lewy_set3 <- lewy[which(lewy$NACCLEWY == 0),]
    ##plusplus
    plusplus_lewy_mild <- length(which(lewy_set3$CDRSUM > 12))
    plusplus_lewy_mild
    plusplus_lewy_mild/lewy_mild
    
    pp_lewy_index1 <- lewy_set1[which(lewy_set3$CDRSUM > 12),]
    
    ##plus
    plus_lewy_mild <- length(which(lewy_set3$CDRSUM > 6 & lewy_set3$CDRSUM <= 12))
    plus_lewy_mild
    plus_lewy_mild/lewy_mild
    
    p_lewy_index0.4 <- lewy_set1[which(lewy_set3$CDRSUM > 6 & lewy_set3$CDRSUM <= 12),]
    
    ##zero
    zero_lewy_mild <- length(which(lewy_set3$CDRSUM <= 6))
    zero_lewy_mild
    zero_lewy_mild/lewy_mild
    

#######NACCAMY
table(lasts$NACCAMY, useNA = "always") #look at the data
#code missing data as NA
amy <- lasts
amy$NACCAMY[amy$NACCAMY == 8 | amy$NACCAMY == 9] <- NA
table(amy$NACCAMY, useNA = 'always')
    
###############index 1 
amy_sev <- length(which(amy$NACCAMY == 3))
amy_sev
amy_sev/tot
amy_set1 <- amy[which(amy$NACCAMY == 3),]
    ##plusplus
    plusplus_amy_sev <- length(which(amy_set1$CDRSUM > 12))
    plusplus_amy_sev
    plusplus_amy_sev/amy_sev
    
    pp_amy_index1 <- amy_set1[which(amy_set1$CDRSUM > 12),]
    
    ##plus
    plus_amy_sev <- length(which(amy_set1$CDRSUM > 6 & amy_set1$CDRSUM <= 12))
    plus_amy_sev
    plus_amy_sev/amy_sev
    
    p_amy_index0.4 <- amy_set1[which(amy_set1$CDRSUM > 6 & amy_set1$CDRSUM <= 12),]
    
    ##zero
    zero_amy_sev <- length(which(amy_set1$CDRSUM <= 6))
    zero_amy_sev
    zero_amy_sev/amy_sev
    
###############index 0.40 
amy_mod <- length(which(amy$NACCAMY == 2))
amy_mod
amy_mod/tot
amy_set2 <- amy[which(amy$NACCAMY == 2),]
    ##plusplus
    plusplus_amy_mod <- length(which(amy_set2$CDRSUM > 12))
    plusplus_amy_mod
    plusplus_amy_mod/amy_mod
    
    pp_amy_index1 <- amy_set1[which(amy_set2$CDRSUM > 12),]
    
    ##plus
    plus_amy_mod <- length(which(amy_set2$CDRSUM > 6 & amy_set2$CDRSUM <= 12))
    plus_amy_mod
    plus_amy_mod/amy_mod
    
    p_amy_index0.4 <- amy_set1[which(amy_set2$CDRSUM > 6 & amy_set2$CDRSUM <= 12),]
    
    ##zero
    zero_amy_mod <- length(which(amy_set2$CDRSUM <= 6))
    zero_amy_mod
    zero_amy_mod/amy_mod
    
###############index 0
amy_mild <- length(which(amy$NACCAMY == 0 | amy$NACCAMY == 1))
amy_mild
amy_mild/tot
amy_set3 <- amy[which(amy$NACCAMY == 0 | amy$NACCAMY == 1),]
    ##plusplus
    plusplus_amy_mild <- length(which(amy_set3$CDRSUM > 12))
    plusplus_amy_mild
    plusplus_amy_mild/amy_mild
    
    pp_amy_index1 <- amy_set1[which(amy_set3$CDRSUM > 12),]
    
    ##plus
    plus_amy_mild <- length(which(amy_set3$CDRSUM > 6 & amy_set3$CDRSUM <= 12))
    plus_amy_mild
    plus_amy_mild/amy_mild
    
    p_amy_index0.4 <- amy_set1[which(amy_set3$CDRSUM > 6 & amy_set3$CDRSUM <= 12),]
    
    ##zero
    zero_amy_mild <- length(which(amy_set3$CDRSUM <= 6))
    zero_amy_mild
    zero_amy_mild/amy_mild
    

#######NACCARTE
table(lasts$NACCARTE, useNA = "always") #look at the data
#code missing data as NA
arte <- lasts
arte$NACCARTE[arte$NACCARTE == 8 | arte$NACCARTE == 9] <- NA
table(arte$NACCARTE, useNA = 'always')
    ###############index 1 
    arte_sev <- length(which(arte$NACCARTE == 3))
    arte_sev
    arte_sev/tot
    arte_set1 <- arte[which(arte$NACCARTE == 3),]
    ##plusplus
    plusplus_arte_sev <- length(which(arte_set1$CDRSUM > 12))
    plusplus_arte_sev
    plusplus_arte_sev/arte_sev
    
    pp_arte_index1 <- arte_set1[which(arte_set1$CDRSUM > 12),]
    
    ##plus
    plus_arte_sev <- length(which(arte_set1$CDRSUM > 6 & arte_set1$CDRSUM <= 12))
    plus_arte_sev
    plus_arte_sev/arte_sev
    
    p_arte_index0.4 <- arte_set1[which(arte_set1$CDRSUM > 6 & arte_set1$CDRSUM <= 12),]
    
    ##zero
    zero_arte_sev <- length(which(arte_set1$CDRSUM <= 6))
    zero_arte_sev
    zero_arte_sev/arte_sev
    
###############index 0.40 
arte_mod <- length(which(arte$NACCARTE == 2))
arte_mod
arte_mod/tot
arte_set2 <- arte[which(arte$NACCARTE == 2),]
    ##plusplus
    plusplus_arte_mod <- length(which(arte_set2$CDRSUM > 12))
    plusplus_arte_mod
    plusplus_arte_mod/arte_mod
    
    pp_arte_index1 <- arte_set1[which(arte_set2$CDRSUM > 12),]
    
    ##plus
    plus_arte_mod <- length(which(arte_set2$CDRSUM > 6 & arte_set2$CDRSUM <= 12))
    plus_arte_mod
    plus_arte_mod/arte_mod
    
    p_arte_index0.4 <- arte_set1[which(arte_set2$CDRSUM > 6 & arte_set2$CDRSUM <= 12),]
    
    ##zero
    zero_arte_mod <- length(which(arte_set2$CDRSUM <= 6))
    zero_arte_mod
    zero_arte_mod/arte_mod
    
###############index 0
arte_mild <- length(which(arte$NACCARTE == 0 | arte$NACCARTE == 1))
arte_mild
arte_mild/tot
arte_set3 <- arte[which(arte$NACCARTE == 0 | arte$NACCARTE == 1),]
    ##plusplus
    plusplus_arte_mild <- length(which(arte_set3$CDRSUM > 12))
    plusplus_arte_mild
    plusplus_arte_mild/arte_mild
    
    pp_arte_index1 <- arte_set1[which(arte_set3$CDRSUM > 12),]
    
    ##plus
    plus_arte_mild <- length(which(arte_set3$CDRSUM > 6 & arte_set3$CDRSUM <= 12))
    plus_arte_mild
    plus_arte_mild/arte_mild
    
    p_arte_index0.4 <- arte_set1[which(arte_set3$CDRSUM > 6 & arte_set3$CDRSUM <= 12),]
    
    ##zero
    zero_arte_mild <- length(which(arte_set3$CDRSUM <= 6))
    zero_arte_mild
    zero_arte_mild/arte_mild

    
#######NPHIPSCL AND NPSCL
table(lasts$NPHIPSCL, useNA = "always") #look at the data
table(lasts$NPSCL, useNA = "always") #look at the data
#code missing data as NA
hip <- lasts
hip$NPSCL[hip$NPSCL== -4 | hip$NPSCL == 3 | hip$NPSCL == 9] <- NA
hip$NPHIPSCL[hip$NPHIPSCL== -4 | hip$NPHIPSCL == 8 | hip$NPHIPSCL == 9] <- NA

table(hip$NPSCL, useNA = 'always')
table(hip$NPHIPSCL, useNA = 'always')

###############index 1
hip_sev <- length(which(hip$NPHIPSCL == 2))
hip_sev
hip_sev/tot
hip_set1 <- hip[which(hip$NPHIPSCL == 2),]
    ##plusplus
    plusplus_hip_sev <- length(which(hip_set1$CDRSUM > 12))
    plusplus_hip_sev
    plusplus_hip_sev/hip_sev
    
    pp_hip_index1 <- hip_set1[which(hip_set1$CDRSUM > 12),]
    
    ##plus
    plus_hip_sev <- length(which(hip_set1$CDRSUM > 6 & hip_set1$CDRSUM <= 12))
    plus_hip_sev
    plus_hip_sev/hip_sev
    
    p_hip_index0.4 <- hip_set1[which(hip_set1$CDRSUM > 6 & hip_set1$CDRSUM <= 12),]
    
    ##zero
    zero_hip_sev <- length(which(hip_set1$CDRSUM <= 6))
    zero_hip_sev
    zero_hip_sev/hip_sev

###############index 0.40 
hip_mod <- length(which(hip$NPHIPSCL == 1 | hip$NPHIPSCL == 3 | hip$NPSCL == 1) )
hip_mod
hip_mod/tot
hip_set2 <- hip[which(hip$NPHIPSCL == 1 | hip$NPHIPSCL == 3 | hip$NPSCL == 1),]
    ##plusplus
    plusplus_hip_mod <- length(which(hip_set2$CDRSUM > 12))
    plusplus_hip_mod
    plusplus_hip_mod/hip_mod
    
    pp_hip_index1 <- hip_set2[which(hip_set2$CDRSUM > 12),]
    
    ##plus
    plus_hip_mod <- length(which(hip_set2$CDRSUM > 6 & hip_set2$CDRSUM <= 12))
    plus_hip_mod
    plus_hip_mod/hip_mod
    
    p_hip_index0.4 <- hip_set2[which(hip_set2$CDRSUM > 6 & hip_set2$CDRSUM <= 12),]
    
    ##zero
    zero_hip_mod <- length(which(hip_set2$CDRSUM <= 6))
    zero_hip_mod
    zero_hip_mod/hip_mod

###############index 0
hip_mild <- length(which(hip$NPHIPSCL == 0 | hip$NPHIPSCL == 1 | hip$NPSCL == 2 ))
hip_mild
hip_mild/tot
hip_set3 <- hip[which(hip$NPHIPSCL == 0 | hip$NPHIPSCL == 1 | hip$NPSCL == 2 ),]
    ##plusplus
    plusplus_hip_mild <- length(which(hip_set3$CDRSUM > 12))
    plusplus_hip_mild
    plusplus_hip_mild/hip_mild
    
    pp_hip_index1 <- hip_set3[which(hip_set3$CDRSUM > 12),]
    
    ##plus
    plus_hip_mild <- length(which(hip_set3$CDRSUM > 6 & hip_set3$CDRSUM <= 12))
    plus_hip_mild
    plus_hip_mild/hip_mild
    
    p_hip_index0.4 <- hip_set3[which(hip_set3$CDRSUM > 6 & hip_set3$CDRSUM <= 12),]
    
    ##zero
    zero_hip_mild <- length(which(hip_set3$CDRSUM <= 6))
    zero_hip_mild
    zero_hip_mild/hip_mild
    
####NPART and NPWMR => WMR
table(lasts$NPWMR, useNA = "always")
table(lasts$NPART, useNA = "always")

wmr <- lasts
wmr$NPART[wmr$NPART== -4 | wmr$NPART == 3 | wmr$NPART == 9] <- NA
wmr$NPWMR[wmr$NPWMR== -4 | wmr$NPWMR == 8 | wmr$NPWMR == 9] <- NA
table(wmr$NPART, useNA = "always")
table(wmr$NPWMR, useNA = "always")
    
    ###############index 1
    wmr_sev <- length(which(wmr$NPWMR == 1 | wmr$NPWMR == 2 | wmr$NPWMR == 3 | wmr$NPART == 1))
    wmr_sev
    wmr_sev/tot
    wmr_set1 <- wmr[which(wmr$NPWMR == 1 | wmr$NPWMR == 2 | wmr$NPWMR == 3 | wmr$NPART == 1),]
    
    ###############index 0
    wmr_mild <- length(which(wmr$NPWMR == 0 | wmr$NPART == 2))
    wmr_mild
    wmr_mild/tot
    wmr_set3 <- wmr[which(wmr$NPWMR == 0 | wmr$NPART == 2),]
    
    ############### missing
    wmr_miss <- length(which(is.na(wmr$NPART) & is.na(wmr$NPWMR))) 
    wmr_miss
    wmr_miss/tot


  
####NACCHEM 
table(lasts$NACCHEM, useNA = "always")

hem <- lasts
hem$NACCHEM[hem$NACCHEM == 8 | hem$NACCHEM == 9] <- NA
table(hem$NACCHEM, useNA = "always")
    
    ###############index 1
    hem_sev <- length(which(hem$NACCHEM == 1))
    hem_sev
    hem_sev/tot
    hem_set1 <- hem[which(hem$NACCHEM == 1),]
    
    ###############index 0
    hem_mild <- length(which(hem$NACCHEM == 0))
    hem_mild
    hem_mild/tot
    hem_set3 <- hem[which(hem$NACCHEM == 0),]
    
    ############### missing
    hem_miss <- length(which(is.na(hem$NACCHEM))) 
    hem_miss
    hem_miss/tot  
  
####NACCINF
table(lasts$NACCINF, useNA = "always")

inf <- lasts
inf$NACCINF[inf$NACCINF == 8 | inf$NACCINF == 9] <- NA
table(inf$NACCINF, useNA = "always")

    ###############index 1
    inf_sev <- length(which(inf$NACCINF == 1))
    inf_sev
    inf_sev/tot
    inf_set1 <- inf[which(inf$NACCINF == 1),]
    
    ###############index 0
    inf_mild <- length(which(inf$NACCINF == 0))
    inf_mild
    inf_mild/tot
    inf_set3 <- inf[which(inf$NACCINF == 0),]
    
    ############### missing
    inf_miss <- length(which(is.na(inf$NACCINF))) 
    inf_miss
    inf_miss/tot  
  
####NACCMICR
table(lasts$NACCMICR, useNA = "always")

micr <- lasts
micr$NACCMICR[micr$NACCMICR == 8 | micr$NACCMICR == 9] <- NA
table(micr$NACCMICR, useNA = "always")

    ###############index 1
    micr_sev <- length(which(micr$NACCMICR == 1))
    micr_sev
    micr_sev/tot
    micr_set1 <- micr[which(micr$NACCMICR == 1),]
    
    ###############index 0
    micr_mild <- length(which(micr$NACCMICR == 0))
    micr_mild
    micr_mild/tot
    micr_set3 <- micr[which(micr$NACCMICR == 0),]
    
    ############### missing
    micr_miss <- length(which(is.na(micr$NACCMICR))) 
    micr_miss
    micr_miss/tot   
  

###################################################
################ NORMALIZING DATA ################
##################################################
Zscore_first <- c()
Zscore_last <- c()

#missing data ## This part needs editing because ones are NOT accounted for
lasts$NACCMMSE[lasts$NACCMMSE == -4 |lasts$NACCMMSE == 88 |  lasts$NACCMMSE == 95 |
                 lasts$NACCMMSE == 96 | lasts$NACCMMSE == 97 | lasts$NACCMMSE == 98] <- NA
ones$NACCMMSE[ones$NACCMMSE == -4 |ones$NACCMMSE == 88 |  ones$NACCMMSE == 95 |
                ones$NACCMMSE == 96 | ones$NACCMMSE == 97 | ones$NACCMMSE == 98] <- NA
miss1 <- which(is.na(lasts$NACCMMSE))
miss2 <- which(is.na(ones$NACCMMSE))

lasts <- lasts[-miss1,]
ones <- ones[-miss2,]

for (i in 1:nrow(lasts)){
  Zscore_last[i] <- (lasts$NACCMMSE[i]-(28.40921584 + (-0.48003363*lasts$SEX[i]) + (-0.02015242*lasts$NACCAGE[i]) 
                                        + (0.14331069*lasts$EDUC[i])))/(1.239254)
}

for (i in 1:nrow(ones)){
Zscore_first[i] <- (ones$NACCMMSE[i]-(28.40921584 + (-0.48003363*ones$SEX[i]) + (-0.02015242*ones$NACCAGE[i]) 
                                      + (0.14331069*ones$EDUC[i])))/(1.239254)
}

###TRIAL FOR NORMALITY OF SCORE / NP PHENOTYPE 
getQQ <- function(set1,set2,set3){
    a <- which(lasts$NACCID %in% as.matrix(set1[,1]))
    b <- which(lasts$NACCID %in% as.matrix(set2[,1]))
    c <- which(lasts$NACCID %in% as.matrix(set3[,1]))
    
    index <- c(rep(0,nrow(lasts)))
    for (i in 1:nrow(lasts)){
      if(i %in% a){index[i] <- 1} else{}
      if(i %in% b){index[i] <- 2} else{}
      if(i %in% c){index[i] <- 3} else{}
    }
    
    newDF <- data.frame(lasts$NACCID, Zscore_last, index)
    names(newDF) <- c("ID","Zscore","index")
    
    sevHist <- newDF
    modHist <- newDF
    mildHist <- newDF
    
    sevHist <- sevHist %>% filter(newDF$index == 3)
    modHist <- modHist %>% filter(newDF$index == 2)
    mildHist <- mildHist %>% filter(newDF$index == 1)
    
    library(ggpubr)
    pltsev <- ggqqplot(sevHist$Zscore, color = "red")
    pltmod <- ggqqplot(modHist$Zscore, color = "yellow")
    pltmild <- ggqqplot(mildHist$Zscore, color = "green")
    return (ggarrange(pltsev,pltmod,pltmild, nrow = 3, common.legend = T))
}

PLTbraak <- getQQ(braak_set1, braak_set2, braak_set3)
PLTneur <- getQQ(neur_set1, neur_set2, neur_set3)
PLTdiff <- getQQ(diff_set1, diff_set2, diff_set3)
PLTlewy <- getQQ(lewy_set1, lewy_set2, lewy_set3)
PLTamy <- getQQ(amy_set1, amy_set2, amy_set3)
PLTarte <- getQQ(arte_set1, arte_set2, arte_set3)
PLThip <- getQQ(hip_set1, hip_set2, hip_set3)

###################################################
#####FINDING INDIVS W/O ANY MISSING DATA #########
##################################################

counter <- c(rep(0,3850))
for (i in 1:nrow(lasts)){
  if (lasts$NACCBRAA[i] != 7 & lasts$NACCBRAA[i] != 8 & lasts$NACCBRAA[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCLEWY[i] != 4 & lasts$NACCLEWY[i] != 8 & lasts$NACCLEWY[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCNEUR[i] != 8 & lasts$NACCNEUR[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCDIFF[i] != 8 & lasts$NACCDIFF[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCARTE[i] != 8 & lasts$NACCARTE[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCAMY[i] != 8 & lasts$NACCAMY[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCHEM[i] != 8 & lasts$NACCHEM[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCINF[i] != 8 & lasts$NACCINF[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NACCMICR[i] != 8 & lasts$NACCMICR[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NPART[i] != -4 & lasts$NPART[i] != 3 & lasts$NPART[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NPWMR[i] != -4 & lasts$NPWMR[i] != 8 & lasts$NPWMR[i] != 9){
    counter[i] <- counter[i] + 1 
  }  else {}
  
  if (lasts$NPSCL[i] != -4 & lasts$NPSCL[i] != 3 & lasts$NPSCL[i] != 9){
    counter[i] <- counter[i] + 1 
  } 
  
  else if (lasts$NPHIPSCL[i] != -4 & lasts$NPHIPSCL[i] != 8 & lasts$NPHIPSCL[i] != 9){
    counter[i] <- counter[i] + 1 
  }  
  
}

#nonMissing <- lasts[which(counter == 7),] # n = 2,965, WITHOUT vascular variables
nonMissing <- lasts[which(counter >= 11),] # n = 824, WITH vascular variables


comp <- c(rep(0,2965))

for (i in 1:nrow(nonMissing)){
  #for a severe score, plus 1 per category match to composite score
  if (as.character(nonMissing[i,1]) %in% as.matrix(braak_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(neur_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(diff_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(lewy_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(amy_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(arte_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(hip_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(wmr_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(hem_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(inf_set1[,1])){comp[i] <- comp[i]+1} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(micr_set1[,1])){comp[i] <- comp[i]+1} else{}
  
  #for a moderate score, plus 0.40 per category match to composite score
    #no vascular variables here -- they are binary
  if (as.character(nonMissing[i,1]) %in% as.matrix(braak_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(neur_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(diff_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(lewy_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(amy_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(arte_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  if (as.character(nonMissing[i,1]) %in% as.matrix(hip_set2[,1])){comp[i] <- comp[i]+0.4} else{}
  
  #for mild/none score, no need to add zero
}

compTable <- table(comp)
write.csv(compTable, file = "compTab", quote = F)

plot(table(comp)) #quickly look at data
ggqqplot(comp) #qq plot of data 

# look @ distribution of composite scores
hist(comp, breaks = 20, xlab = "composite score", col = "grey", 
     main = c("Composite NP Index Scores\nn = 824"))
    
    
    
#interesting cols 
pheno <- nonMissing %>% select(NACCID, NACCBRAA, NACCNEUR, NACCDIFF, NACCLEWY, NACCAMY, NACCARTE, NPHIPSCL,
                         NPSCL,NACCINF, NACCMICR, NACCHEM, NPART, NPWMR)




