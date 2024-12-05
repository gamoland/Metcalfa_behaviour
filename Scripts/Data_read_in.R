library(tidyverse)
library(tidyr)
library(xlsx)


Metcalfa_behavior_data <- read.xlsx("Data/Metcalfa_nymph_Y_olf_final results.xlsx", sheetIndex = 1) %>% 
  filter(!(J_ID %in% c( "J9", "J10", "J11", "J14"))) %>% 
  mutate(Choosed_the_compound = ifelse(Result == "blank" | Result == "L", 
                                       "No", 
                                       ifelse(Result == "U", 
                                              "U", 
                                              ifelse(Result == "N",
                                                     "N",
                                                     "Yes"))),
         Test = ifelse(VC2 == "blank",
                paste(VC2, VC1),
                paste(VC1, VC2)),
         VC1 = ifelse(substr(DATE, 1, 4) == "2024" & VC1 == "blank",
                      VC2.side,
                      VC1),
         VC2 = ifelse(substr(DATE, 1, 4) == "2024" & VC2 != "blank",
                      VC1.side,
                      VC2),
         Result = as.factor(Result),
         VC2 = as.factor(VC2),
         VC1 = as.factor(VC1),
         Year = as.factor(str_sub(DATE, 1,4)),
         Test = as.factor(Test))

Metcalfa_behavior_data_without_nonstarters <- Metcalfa_behavior_data %>% 
  filter(Result != "N")
  

