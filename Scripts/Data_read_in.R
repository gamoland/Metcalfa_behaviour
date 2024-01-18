library(tidyverse)
library(tidyr)
library(xlsx)


Metcalfa_behavior_data <- read.xlsx("Data/final_results.xlsx", sheetIndex = 1) %>% 
  filter(Result != "N",
         !(J_ID %in% c( "J9", "J10", "J11", "J14"))) %>% 
  mutate(Choosed_the_compound = ifelse(Result == "Blank" | Result == "L", 
                                       "No", 
                                       ifelse(Result == "U", 
                                              "U", 
                                              "Yes")),
         Test = as.factor(paste(VC1, VC2)),
         Result = as.factor(Result))

