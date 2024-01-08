library(tidyverse)
library(tidyr)
library(xlsx)


Metcalfa_behavior_data <- read.xlsx("Data/final_results.xlsx", sheetIndex = 1) %>% 
  filter(Result != "N") %>% 
  mutate(Choosed_the_compound = ifelse(Result == "Blank" | Result == "L", 
                                       "No", 
                                       ifelse(Result == "U", 
                                              "U", 
                                              "Yes")),
         Test = paste(VC1, VC2))

