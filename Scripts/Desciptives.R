source("Scripts/Data_read_in.R")
library("ggstatsplot")
library(stringi)

For_descriptives <- Metcalfa_behavior_data %>% 
  filter(VC1 != "Blank",
         !(Result %in%  c("Blank", "U"))) %>% 
  group_by(Test) 

Replicates <- For_descriptives %>% 
  summarise(n = n()) #--> based on this "Piperiton  Mesa" should be excluded

# Violin_plot <- For_descriptives %>% 
#   filter(VC1 != "Blank",
#          !(Result %in%  c("Blank", "U"))) %>% 
#   ggbetweenstats(x = Test, y = duration)


  
  