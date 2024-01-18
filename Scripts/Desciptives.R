source("Scripts/Data_read_in.R")
library("ggstatsplot")
library(stringi)
library(MASS)
library("bestNormalize")
library("timetk")

For_descriptives <- Metcalfa_behavior_data %>% 
  filter(VC1 != "Blank",
         !(Result %in%  c("Blank", "U"))) %>% 
  group_by(Test) 

Replicates <- For_descriptives %>% 
  summarise(n = n()) #--> based on this "Piperiton  Mesa" should be excluded

Metcalfa_behavior_data_boxcox <- Metcalfa_behavior_data %>% 
  filter(!is.na(duration))%>% 
  group_by(Test) %>% 
  mutate(duration_boxcoxed = box_cox_vec(duration)) %>% 
  ungroup()


Plot_time <- Metcalfa_behavior_data_boxcox %>% 
  filter(Result != "U") %>% 
  ggplot() +
  geom_violin(aes(x = Result, y = duration),fill = "salmon", color = "black")+
  geom_jitter(aes(x = Result, y = duration), width = 0.2) + 
  facet_grid(Test ~ .)
Plot_time


  
  