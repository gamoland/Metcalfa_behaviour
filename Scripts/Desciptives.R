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


Plot_time_Blank <- Metcalfa_behavior_data_boxcox %>% 
  filter(Result != "U",
         !(Test %in% c("DMNT Piperiton", "Kámfor Piperiton"))) %>% 
  mutate(Xaxis = if_else(Result == "Blank", "Blank", "Compound")) %>% 
  ggplot() +
  geom_jitter(aes(x = Xaxis, y = duration), width = 0.15, ) +
  facet_grid(VC1 ~ ., ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_line(color = "grey", linetype = "dotted", size = 0.5),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(color = "black", fill = "white"),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.spacing = unit(0.7, "lines")
        ) +
  labs(x = "Side", y = "time (min)")
  


Plot_time_PipCamphor<- Metcalfa_behavior_data_boxcox %>% 
  filter(Result != "U",
         Test == "Kámfor Piperiton") %>% 
  ggplot() +
  geom_jitter(aes(x = Result, y = duration), width = 0.15, ) +
  # facet_grid(VC1 ~ ., ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_line(color = "grey", linetype = "dotted", size = 0.5),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(color = "black", fill = "white"),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.spacing = unit(0.7, "lines")
  ) +
  labs(x = "Side", y = "time (min)")
Plot_time_PipCamphor


ggsave("Output/Time_Blanks.pdf", plot = Plot_time, width = 20, height = 40, units = "cm")
ggsave("Output/PipCamphor.pdf", plot = Plot_time_PipCamphor, width = 20, height = 40, units = "cm")


  
  