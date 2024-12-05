source("Scripts/Data_read_in.R")
library("ggstatsplot")
library(stringi)
library(MASS)
library("bestNormalize")
library("timetk")
library(ggplot2)

Responder_nonresponder_undecided <- Metcalfa_behavior_data %>%
  filter(!(Test %in% c("DMNT piperiton", "piperiton Mesa"))) %>% 
  mutate(is_blank_blank = ifelse(Test == "blank blank", TRUE, FALSE)) %>% 
  group_by(Test) %>% 
  summarise(n = n(),
            Responders = ifelse(any(is_blank_blank), 
                                sum(Result %in% c("L", "R")), 
                                sum(Result %in% c("DMNT", "camphor", "piperiton", "MeSa", "blank"))),
            Undecided = sum(Result == "U"),
            Non_starters = sum(Result == "N"),
            Responders_percent = round(ifelse(any(is_blank_blank), 
                                       (sum(Result %in% c("L", "R"))*100)/n(), 
                                       (sum(Result %in% c("DMNT", "camphor", "piperiton", "MeSa", "blank"))*100)/n()), 2),
            Undecided_percent = round((sum(Result == "U")*100)/n(), 2),
            Non_starters_percent = round((sum(Result == "N")*100)/n(), 2))

write.xlsx(Responder_nonresponder_undecided, "Output/Sample_size.xlsx")













Final_data <- Metcalfa_behavior_data %>% 
  filter(!(Test %in% c("DMNT piperiton", "piperiton Mesa")))


Metcalfa_behavior_data_boxcox <- Metcalfa_behavior_data %>% 
  filter(!is.na(duration))%>% 
  group_by(Test) %>% 
  mutate(duration_boxcoxed = box_cox_vec(duration)) %>% 
  ungroup()


Plot_time_Blank <- Metcalfa_behavior_data_boxcox %>% 
  filter(Result != "U",
         !(Test %in% c("DMNT piperiton", "camphor piperiton"))) %>% 
  mutate(Xaxis = if_else(Result == "blank", "blank", "Compound")) %>% 
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


  
  