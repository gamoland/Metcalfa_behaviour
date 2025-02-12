source("Scripts/Data_read_in.R")

Logregdata <- Metcalfa_behavior_data %>% 
  filter(!Test %in% c("DMNT piperiton", "piperiton MeSa")) %>%
  mutate(
    Starters = ifelse(Result == "N", 0, 1),
    Deciders = case_when(
      Result == "U" ~ 0,
      Result == "N" ~ NA_real_,
      TRUE ~ 1
    ),
    Compound = case_when(
      Choosed_the_compound == "Yes" & Result == "piperiton" & Test == "camphor piperiton" ~ 1,
      Choosed_the_compound == "Yes" & Result == "camphor" & Test == "camphor piperiton" ~ 0,
      Choosed_the_compound == "Yes" & Test != "camphor piperiton" ~ 1,
      Result %in% c("N", "U") ~ NA_real_,
      TRUE ~ 0
    )  
  )

#binomialis - Starters
Bin_log_reg_starters <- glmmTMB(Starters ~ Test + (1 | Year),
                                Logregdata,
                                family = "binomial")
Bin_log_reg_starters_y <- glmmTMB(Starters ~ Test + Year,
                                  Logregdata,
                                  family = "binomial")

simulationOutput_starters <- simulateResiduals(fittedModel = Bin_log_reg_starters_y)
plot(simulationOutput_starters)

Anova(Bin_log_reg_starters)
Anova(Bin_log_reg_starters_y)
anova(Bin_log_reg_starters, Bin_log_reg_starters_y)

summary(Bin_log_reg_starters)

em_ST_Y <- emmeans(Bin_log_reg_starters, ~ Test) 
emmeans::contrast(em_ST_Y, "trt.vs.ctrl", ref = "blank blank", type ="response", adjust = "fdr")
