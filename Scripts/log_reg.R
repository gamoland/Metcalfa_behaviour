library("tidymodels")
library("ROCR")
library(emmeans)
library(DHARMa)
library("car")
library(MASS)

source("Scripts/Data_read_in.R")


Log_reg_data <- Metcalfa_behavior_data %>% 
  filter(Choosed_the_compound != "U",
         VC2 == "Blank") %>% 
  mutate(Result = as.factor(Result),
         Result_binary = ifelse(Choosed_the_compound == "Yes", 
                                1,
                                0))

Plot_Test <- Log_reg_data %>% 
  ggplot() +
  geom_bar(aes(fill = Choosed_the_compound, x = VC1))
Plot_Test

#Assumptions
ggplot(Log_reg_data, aes(x = Test)) +
  geom_bar(fill = "salmon", color = "black")

logistic_model <- glm(Result_binary ~ Test,
                      data = Log_reg_data,
                      family = "binomial")
# Summary
summary(logistic_model)


#Cutting out the bad camphors
First_set_camphor <- Log_reg_data %>% 
  arrange(DATE) %>% 
  slice(1:63)

#Assumptions
ggplot(First_set_camphor, aes(x = Test)) +
  geom_bar(fill = "salmon", color = "black")

logistic_model_short <- glm(Result_binary ~ Test,
                      data = First_set_camphor,
                      family = "binomial")
summary(logistic_model_short)


poisson <- fitdistr(First_set_camphor$Result_binary, "Poisson")
qqp(First_set_camphor$Result_binary, "pois", lambda = poisson$estimate,)




