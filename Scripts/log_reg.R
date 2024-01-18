library("tidymodels")
library("ROCR")
library(emmeans)
library(DHARMa)
library("car")
library(MASS)
library("performance")
library("see")
library(nlme)
library(grafify)
library(emmeans)



source("Scripts/Data_read_in.R")


Log_reg_data <- Metcalfa_behavior_data %>% 
  filter(Choosed_the_compound != "U",
         VC1 == "Piperiton " | VC1 == "Kámfor") %>% 
  mutate(Result_binary = ifelse(Choosed_the_compound == "Yes", 
                                1,
                                0))


Pip_Camphor_logreg <- Metcalfa_behavior_data %>% 
  filter(Choosed_the_compound != "U",
         VC2 == "Piperiton",
         VC1 == "Kámfor") %>% 
  mutate(Result_binary = ifelse(Result == "Piperiton", 
                                1,
                                0))


Plot_Test <- Log_reg_data %>% 
  ggplot() +
  geom_bar(aes(fill = Choosed_the_compound, x = VC1))
Plot_Test

#Assumptions
plot <- Pip_Camphor_logreg %>% 
  ggplot() +
  geom_violin(aes(x = Result, y = duration),fill = "salmon", color = "black")+
  geom_jitter(aes(x = Result, y = duration), width = 0.2)
plot


logistic_model <- glm(Result_binary ~ Result * duration,
                      data = Pip_Camphor_logreg,
                      family = "binomial")
# Summary
summary(logistic_model)


#Cutting out the bad camphors
First_set_camphor <- Log_reg_data %>% 
  arrange(DATE) %>% 
  slice(1:30)

#Assumptions
ggplot(First_set_camphor, aes(x = Test)) +
  geom_bar(fill = "salmon", color = "black")

logistic_model_short <- glm(duration ~ Choo,
                      data = Metcalfa_behavior_data_boxcox,
                      family = "poisson")
summary(logistic_model_short)


poisson <- fitdistr(Metcalfa_behavior_data$duration, "Poisson")
qqp(First_set_camphor$Result_binary, "pois", lambda = poisson$estimate,)



#duration-test

Plot_ <- Metcalfa_behavior_data %>% 
  filter(Result != "U") %>% 
  ggplot() +
  geom_jitter(aes(x = Test, y = duration), width = 0.2)
Plot_

GLM_data <- Metcalfa_behavior_data %>% 
  filter((!Result %in% c("U"
                         , "Mesa", "DMNT"
                         )),
         VC1 != "Blank",
         Test  != "DMNT Piperiton"
         ) %>%
  mutate(J_ID_ID = paste(J_ID,ID))

GLM_DURATION <- lme(fixed = duration ~ Result,
                    random = ~ 1 | Test,
                    data = GLM_data,
                    na.action = na.omit)
summary(GLM_DURATION)
anova(GLM_DURATION)

simulationOutput_lme <- simulateResiduals(fittedModel = GLM_DURATION)
plot(simulationOutput_lme)
posthoc <- emmeans(GLM_DURATION, "Result")
summary(posthoc, adjust = "fdr", infer = c(TRUE, FALSE), level = .84)
PostHoc_results <- posthoc_Pairwise(Model = GLM_DURATION, Fixed_Factor = "Result", P_Adj = "fdr", level = .84)
summary(PostHoc_results, level = .84)
