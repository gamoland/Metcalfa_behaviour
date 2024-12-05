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
library(lme4)
library(sandwich)
library(lmtest)
library(glmmTMB)

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

Summary_data <- Logregdata %>% 
  group_by(Test) %>% 
  summarise(
    Nonstarters = sum(!Starters),
    Starters = sum(Starters == T, na.rm = T),
    Undiceded = sum(!Deciders, na.rm = T),
    Deciders = sum(Deciders == T, na.rm = T),
    Blank = sum(!Compound, na.rm = T),
    Compound = sum(Compound == T, na.rm = T),
    Total = n())

  
tmp1<-subset(Logregdata,Compound!="NA")
#binomialis - Choosers
Bin_log_reg_choosers <- glmmTMB(formula = Compound ~ Test + (1 | Year), 
                                data = tmp1,
                                family = "binomial")
simulationOutput_BCH <- simulateResiduals(fittedModel = Bin_log_reg_choosers)
plot(simulationOutput_BCH)

em123 <- emmeans(Bin_log_reg_choosers, ~ Test)

contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response")
test(em1,adjust="fdr",type="response")
summary(Bin_log_reg_choosers)


tmp3 <- tmp3 %>% 
  mutate(Compound = ifelse(is.na(Compound), 0, 1))

temp123 <- Logregdata %>% 
  filter(Starters != 0,
         Test != "camphor piperiton") %>% 
  mutate(Compound = ifelse(is.na(Compound),
                           0,
                           Compound))
 
mod123 <- glmmTMB(Compound ~ Test,
                data = temp123,
                family = "binomial")
         
Anova(mod123)      
em123 <- emmeans(mod123, ~ Test)
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response")

temp12345 <- temp123 %>% 
  filter(Deciders != 0)
mod12345 <- glmer(Compound ~ Test + (1 | Year),
                  data = Logregdata,
                  family = "binomial")
mod12345_y <- glm(Compound ~ Test + Year,
                  data = Logregdata,
                  family = "binomial")
Anova(mod12345)
summary(mod12345)
anova(mod12345, mod12345_y)

em12345 <- emmeans(mod12345, ~ Test)
contrast(em12345, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")


# foraging behaviourt befolysolja az illataanyg jelenléte, a választást magát igazából nem, hibána akarjuk
# de erre igaázból jó a starters- nonstartesr összehasonlítás, ahol ez kijött, hogy a mesa meg a dmnt ha ott van, akkor kevesebben indulnak el
# ha már elindultak, akkor viszont nem választják egyiket sem többen, csak egy trend figyelhető meg --> kevés a mintaszám


C_WNAs <- subset(Logregdata, Compound != "NA" & Test != "camphor piperiton")

mod_final <- glmer(Compound ~ Test + (1 | Year),
                   data = C_WNAs,
                   family = "binomial")
mod_final_y <- glm(Compound ~ Test * Year,
                   data = C_WNAs,
                   family = "binomial")
anova(mod_final, mod_final_y)
summary(mod_final)
Anova(mod_final_y)
em_final <- emmeans(mod_final, ~ Test, level = .84)
contrast(em_final, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr", level = .84)


#binomialis - Deciders

tmp2<-subset(Logregdata, Deciders!="NA" & Test != "camphor piperiton")
Bin_log_reg_deciders <- glmmTMB(Deciders ~ Test + (1 | Year),
                    data=tmp2,
                    family = "binomial")
Bin_log_reg_deciders_y <- glmmTMB(Deciders ~ Test + Year,
                                data=tmp2,
                                family = "binomial")


simulationOutput_DC <- simulateResiduals(fittedModel = Bin_log_reg_deciders_y)
plot(simulationOutput_DC)

Anova(Bin_log_reg_deciders)
Anova(Bin_log_reg_deciders_y)
anova(Bin_log_reg_deciders_y, Bin_log_reg_deciders)

summary(Bin_log_reg_deciders_y)

em2 <- emmeans(Bin_log_reg_deciders_y, ~ Test | Year)
contrast(em2, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")



temp123456 <- Logregdata %>% 
  filter(Starters != 0,
         Test != "camphor piperiton")
mod123456 <- glmmTMB(Deciders ~ Test + (1 | Year),
                             data=temp123456,
                             family = "binomial")
summary(mod123456)
em123456 <- emmeans(mod123456, ~ Test) 
contrast(em123456, "trt.vs.ctrl", ref = "blank blank", type ="response")


#binomialis - Starters
tmp3<-subset(Logregdata, Starters!="NA" & Test != "camphor piperiton")
Bin_log_reg_starters <- glmer(Starters ~ Test + (1 | Year),
                    Logregdata,
                    family = "binomial")
Bin_log_reg_starters_y <- glm(Starters ~ Test + Year,
                              Logregdata,
                                family = "binomial")


simulationOutput_starters <- simulateResiduals(fittedModel = Bin_log_reg_starters_y)
plot(simulationOutput_starters)

Anova(Bin_log_reg_starters)
Anova(Bin_log_reg_starters_y)
anova(Bin_log_reg_starters, Bin_log_reg_starters_y)

summary(Bin_log_reg_starters_y)
summary(Bin_log_reg_starters)


em_ST_Y <- emmeans(Bin_log_reg_starters, ~ Test) 
contrast(em_ST_Y, "trt.vs.ctrl", ref = "blank blank", type ="response", adjust = "fdr")
