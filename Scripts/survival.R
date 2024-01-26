source("Scripts/Data_read_in.R")
library("survival")
library("survminer")
library("coxme")
library("rms")
library("adjustedCurves")
library(ggplot2)
library(reshape2)
library("pammtools")



#kétfélét lehetne csinálni: 1) censoráltnak az U-kat, meghaltnak a célbaérőket blankokat kizárva; (event occured? 0 no, 1 for yes)
                           #2) censoráltnak az U-kat, meghaltnak a célbaérőket --> ezt nem lehet megcsinálni, mert 2-4 blankot választó adatunk van

#Create data for mixed-effects Cox model
Survavival_data <- Metcalfa_behavior_data %>%
  mutate(Censored = ifelse(Result == "U", 0, 1)) %>% 
  filter(!is.na(duration),
         !Test %in% c("DMNT Piperiton", "Kámfor Piperiton"),
         # Result != "U"
         ) %>%
  droplevels()

Surv_object <- Surv(time = Survavival_data$duration, event = Survavival_data$Censored)

#Fit model
Fit_model_coxme <- coxme(formula = Surv_object ~ VC1 + ( 1 | Test), data = Survavival_data)

#Check assumptions
plot(cox.zph(Fit_model_coxme))
summary(cox.zph(Fit_model_coxme)$table)

#Check if random effect has a sign effect
Fit_model_coxph <- coxph(formula = Surv(time = Survavival_data$duration, event = Survavival_data$Censored) ~ VC1, data = Survavival_data)
Lr_test_result <- anova(Fit_model_coxph, Fit_model_coxme, test = "Chisq")
Lr_test_result

#Likelihood Ratio Test
lrt_result <- anova(Fit_model_coxme)
lrt_result

#pairwise comparision of log hazard rates
Kaki <- emmeans(Fit_model_coxme, pairwise ~ VC1,  adjust = "fdr", level = .84)

#Visualize survival curves
CPH_object <- cph(Surv_object ~ VC1, random = Fit_model_coxme$Test, x = TRUE, y = TRUE, data = Survavival_data)
ggadjustedcurves(CPH_object, variable = "VC1", data = Survavival_data )


#Piperinton&camphor
Survavival_data_PipCam <- Metcalfa_behavior_data %>%
  mutate(Censored = ifelse(Result == "U", 0, 1)) %>% 
  filter(Test == "Kámfor Piperiton",
         # Result != "U"
  ) %>%
  droplevels()

Surv_object_PipCam <- Surv(time = Survavival_data_PipCam$duration, event = Survavival_data_PipCam$Censored)
Fit_model_coxph__PipCamp <- coxph(formula = Surv_object_PipCam ~ Result, data = Survavival_data_PipCam)
summary(Fit_model_coxph__PipCamp)

Plot_data_PipCam <- survfit(Surv(time = duration, event = Censored) ~ Result, 
                            data = Survavival_data_PipCam)
ggsurvplot(data = Survavival_data_PipCam, fit = Plot_data_PipCam)
pairwise_survdiff(formula = Surv(time = duration, event = Censored) ~ Result, 
                  data = Survavival_data_PipCam, 
                  p.adjust.method = "fdr")
