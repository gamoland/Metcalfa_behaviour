source("Scripts/Data_read_in.R")
library("survival")
library("survminer")
library("coxme")
library("rms")

#N --> ki kell zárni
#U --> cenzoráltak
#B --> cenzoráltak
#compund --> nem cesoráltak

# write.csv2(Survavival_data, file="SurvData_EXPORTED.csv")
Survd <- read.csv("SurvData_EXPORTED.csv")
Survd$Test <- factor(Survd$Test)
Survd$Surv_result <- factor(Survd$Surv_result)

Surv_object <- Surv(time = Survd$Duration, event = Survd$Censored)

#Fit model
Fit_model_coxme <- coxme(formula = Surv_object ~ Surv_result + (1 | Year), 
                         data = Survd)
summary(Fit_model_coxme)

#Check assumptions
plot(cox.zph(Fit_model_coxme))
summary(cox.zph(Fit_model_coxme)$table)

#Check if random effect has a sign effect
Fit_model_coxph <- coxph(formula =  Surv_object ~ Surv_result, 
                         data = Survd)
anova(Fit_model_coxph, Fit_model_coxme, test = "Chisq")
Anova(Fit_model_coxme)

#pairwise comparision of log hazard rates
Kaki <- emmeans(Fit_model_coxme, pairwise ~ Surv_result,  adjust = "fdr", level = .84)

#Visualize survival curves
Plot_model_coxph <- coxph(formula =  Surv_object ~ Surv_result, 
                         data = Survd)
Plot_data<- survfit(Surv_object ~ Surv_result, data = Survd)
ggsurvplot(data = Survd, 
                           fit = Plot_data,
                           risk.table = TRUE,    
                           risk.table.col = "Surv_result", 
                           # conf.int = TRUE, 
                           # conf.int.style = "step",
                           pval = TRUE)


#Vegyületenként blank nélkül
#Fit model
Survd_B <- subset(Survd, Result!="blank")
Surv_object_B <- Surv(time = Survd_B$Duration, event = Survd_B$Censored)
Fit_model_coxme_B <- coxme(formula = Surv_object_B ~ Result + (1 | Year), 
                         data = Survd_B)
summary(Fit_model_coxme_B)

#Check assumptions
plot(cox.zph(Fit_model_coxme_B))
summary(cox.zph(Fit_model_coxme_B)$table)

#Check if random effect has a sign effect
Fit_model_coxph_B <- coxph(formula =  Surv_object_B ~ Result, 
                         data = Survd_B)
anova(Fit_model_coxph_B, Fit_model_coxme_B, test = "Chisq")
Anova(Fit_model_coxme_B)

#pairwise comparision of log hazard rates
emmeans(Fit_model_coxme_B, ~ Result,  adjust = "fdr", level = .84)

#Visualize survival curves
Plot_data<- survfit(Surv_object_B ~ Result, data = Survd_B)
ggsurvplot(data = Survd_B, 
           fit = Plot_data,
           #risk.table = TRUE,    
           #risk.table.col = "Result", 
           conf.int = TRUE, 
           conf.int.style = "step",
           #pval = 
           )







#Piperinton&camphor
Survavival_data_PipCam <- Metcalfa_behavior_data %>%
  mutate(Censored = ifelse(Result == "U", 0, 1),
         duration = as.numeric(duration)) %>% 
  filter(Test == "camphor piperiton",
         Result %in% c("piperiton", "camphor"),
         !is.na(duration))

write.xlsx(Survavival_data_PipCam, file="Output/SurvData_PipCam.xlsx")
SurvD_PC <- read.xlsx("Output/SurvData_PipCam.xlsx")
SurvD_PC <- read.xlsx("Output/SurvData_PipCam.xlsx", sheetIndex = 1)

Surv_object_PipCam <- Surv(time = SurvD_PC$duration, 
                           event = SurvD_PC$Censored)
Fit_model_coxph__PipCamp <- coxph(formula = Surv_object_PipCam ~ Result, 
                                  data = SurvD_PC)
summary(Fit_model_coxph__PipCamp)

plot(cox.zph(Fit_model_coxph__PipCamp))
summary(cox.zph(Fit_model_coxph__PipCamp)$table)

anova(Fit_model_coxph__PipCamp)

Plot_data_PipCam <- survfit(Surv(time = duration, event = Censored) ~ Result, 
                            data = SurvD_PC)
Plot_PipCamp <- ggsurvplot(data = SurvD_PC, 
                           fit = Plot_data_PipCam,
                           #risk.table = TRUE,    
                           #risk.table.col = "Result", 
                           conf.int = TRUE, 
                           conf.int.style = "step",
                           #pval = TRUE
                           )
ggsave(plot = Plot_PipCamp, filename = "Output/Surv_PipCamp.pdf", width = 250, height = 160, units = "mm")

