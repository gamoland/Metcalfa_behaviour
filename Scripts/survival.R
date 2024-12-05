source("Scripts/Data_read_in.R")
library("survival")
library("survminer")
library("coxme")
library("rms")

grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}


#N --> ki kell zárni
#U --> cenzoráltak
#B --> cenzoráltak
#compund --> nem cesoráltak



#Create data for mixed-effects Cox model for checking if blank was choose faster
Survavival_data <- Metcalfa_behavior_data %>%
  mutate(Duration = ifelse(duration == "na",
                           NA, 
                           as.numeric(duration)),
         Surv_result = ifelse(Result %in% c("blank", "L", "R"),
                              "blank",
                              "compound"),
         Test = factor(Test)) %>% 
  filter(!is.na(Duration),
         !Test %in% c("DMNT piperiton", "camphor piperiton", "piperiton MeSa"),
         !Result %in% c("U", "N")) %>%
  mutate(Censored = ifelse(Result %in% c("U", "N"), 0, 1))

write.csv2(Survavival_data, file="SurvData_EXPORTED.csv")
Survd <- read.csv("SurvData_EXPORTED.csv")
Survd$Test <- factor(Survd$Test)
Survd$Surv_result <- factor(Survd$Surv_result)


Survavival_data2 <- subset(Survavival_data, Test!="piperiton MeSa"& Test!="DMNT piperiton"& Test!="camphor piperiton")

Surv_object <- Surv(time = Survd$Duration, event = Survd$Censored)

#Fit model
Fit_model_coxme <- coxme(formula = Surv_object ~ Surv_result * Test + (1 | Year), 
                         data = subset(Survd,Test!="blank blank"))
summary(Fit_model_coxme)
#ha a test-nek nincs hatása, elhagyhatom?

#Check assumptions
plot(cox.zph(Fit_model_coxme))
summary(cox.zph(Fit_model_coxme)$table)

#Check if random effect has a sign effect
Fit_model_coxph <- coxph(formula =  Surv_object ~ Surv_result * Test, data = Survd)
Lr_test_result <- anova(Fit_model_coxph, Fit_model_coxme, test = "Chisq")
Lr_test_result

#Likelihood Ratio Test
lrt_result <- anova(Fit_model_coxme)
lrt_result # --> random has effect, must use COXME!!!!

#pairwise comparision of log hazard rates
Kaki <- emmeans(Fit_model_coxme, pairwise ~ Result,  adjust = "fdr", level = .84)

#Visualize survival curves
CPH_object <- cph(Surv_object ~ Result, random = Fit_model_coxme$Test, x = TRUE, y = TRUE, data = Survavival_data)
ggadjustedcurves(CPH_object, variable = "Result", data = Survavival_data )


#Piperinton&camphor
Survavival_data_PipCam <- Metcalfa_behavior_data %>%
  mutate(Censored = ifelse(Result == "U", 0, 1)) %>% 
  filter(Test == "Kámfor Piperiton",
         Result != "U"
  ) %>%
  droplevels()

Surv_object_PipCam <- Surv(time = Survavival_data_PipCam$duration, event = Survavival_data_PipCam$Censored)
Fit_model_coxph__PipCamp <- coxph(formula = Surv_object_PipCam ~ Result, data = Survavival_data_PipCam)
summary(Fit_model_coxph__PipCamp)

plot(cox.zph(Fit_model_coxph__PipCamp))
summary(cox.zph(Fit_model_coxph__PipCamp)$table)

Plot_data_PipCam <- survfit(Surv(time = duration, event = Censored) ~ Result, 
                            data = Survavival_data_PipCam)
Plot_PipCamp <- ggsurvplot(data = Survavival_data_PipCam, 
                           fit = Plot_data_PipCam,
                           risk.table = TRUE,    
                           risk.table.col = "Result", 
                           # conf.int = TRUE, 
                           # conf.int.style = "step",
                           pval = TRUE)
ggsave(plot = Plot_PipCamp, filename = "Output/Surv_PipCamp.pdf", width = 250, height = 160, units = "mm")


#Against blanks but without blanks
Survavival_data_All_ohneblank <- Metcalfa_behavior_data %>%
  mutate(Censored = ifelse(Result == "U", 0, 1)) %>% 
  filter(!is.na(duration),
         !Test %in% c("DMNT Piperiton", "Kámfor Piperiton"),
         !Result %in% c( "U", "Blank")
  ) %>%
  droplevels()

Surv_object_ohneblank <- Surv(time = Survavival_data_All_ohneblank$duration, event = Survavival_data_All_ohneblank$Censored)
Fit_model_coxph__ohneblank <- coxph(formula = Surv_object_ohneblank ~ Result, data = Survavival_data_All_ohneblank)
summary(Fit_model_coxph__ohneblank)

plot(cox.zph(Fit_model_coxph__ohneblank))
summary(cox.zph(Fit_model_coxph__ohneblank)$table)

Plot_data_ohneblank <- survfit(Surv(time = duration, event = Censored) ~ Result, 
                            data = Survavival_data_All_ohneblank)

Plot_blank <- ggsurvplot(data = Survavival_data_All_ohneblank, 
                         fit = Plot_data_ohneblank, 
                         risk.table = TRUE,    
                         risk.table.col = "Result", 
                         # conf.int = TRUE, 
                         # conf.int.style = "step",
                         pval = TRUE)
ggsave(plot = Plot_blank, filename = "Output/Surv_Blank.pdf", width = 250, height = 160, units = "mm")
pairwise_survdiff(formula = Surv(time = duration, event = Censored) ~ Result, 
                  data = Survavival_data_All_ohneblank, 
                  p.adjust.method = "fdr")
