tmp1 <- subset(Logregdata, Compound != NA)
tmp1 <- subset(Logregdata, Compound != "NA")
View(tmp1)
#binomialis - Choosers
Bin_log_reg_choosers <- glmmTMB(formula = Compound ~ Test + (1 | Year),
                                data = tmp1,
                                family = "binomial")
simulationOutput_BCH <- simulateResiduals(fittedModel = Bin_log_reg_choosers)
plot(simulationOutput_BCH)
summary(Bin_log_reg_choosers)
library(car)
Anova(Bin_log_reg_choosers)
em1 <- emmeans(Bin_log_reg_choosers, ~Test)
contrast(em1,"trt.vs.ctrl", ref = "blank blank", type = "response", adjust="fdr")
contrast(em1,"trt.vs.ctrl", ref = "blank blank", type = "response", adjust="fdr")
tmp1 <- subset(Logregdata, Compound != "NA")
#binomialis - Choosers
Bin_log_reg_choosers <- glmmTMB(formula = Compound ~ Test + (1 | Year),
                                data = tmp1,
                                family = "binomial")
simulationOutput_BCH <- simulateResiduals(fittedModel = Bin_log_reg_choosers)
plot(simulationOutput_BCH)
library(emmeans)
em1 <- emmeans(Bin_log_reg_choosers ,~Test)
contrast(em1,"trt.vs.ctrl", ref="blank blank")
summary(Bin_log_reg_choosers)
Anova(Bin_log_reg_choosers)
#binomialis - Choosers
Bin_log_reg_choosers <- glmer(formula = Compound ~ Test + (1 | Year),
                              data = tmp1,
                              family = "binomial")
Anova(Bin_log_reg_choosers)
em1 <- emmeans(Bin_log_reg_choosers ,~Test)
contrast(em1,"trt.vs.ctrl", ref="blank blank")
contrast(em1,"trt.vs.ctrl", ref="blank blank", type=response)
contrast(em1,"trt.vs.ctrl", ref="blank blank", type="response")
#binomialis - Choosers
Bin_log_reg_choosers <- glmmTMB(formula = cbind(Deciders, Undiceded) ~ Test + (1 | Year),
                                data = Logregdata,
                                family = "binomial")
View(Logregdata)
#binomialis - Choosers
Bin_log_reg_choosers <- glmmTMB(formula = cbind(Deciders,(Starters-Deciders)) ~ Test + (1 | Year),
                                data = Logregdata,
                                family = "binomial")
Anova(Bin_log_reg_choosers)
em1 <- emmeans(Bin_log_reg_choosers ,~Test)
tmp2 <- subset(Logregdata, Deciders != "NA")
#binomialis - Deciders
Bin_log_reg_deciders <- glmmTMB(Deciders ~ Test + (1 | Year),
                                data = tmp2,
                                family = "binomial")
simulationOutput_DC <- simulateResiduals(fittedModel = Bin_log_reg_deciders)
plot(simulationOutput_DC)
summary(Bin_log_reg_deciders)
em2 <- emmeans(Bin_log_reg_deciders, ~ Test)
contrast(em2, "trt.vs.crtl", ref = "blank blank", type = "response", adjust = "fdr")
contrast(em2, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
#binomialis - Starters
tmp3 <- subset(Logregdata, Starters != "NA")
Bin_log_reg_starters <- glmmTMB(Starters ~ Test + (1 | Year),
                                tmp3,
                                family = "binomial")
simulationOutput_starters <- simulateResiduals(fittedModel = Bin_log_reg_starters)
plot(simulationOutput_starters)
summary(Bin_log_reg_starters)
anova(Bin_log_reg_starters)
Anova(Bin_log_reg_starters)
em3 <- emmeans(Bin_log_reg_starters, ~Test)
contrast(em3, "trt.vs.ctrl", ref = "blank blank", type = "response")
Bin_log_reg_starters <- glmmTMB(cbind(Compound, (Starters - Compound)) ~ Test + (1 | Year),
                                tmp3,
                                family = "binomial")
Anova(Bin_log_reg_starters)
tmp3$Not <- ifelse(tmp3$Compound == 1, 1, 0)
Bin_log_reg_starters <- glmmTMB(cbind(Compound, Not) ~ Test + (1 | Year),
                                tmp3,
                                family = "binomial")
Anova(Bin_log_reg_starters)
View(tmp3)
#binomialis - Starters
tmp3 <- subset(Logregdata, Starters != 0)
tmp3$Not <- ifelse(tmp3$Compound == 1, 1, 0)
tmp3$Not <- ifelse(tmp3$Not == "NA", 0, tmp3$Not)
tmp3$Not <- ifelse(tmp3$Not == NA, 0, tmp3$Not)
#binomialis - Starters
tmp3 <- subset(Logregdata, Starters != 0)
tmp3$Compound <- ifelse(tmp3$Compound == "NA", 0, tmp3$Compound)
View(tmp3)
tmp3$Compound <- ifelse(tmp3$Compound == 1, 1, 0)
View(tmp3)
tmp3 <- tmp3 %>%
  mutate(Compound = ifelse(is.na(Compound), 0, 1))
mod123 <- glmer(cbind(Compound, (Starters - Compound)) ~ Test + (1 | Year),
                tmp3,
                "binomial")
mod123 <- glmer(cbind(Compound, (Starters - Compound)) ~ Test + (1 | Year),
                tmp3,
                "binomial")
em123 <- emmeans(mod123, ~Test)
em123 <- emmeans(mod123, ~Test)
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
mod123 <- glmmTMB(cbind(Compound, (Starters-Compound)) ~ Test + (1 | Year), tmp3, family = "binomial")
Anova(mod123)
em123 <- emmeans(mod123, ~ Test)
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
mod123 <- glmmTMB(cbind(Compound, (Starters-Compound)) ~ Test + (1 | Year), data = tmp3, family = "binomial")
Anova(mod123)
em123 <- emmeans(mod123, ~ Test)
mod123 <- glmmTMB(Compound ~ Test + (1 | Year), data = tmp3, family = "binomial")
Anova(mod123)
temp123 <- subset(Logregdata, Starters != "NA")
temp123 <- Logregdata %>% subset(Starters != "NA")

tmp1 <- subset(Logregdata, Compound != "NA")
temp123 <- Logregdata %>%
  subset(Starters != "NA")
View(temp123)
temp123 <- Logregdata %>%
  subset(Starters != 0)
temp123 <- Logregdata %>%
  filter(Starters == 0) %>%
  mutate(Compound = case_when(Starters == 1 & Deciders == 0 ~ 0))
temp123 <- Logregdata %>%
  filter(Starters == 0) %>%
  mutate(Compound = ifelse(is.na(Compound), 0, Compound))
temp123 <- Logregdata %>%
  filter(Starters != 0) %>%
  mutate(Compound = ifelse(is.na(Compound), 0, Compound))
mod123 <- glmmTMB(Compouns ~ Test + (1 | Year), data = tmp3, family = "binomial")
mod123 <- glmmTMB(Compound ~ Test + (1 | Year), data = tmp3, family = "binomial")
temp123 <- Logregdata %>%
  filter(Starters != 0) %>%
  mutate(Compound = ifelse(is.na(Compound), 0, Compound))
mod123 <- glmmTMB(Compound ~ Test + (1 | Year), data = tmp123, family = "binomial")
mod123 <- glmmTMB(Compound ~ Test + (1 | Year), data = temp123, family = "binomial")
Anova(mod123)
em123 <- emmeans(mod123, ~ Test)
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
mod123 <- glmmTMB(Compound ~ Test, data = temp123, family = "binomial")
Anova(mod123)
em123 <- emmeans(mod123, ~ Test)
contrast(em123, "trt.vs.ctrl", ref = "blank blank", type = "response")
mod1234 <- glmmTMB(Compound ~ Test + (1 | year), data = temp123, family = "binomial")
mod1234 <- glmmTMB(Compound ~ Test + (1 | Year), data = temp123, family = "binomial")
anova(mod123, mod1234)
temp12345 <- temp123 %>%
  filter(Deciders != 0)
View(temp12345)
mod12345 <- glmer(Compound ~ Test + (1 | Year), data = temp12345, family = "binomial")
mod12345_y <- glm(Compound ~ Test, data = temp12345, family = "binomial")
anova(mod12345, mod12345_y)
em12345 <- emmeans(mod12345, ~ Test)
contrast(em12345, "trt.vs.ctrl", ref = "blank blank", type = "response")
contrast(em12345, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
contrast(em12345, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")
temp123456 <- Logregdata %>%
  filter(Starters != 0)
View(temp123456)
mod123456 <- glmmTMB(Deciders ~ Test + (1 | Year), data = tmp2, family = "binomial")
mod123456 <- glmmTMB(Deciders ~ Test + (1 | Year), data = tmp2, family = "binomial")
mod123456 <- glmmTMB(Deciders ~ Test + (1 | Year), data = tmp2, family = "binomial")
mod123456 <- glmmTMB(Deciders ~ Test + (1 | Year), data = temp123456, family = "binomial")
em123456 <- emmeans(mod123456, ~ Test)
contrast(em123456, "trt.vs.crtl", ref = "balnk blank", type = "response")
em123456 <- emmeans(mod123456, ~ Test)
contrast(em123456, "trt.vs.crtl", ref = "blank blank", type = "response")
mod123456 <- glmer(Deciders ~ Test + (1 | Year), data = temp123456, family = "binomial")
em123456 <- emmeans(mod123456, ~ Test)
contrast(em123456, "trt.vs.crtl", ref = "blank blank", type = "response")
summary(mod123456)
temp123456 <- Logregdata %>%
  filter(Starters != 0)
mod1234567 <- glmmTMB(Deciders ~ Test + (1 | Year), data = temp123456, family = "binomial")
summary(mod1234567)
mod1234567 <- glmmTMB(Deciders ~ Test + (1 | Year), data = temp123456, family = "binomial")
em1234567 <- emmeans(mod1234567, ~ Test)
contrast(em1234567, "trt.vs.ctrl", ref = "blank blank", type = "response")

