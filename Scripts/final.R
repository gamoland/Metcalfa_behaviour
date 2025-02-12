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


em_ST <- emmeans(Bin_log_reg_starters, ~ Test) 
emmeans::contrast(em_ST, "trt.vs.ctrl", ref = "blank blank", type ="response", adjust = "fdr")


#binomialis - Deciders
Deciders_data<-subset(Logregdata, Deciders!="NA")
Bin_log_reg_deciders <- glmmTMB(Deciders ~ Test + (1 | Year),
                                data=Deciders_data,
                                family = "binomial")
Bin_log_reg_deciders_y <- glmmTMB(Deciders ~ Test + Year,
                                  data=Deciders_data,
                                  family = "binomial")


simulationOutput_DC <- simulateResiduals(fittedModel = Bin_log_reg_deciders_y)
plot(simulationOutput_DC)

Anova(Bin_log_reg_deciders)
Anova(Bin_log_reg_deciders_y)
anova(Bin_log_reg_deciders_y, Bin_log_reg_deciders)

summary(Bin_log_reg_deciders_y)

em_D_Y <- emmeans(Bin_log_reg_deciders_y, ~ Test | Year)
contrast(em2, "trt.vs.ctrl", ref = "blank blank", type = "response", adjust = "fdr")

#binomialis - Choosers
C_WNAs <- subset(Logregdata, Compound != "NA")

mod_final <- glmer(Compound ~ Test + (1 | Year),
                   data = C_WNAs,
                   family = "binomial")
mod_final_y <- glm(Compound ~ Test * Year,
                   data = C_WNAs,
                   family = "binomial")
Anova(mod_final_y)
Anova(mod_final)
anova(mod_final, mod_final_y)
summary(mod_final)

em_final <- emmeans(mod_final, ~ Test)
emmeans::test(em_final, adjust = "fdr")


#ICICLE

library(plotly)

# Define labels (unique nodes)
labels <- c(
  "Nymphs",
  "blank blank", "Starters blank blank", "Decided blank blank", "Chose Blank blank blank", "Chose Compound blank blank", "Undecided blank blank", "Nonstarters blank blank",
  "blank camphor", "Starters blank camphor", "Decided blank camphor", "Chose Blank blank camphor", "Chose Compound blank camphor", "Undecided blank camphor", "Nonstarters blank camphor",
  "blank DMNT", "Starters blank DMNT", "Decided blank DMNT", "Chose Blank blank DMNT", "Chose Compound blank DMNT", "Undecided blank DMNT", "Nonstarters blank DMNT",
  "blank MeSa", "Starters blank MeSa", "Decided blank MeSa", "Chose Blank blank MeSa", "Chose Compound blank MeSa", "Undecided blank MeSa", "Nonstarters blank MeSa",
  "blank piperiton", "Starters blank piperiton", "Decided blank piperiton", "Chose Blank blank piperiton", "Chose Compound blank piperiton", "Undecided blank piperiton", "Nonstarters blank piperiton",
  "camphor piperiton", "Starters camphor piperiton", "Decided camphor piperiton", "Chose Blank camphor piperiton", "Chose Compound camphor piperiton", "Undecided camphor piperiton", "Nonstarters camphor piperiton"
)

# Define parent-child relationships
parents <- c(
  "",  # Root node
  "Nymphs", "blank blank", "Starters blank blank", "Decided blank blank", "Decided blank blank", "Starters blank blank", "blank blank",
  "Nymphs", "blank camphor", "Starters blank camphor", "Decided blank camphor", "Decided blank camphor", "Starters blank camphor", "blank camphor",
  "Nymphs", "blank DMNT", "Starters blank DMNT", "Decided blank DMNT", "Decided blank DMNT", "Starters blank DMNT", "blank DMNT",
  "Nymphs", "blank MeSa", "Starters blank MeSa", "Decided blank MeSa", "Decided blank MeSa", "Starters blank MeSa", "blank MeSa",
  "Nymphs", "blank piperiton", "Starters blank piperiton", "Decided blank piperiton", "Decided blank piperiton", "Starters blank piperiton", "blank piperiton",
  "Nymphs", "camphor piperiton", "Starters camphor piperiton", "Decided camphor piperiton", "Decided camphor piperiton", "Starters camphor piperiton", "camphor piperiton"
)

# Define values (using provided data)
values <- c(
  sum(c(53, 40, 49, 42, 33, 55)),  # Nymphs total
  53, 45, 24, 12, 12, 21, 8,  # blank blank
  40, 33, 23, 5, 18, 10, 7,   # blank camphor
  49, 34, 21, 9, 12, 13, 15,  # blank DMNT
  42, 29, 19, 8, 11, 10, 13,  # blank MeSa
  33, 27, 20, 4, 16, 7, 6,    # blank piperiton
  55, 33, 25, 14, 11, 8, 22   # camphor piperiton
)

# Define hover text (to show values)
hover_text <- labels
hover_text[2:length(labels)] <- paste0(labels[2:length(labels)], ": ", values[2:length(values)])
colors <- c(
  "white",  # Nymphs
  '#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000',  # blank blank
  '#dadaeb','#bcbddc','#9e9ac8','#6a51a3','#807dba','#54278f','#3f007d',  # blank camphor
  '#fdd0a2','#fdae6b','#fd8d3c','#d94801','#f16913','#a63603','#7f2704',  # blank DMNT
  '#c7e9c0','#a1d99b','#74c476','#238b45','#41ab5d','#006d2c','#00441b',  # blank MeSa
  '#c6dbef','#9ecae1','#6baed6','#2171b5','#4292c6','#08519c','#08306b',  # blank piperiton
  '#F5E6AD','#F4CAA4','#F4AD9B','#F39192','#F27589','#F25880','#F13C77'  # camphor piperiton
)
marker <- list(
  colors = ~colors,
  line = list(
    color = "black",
    width = 2
  )
)

# Create the icicle plot
fig <- plot_ly(
  labels = labels,
  parents = parents,
  values = values,
  text = hover_text,
  hoverinfo = "text",
  type = "icicle",
  branchvalues = 'total',
  textinfo = "text",
  tiling = list(orientation = "v"),
  marker = marker)



