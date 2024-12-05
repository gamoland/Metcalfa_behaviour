source("Scripts/Data_read_in.R")
library(rstatix)

# Data manipulation
Tests_names <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Blank",
         VC1 != "Blank") %>% 
  group_by(Test) %>% 
  summarise()

Metcalfa_behavior_data_N <- read.xlsx("Data/final_results.xlsx", sheetIndex = 1) %>% 
  mutate(Test = as.factor(paste(VC1, VC2))) %>% 
  filter(!Test %in% c("DMNT Piperiton", "Piperiton MeSa"),
         !(J_ID %in% c( "J9", "J10", "J11", "J14")))

Responder_nonresponder <- Metcalfa_behavior_data_N %>%
  mutate(is_blank_blank = ifelse(Test == "Blank Blank", TRUE, FALSE)) %>% 
  group_by(Test) %>% 
  summarise(Responders = ifelse(any(is_blank_blank), 
                                sum(Result %in% c("L", "R")), 
                                sum(Result %in% c("DMNT", "Kámfor", "Piperiton", "MeSa", "Blank"))),
            Non_responders = sum(Result %in% c("U", "N")))

Responder_nonresponder_U <- Metcalfa_behavior_data_N %>%
  mutate(is_blank_blank = ifelse(Test == "Blank Blank", TRUE, FALSE)) %>% 
  group_by(Test) %>% 
  summarise(Responders = ifelse(any(is_blank_blank), 
                                sum(Result %in% c("L", "R")), 
                                sum(Result %in% c("DMNT", "Kámfor", "Piperiton", "MeSa", "Blank"))),
            Non_responders = sum(Result == "U"))

#Nonresponders = U+N
Corrected_p_values_ratio_fisher <- tibble()
i = 1
for (i in 1:nrow(Tests_names)) {
  
  Fisher_table <- Responder_nonresponder %>% 
    filter(Test == "Blank Blank" | Test == Tests_names$Test[i]
    ) %>% 
    column_to_rownames("Test")
  
  Fisher_test <- fisher_test(Fisher_table,
                             detailed = T) %>%
    add_significance() %>%
    add_column(.before = "n", Test = Tests_names$Test[i])
  
  Corrected_p_values_ratio_fisher <- Corrected_p_values_ratio_fisher %>% 
    bind_rows(Fisher_test)
}

Corrected_p_values_ratio_fisher <- Corrected_p_values_ratio_fisher %>% 
  adjust_pvalue(p.col = "p", method = "fdr") %>%
  add_significance()


Responder_nonresponder_wide <- Responder_nonresponder %>% 
  filter(Test != "Blank Blank") %>% 
  column_to_rownames("Test") %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

Pairwise_fisher <- pairwise_fisher_test(Responder_nonresponder_wide[2:5], detailed = T, p.adjust.method = "fdr")


#Nonresponders = U
Corrected_p_values_ratio_fisher_U <- tibble()
i = 1
for (i in 1:nrow(Tests_names)) {
  
  Fisher_table <- Responder_nonresponder_U %>% 
    filter(Test == "Blank Blank" | Test == Tests_names$Test[i]
    ) %>% 
    column_to_rownames("Test")
  
  Fisher_test <- fisher_test(Fisher_table,
                             detailed = T) %>%
    add_significance() %>%
    add_column(.before = "n", Test = Tests_names$Test[i])
  
  Corrected_p_values_ratio_fisher_U <- Corrected_p_values_ratio_fisher_U %>% 
    bind_rows(Fisher_test)
}

Corrected_p_values_ratio_fisher_U <- Corrected_p_values_ratio_fisher_U %>% 
  adjust_pvalue(p.col = "p", method = "fdr") %>%
  add_significance()






