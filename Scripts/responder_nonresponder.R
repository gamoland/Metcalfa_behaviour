source("Scripts/Data_read_in.R")
library(rstatix)



First_set_camphor <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Blank") %>% 
  arrange(DATE) %>% 
  slice(1:83)

Cont_table_1_responders <- First_set_camphor %>% 
  group_by(Test) %>% 
  summarise(Choose = sum(Choosed_the_compound == "Yes" | Choosed_the_compound == "No"))

Cont_table_2_responders <- First_set_camphor %>% 
  group_by(Test) %>% 
  summarise(DidntChoose = sum(Choosed_the_compound == "U"))


Cont_table_responderratio <- Cont_table_1_responders %>% 
  left_join(Cont_table_2_responders) %>%
  column_to_rownames("Test") %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

Cont_table_responderratio <- as.table(Cont_table_responderratio)

Fisher_test_test <- rstatix::pairwise_fisher_test(Cont_table_responderratio[2:6], detailed = T)


#Fisher_responderratio
Corrected_p_values_ratio_fisher <- tibble()

i = 1
for (i in 1:nrow(Cont_table)) {
  
  Fisher_table <- Cont_table_responderratio %>% 
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

