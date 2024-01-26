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
  left_join(Cont_table_2_responders) 
  column_to_rownames("Test") %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

Fisher_test_test <- rstatix::pairwise_fisher_test(Cont_table_responderratio[2:6], detailed = T)

Chi_square_test_ratio <- rstatix::pairwise_chisq_gof_test(Cont_table_responderratio[2:6])

DMNT_U <- c(11,6)
Camphor_U <- c(14,1)
MeSa_U <- c(10,2)
Pip_U <- c(17,2)
valosz_U <- c(3/5 , 2/5)

chisq.test(x = DMNT_U, p = valosz_U)
chisq.test(x = Camphor_U, p = valosz_U)
chisq.test(x = MeSa_U, p = valosz_U)
chisq.test(x = Pip_U, p = valosz_U)

#Fisher_responderratio
Corrected_p_values_ratio_fisher <- tibble()

i = 1
for (i in 1:nrow(Cont_table_responderratio)) {
  
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

