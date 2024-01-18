source("Scripts/Data_read_in.R")
library(rstatix)

Tests_names <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Blank",
         VC1 != "Blank") %>% 
  group_by(Test) %>% 
  summarise()

First_set_camphor <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Blank")

Cont_table_1 <- First_set_camphor %>% 
  group_by(Test) %>% 
  summarise(Choosed_the_acompound = sum(Choosed_the_compound == "Yes"))
  
Cont_table_2 <- First_set_camphor %>% 
  group_by(Test) %>% 
  summarise(
            Choosed_the_blank = sum(Choosed_the_compound == "No"))


Cont_table <- Cont_table_1 %>% 
  left_join(Cont_table_2)

Cont_table_transposed <- Cont_table %>% 
  column_to_rownames("Test") %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

#Fisher_test
Fisher_test_groupwise <- rstatix::pairwise_fisher_test(Cont_table_transposed[2:6], detailed = T)


Corrected_p_values <- tibble()

i = 1
for (i in 1:nrow(Cont_table)) {
  
  Fisher_table <- Cont_table %>% 
    filter(Test == "Blank Blank" | Test == Tests_names$Test[i]
    ) %>% 
    column_to_rownames("Test")
  
  Fisher_test <- fisher_test(Fisher_table,
                             alternative = "two.sided",
                             detailed = T) %>%
    add_significance() %>%
    add_column(.before = "n", Test = Tests_names$Test[i])
  
  Corrected_p_values <- Corrected_p_values %>% 
  bind_rows(Fisher_test)
}

# Result_fisher_1 <- row_wise_fisher_test(Cont_table, p.adjust.method = "fdr")


#Chi_square
Corrected_p_values_chi <- tibble()

b = 1
for (b in 1:nrow(Tests_names)) {
  
  Chi_square_table <- Cont_table %>% 
    filter(Test == Tests_names$Test[b]
    ) %>% 
    column_to_rownames("Test")
  
  Chi_square_test <- pairwise_chisq_gof_test(Chi_square_table) %>% 
    add_column(.before = "n", Test = Tests_names$Test[b])
  
  Corrected_p_values_chi <- Corrected_p_values_chi %>% 
    bind_rows(Chi_square_test)
  
}

Corrected_p_values_chi <- Corrected_p_values_chi %>% 
  adjust_pvalue(p.col = "p", method = "fdr") %>%
  add_significance()

#Chi-square-final!!!!
DMNT <- c(7,4)
Camphor <- c(11,2)
MeSa <- c(6,4)
Pip <- c(14,3)
valosz <- c(1/2 , 1/2)

chisq.test(x = DMNT, p = valosz)
chisq.test(x = Camphor, p = valosz)
chisq.test(x = MeSa, p = valosz)
chisq.test(x = Pip, p = valosz)


#Pip_camphor
Pip_Camphor_1 <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Piperiton",
         VC1 == "Kámfor") %>%
  group_by(Test) %>% 
  summarise(Camphor = sum(Result == "Kámfor"))
Pip_Camphor_2 <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Piperiton",
         VC1 == "Kámfor") %>%
  group_by(Test) %>% 
  summarise(Piperiton = sum(Result == "Piperiton")) 

Pip_Camphor <- Pip_Camphor_2 %>% 
  left_join(Pip_Camphor_1)

