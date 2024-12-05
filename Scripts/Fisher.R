source("Scripts/Data_read_in.R")

Tests_names <- Metcalfa_behavior_data %>% 
  filter(VC2 == "Blank",
         VC1 != "Blank") %>% 
  group_by(Test) %>% 
  summarise()

# 
# #Fisher_test
# Fisher_test_groupwise <- rstatix::pairwise_fisher_test(Cont_table_transposed[2:6], detailed = T)
# 
# 
# Corrected_p_values <- tibble()
# 
# i = 1
# for (i in 1:nrow(Cont_table)) {
#   
#   Fisher_table <- Cont_table %>% 
#     filter(Test == "Blank Blank" | Test == Tests_names$Test[i]
#     ) %>% 
#     column_to_rownames("Test")
#   
#   Fisher_test <- fisher_test(Fisher_table,
#                              alternative = "two.sided",
#                              detailed = T) %>%
#     add_significance() %>%
#     add_column(.before = "n", Test = Tests_names$Test[i])
#   
#   Corrected_p_values <- Corrected_p_values %>% 
#   bind_rows(Fisher_test)
# }
# 
# # Result_fisher_1 <- row_wise_fisher_test(Cont_table, p.adjust.method = "fdr")

