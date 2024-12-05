source("Scripts/Data_read_in.R")
library(rstatix)
library("effectsize")

Tests_names <- Metcalfa_behavior_data %>% 
  group_by(Test) %>%
  summarise() %>% 
  filter (Test != c("DMNT piperiton", "piperiton MeSa"))

Cont_table_base <- Metcalfa_behavior_data %>%
  filter(!(Test %in% c("DMNT piperiton", "piperiton MeSa"))) %>% 
  mutate(is_blank_blank = ifelse(Test == "blank blank", TRUE, FALSE)) %>% 
  group_by(Test) 

Starters_nonStarters <- Cont_table_base%>% 
  summarise(Starters = sum(Result != "N"),
            Non_starters = sum(Result == "N"))

Undiceded_Responders <- Cont_table_base %>% 
  summarise(Responders = ifelse(any(is_blank_blank), 
                                sum(Result %in% c("L", "R")), 
                                sum(Result %in% c("DMNT", "camphor", "piperiton", "MeSa", "blank"))),
            Undecided = sum(Result == "U"))

Kaki <-  Cont_table_base %>%
  filter(Result != "N", Test == "camphor piperiton") %>%
  summarise(Yes_piperiton_Count = sum(Result == "piperiton", na.rm = TRUE),
            No_camphor_Count = sum(Result == "camphor", na.rm = TRUE))

Choosed_compound <- Cont_table_base %>%
  filter(Result != "N", Test != "camphor piperiton") %>% 
  mutate(Is_Yes = Choosed_the_compound == "Yes",
         Is_No = Choosed_the_compound == "No") %>% 
  summarise(Yes_piperiton_Count = sum(Is_Yes, na.rm = TRUE),
            No_camphor_Count = sum(Is_No, na.rm = TRUE)) %>% 
  bind_rows(Kaki)


# # Cont_table_transposed <- Cont_table %>% 
# #   column_to_rownames("Test") %>% 
# #   rownames_to_column() %>% 
# #   gather(variable, value, -rowname) %>% 
# #   spread(rowname, value)

#Chi_square
#ORs

#blankblank: Starters:45, Nonsterters:8
Corrected_p_values_chi_St_NSt <- tibble()
BlankBlank_St <- Starters_nonStarters %>% 
  filter(Test == "blank blank") %>% 
  column_to_rownames("Test")

b = 1
for (b in 1:nrow(Tests_names)) {
  
  Chi_square_table <- Starters_nonStarters %>% 
    filter(Test == Tests_names$Test[b]) %>% 
    column_to_rownames("Test")
  
  Chi_square_test <- chisq_test(Chi_square_table, p = c(45/53, 8/53)) %>% 
    add_column(.before = "n", Test = Tests_names$Test[b])
  
  OR_Starters <- Chi_square_table %>% 
    bind_rows(BlankBlank_St)
  
  ORs <- ((OR_Starters[1,1]/OR_Starters[1,2])/OR_Starters[2,1]/OR_Starters[2,2])
  
  
  Corrected_p_values_chi_St_NSt <- Corrected_p_values_chi_St_NSt %>% 
    bind_rows(Chi_square_test)
  
  print(chisq_descriptives(chisq_test(Chi_square_table)))
  
}

Corrected_p_values_chi_St_NSt <- Corrected_p_values_chi_St_NSt %>% 
  filter (Test != "blank blank") %>% 
  adjust_pvalue(p.col = "p", method = "fdr") %>%
  add_significance()



#blankblank: Undiceded: 21 Responders: 24
Corrected_p_values_chi_U_R <- tibble()

b = 1
for (b in 1:nrow(Tests_names)) {
  
  Chi_square_table <- Undiceded_Responders %>% 
    filter(Test == Tests_names$Test[b]
    ) %>% 
    column_to_rownames("Test")
  
  Chi_square_test <- chisq_test(Chi_square_table, p = c(21/45, 24/45)) %>% 
    add_column(.before = "n", Test = Tests_names$Test[b])
  
  Corrected_p_values_chi_U_R <- Corrected_p_values_chi_U_R %>% 
    bind_rows(Chi_square_test)
  
  print(chisq_descriptives(chisq_test(Chi_square_table)))
  
}

Corrected_p_values_chi_U_R <- Corrected_p_values_chi_U_R %>% 
  filter (Test != "blank blank") %>% 
  adjust_pvalue(p.col = "p", method = "fdr") %>%
  add_significance()







#blankblank: L:12 R:12
Corrected_p_values_chi_compound <- tibble()

b = 1
for (b in 1:nrow(Tests_names)) {
  
  Chi_square_table <- Choosed_compound %>% 
    filter(Test == Tests_names$Test[b]
    ) %>% 
    column_to_rownames("Test")
  
  Chi_square_test <- chisq_test(Chi_square_table, p = c(12/24, 12/24)) %>% 
    add_column(.before = "n", Test = Tests_names$Test[b])
  
  Corrected_p_values_chi_compound <- Corrected_p_values_chi_compound %>% 
    bind_rows(Chi_square_test)
  
  print(chisq_descriptives(chisq_test(Chi_square_table)))
  
}

Corrected_p_values_chi_compound <- Corrected_p_values_chi_compound %>% 
  filter (Test != "blank blank") %>% 
  adjust_pvalue(p.col = "p", method = "fdr") %>%
  add_significance()



#Odds rations
#Starters_NonStarters
b = 1
for (b in 1:nrow(Tests_names)) {
  Chi_square_table <- Starters_nonStarters %>% 
  filter(Test == Tests_names$Test[b]) %>% 
  column_to_rownames("Test")
  
  
}

