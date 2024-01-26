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
  
  Chi_square_test <- chisq_test(Chi_square_table) %>% 
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

#graph

Graph_data <- Metcalfa_behavior_data %>%
  filter(!(Test %in% c("DMNT Piperiton", "Piperiton MeSa"))) %>%
  mutate(is_blank_blank = ifelse(Test == "Blank Blank", TRUE, FALSE)) %>%
  group_by(Test) %>%
  summarise(
    VC1_count = ifelse(any(is_blank_blank), sum(as.character(Result) == "L"), sum(as.character(Result) == as.character(VC1))),
    VC2_count = ifelse(any(is_blank_blank), sum(as.character(Result) == "R"), sum(as.character(Result) == as.character(VC2))),
    U_count = sum(Result == "U")
  ) %>% 
  mutate(VC2_count = -VC2_count,
         Test = fct_reorder(Test, VC1_count)) %>% 
  pivot_longer(cols = c("VC1_count", "VC2_count", "U_count"), names_to = "Side", values_to = "Number")


Graph_data$Test <- factor(Graph_data$Test, levels = Graph_data$Test[order(my_data$order_column)])
Chisqrt_plot <- Graph_data %>% 
  filter(Side != "U_count") %>% 
  ggplot(aes(x = reorder(Test, -Number), y = Number)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "black") +
  geom_blank(aes(y = Number * 1.05)) +
  theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                # panel.grid.major.y = element_line(color = "grey", linetype = "dotted", size = 0.5),
                plot.background = element_rect(fill = "white"),
                strip.background = element_rect(color = "black", fill = "white"),
                panel.background = element_rect(fill = "white", color = "black"),
                panel.spacing = unit(0.7, "lines"))
Chisqrt_plot


