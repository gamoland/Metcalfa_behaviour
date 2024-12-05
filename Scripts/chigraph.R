source("Scripts/Data_read_in.R")
library(rstatix)
library(patchwork)

#graph
Graph_data_prev <- Metcalfa_behavior_data %>%
  filter(!(Test %in% c("DMNT piperiton", "piperiton MeSa", "camphor piperiton"))) %>%
  mutate(is_blank_blank = ifelse(Test == "blank blank", TRUE, FALSE)) %>%
  group_by(Test, VC1, VC2) %>%
  summarise(
    VC1_count = ifelse(any(is_blank_blank), 
                       sum(as.character(Result) == "L"), 
                       sum(as.character(Result) == as.character(VC1))),
    VC2_count = ifelse(any(is_blank_blank), 
                       sum(as.character(Result) == "R"), 
                       sum(as.character(Result) == as.character(VC2))),
    U_count = sum(Result == "U")
  ) %>% 
  mutate(Test = fct_reorder(Test, VC1_count),
         Percent_VC2_count = (VC2_count*100)/(VC2_count + VC1_count),
         Percent_VC1_count = (VC1_count*100)/(VC2_count + VC1_count),
         VC2_count = -VC2_count,
         Percent_VC2_count = -Percent_VC2_count)

Graph_data <- Graph_data_prev %>% 
  pivot_longer(cols = c("VC1_count", "VC2_count", "U_count"), names_to = "Side", values_to = "Number")
Graph_data_percent <- Graph_data_prev %>% 
  pivot_longer(cols = c("Percent_VC2_count", "Percent_VC1_count", "U_count"), names_to = "Side", values_to = "Percent")

desired_order <- c( "blank piperiton","blank camphor","blank MeSa", "blank DMNT","blank blank")
# Create the plot
kakkantas <- Graph_data %>%
  filter(Side != "U_count") %>%
  mutate(Test = factor(Test, levels = desired_order)) %>%
  ggplot(aes(x = Number, y = Test, fill = Side)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.7) +
  coord_cartesian(xlim = c(-max(abs(Graph_data$Number)), 
                           max(abs(Graph_data$Number)))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  # labs(x = "Number", y = "Test", fill = "Side") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = seq(-max(abs(Graph_data$Number)), 
                                  max(abs(Graph_data$Number)), 
                                  by = 1))  # Adjust 'by' for desired spacing


# Create the subtitles as a separate plot
subtitles <- ggplot() +
  annotate("text", x = 0.25, y = 1, label = "blank", size = 5, hjust = -1.75) +
  annotate("text", x = 0.75, y = 1, label = "compound", size = 5, hjust = 1.75) +
  theme_void()

# Combine the subtitles and main plot vertically
final_plot <- subtitles / kakkantas + plot_layout(heights = c(1, 10))  # Adjust heights as needed
final_plot
