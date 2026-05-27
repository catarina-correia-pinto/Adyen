# Load libraries

options(scipen = 999)
set.seed(1)
library(tidyverse)
library(corrplot)
library(dplyr)
library(caret)
library(relaimpo)

#Looker Links
# https://looker.is.adyen.com/explore/spark_support_tooling/cases?qid=VnBl8MtHl47QBNNJ3F2K2o&toggle=fil
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/cases_pii?qid=poPwV25Nh4watUg5Svdh27

# Read and Load data ----

local_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
ticket_data <- read_csv(paste0(local_folder,"/ticket_details_12dec.csv"))

summary(ticket_data)

Q1 <- quantile(ticket_data$`Case Average handle time (mins)`, 0.25, na.rm = T)  
Q3 <- quantile(ticket_data$`Case Average handle time (mins)`, 0.75, na.rm = T)  
IQR <- Q3 - Q1                  

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- ticket_data[ticket_data$`Case Average handle time (mins)` >= lower_bound & ticket_data$`Case Average handle time (mins)` <= upper_bound, ]

clean_tickets <- filtered_data %>% 
  mutate(resolution = `Case Completed Times`,
         follow_up = `Case Waiting for Merchant Times`,
         req_help = `Case Waiting for 3rd Party Times`,
         handover = `Case Case Owner Times`,
         email = `Case Average Agent Replies`,
         handle_time = `Case Average handle time (mins)`,
         internal_notes = `Case Avg Internal Notes`,
         case_id = `Case Case ID`) %>%
  dplyr::select(case_id,handle_time,resolution,follow_up,req_help,handover,email,internal_notes) %>% 
  na.omit()

summary(clean_tickets)

# Exploration ----

p <- 
  ggplot(clean_tickets, aes(x = handle_time)) +
  geom_histogram(binwidth = 2, fill = "#0abf53", color = "black") +
  labs(x = "Handle Time (mins)", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, max(clean_tickets$handle_time), by = 5)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/histogram_with_background.png"), 
       plot = p, width = 4, height = 2, dpi = 300, bg = "transparent")


p2 <- 
  ggplot(clean_tickets, aes(x = '', y = handle_time)) +
  geom_violin(fill = "#0abf53", color = "black") +
  geom_boxplot(width = 0.1, fill = "transparent", color = "black") + 
  labs(x = '', y = "Handle Time (mins)") +
  scale_y_continuous(breaks = seq(0, max(clean_tickets$handle_time), by = 5)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/violing_with_background.png"), 
       plot = p2, width = 4, height = 2, dpi = 300, bg = "transparent")

cor_matrix <- cor(clean_tickets %>% dplyr::select(handle_time,resolution,follow_up,req_help,handover,email))
png(filename = paste0(local_folder, "/correlation_matrix_plot.png"),
    width = 1900, height = 950, res = 300, bg = "transparent")
corrplot(cor_matrix, method = "number", type = "upper",tl.col = "white")
dev.off()

hr <- 
  ggplot(clean_tickets, aes(x = resolution, y = handle_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Handle Time vs. Resolution", x = "Number of Resolutions", y = "Handle Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/handle_resolution.png"), 
       plot = hr, width = 4, height = 2, dpi = 300, bg = "transparent")

he <- 
  ggplot(clean_tickets, aes(x = email, y = handle_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Handle Time vs. Email", x = "Number of Emails", y = "Handle Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/handle_email.png"), 
       plot = he, width = 4, height = 2, dpi = 300, bg = "transparent")

hf <- 
  ggplot(clean_tickets, aes(x = follow_up, y = handle_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Handle Time vs. Follow Up", x = "Number of Follow ups", y = "Handle Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/handle_follow.png"), 
       plot = hf, width = 4, height = 2, dpi = 300, bg = "transparent")

hh <- 
  ggplot(clean_tickets, aes(x = handover, y = handle_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Handle Time vs. Handover", x = "Number of Handovers", y = "Handle Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/handle_handover.png"), 
       plot = hh, width = 4, height = 2, dpi = 300, bg = "transparent")

hhp <- 
  ggplot(clean_tickets, aes(x = req_help, y = handle_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Handle Time vs. Request for help", x = "Number of Help Requests", y = "Handle Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),         
        axis.title = element_text(color = "white"),       
        plot.title = element_text(color = "white"))

ggsave(paste0(local_folder,"/handle_help.png"), 
       plot = hhp, width = 4, height = 2, dpi = 300, bg = "transparent")

# Model ----

model <- lm(handle_time ~ resolution + follow_up + req_help + handover + email + internal_notes,
            data = clean_tickets)
summary(model)

rel_imp <- calc.relimp(model, type = "lmg", rela = TRUE)
print(rel_imp)

rel_imp_scores <- rel_imp$lmg * 10

coefficients_df <- as.data.frame(model$coefficients)
coefficients_df <- coefficients_df[!rownames(coefficients_df) %in% "(Intercept)", , drop = FALSE]

result_df <- cbind(
  data.frame(
  Variable = rownames(coefficients_df),
  Coefficient = coefficients_df[, 1]),
  data.frame(rel_imp_scores)) %>% 
  mutate(weight = Coefficient*rel_imp_scores/10) %>% 
  mutate(weight_2 = ceiling(round(weight,2)*10)/10)
