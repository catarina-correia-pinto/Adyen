# Load libraries
  
options(scipen = 999)
library(tidyverse)
library(MASS)
library(tidyr)
library(ggplot2)
library(reshape2)

adyen_colors <- c('#0ABF53','#0088FF','#FFC200','#00112C','#DE3686','#919191')

#Looker Links
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/cases_pii?qid=PnoCFWES0W5uBhd1p7AqTQ&toggle=fil

# Read and Load data ----

local_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
csat_data <- read_csv(paste0(local_folder,"/tech_support_csat.csv"))

model_data <- csat_data %>% 
  filter(!is.na(`Case Question 1`)) %>% 
  mutate(bad_reopen = as.numeric(gsub("%", "", `Case Bad Reopen Rate`))/100,
         good_reopen = as.numeric(gsub("%", "", `Case Good Reopen Rate`))/100) %>% 
  rename(csat = `Case Question 1`,
         reply_time = `Case Average first reply time (hrs) - Business Hours`,
         wait_time = `Case Average customer waiting time (hrs) - Business Hours`,
         replies = `Case Average Agent Replies`) %>% 
  select(-`Case Case Number`,-`Case Bad Reopen Rate`,-`Case Good Reopen Rate`)

# Proportional Odds Model ----

model <- polr(as.factor(csat) ~ reply_time + wait_time + replies + bad_reopen + good_reopen,
              data = model_data,
              method = "logistic")

summary(model)

# Effect chart

coef_data <- data.frame(
  Predictor = names(coef(model)),  
  Coefficient = coef(model)        
)

ggplot(coef_data, aes(x = reorder(Predictor, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "#0abf53"), labels = c("Negative", "Positive")) +
  labs(x = "Metric", y = "Coefficient") +
  coord_flip() +
  theme_minimal() +theme(
    legend.position = "none", 
    panel.grid = element_blank()  
  )

# Testing

new_data <- data.frame(
  # reply_time = seq(min(model_data$reply_time), max(model_data$reply_time), length.out = 100),  
  wait_time = seq(min(model_data$wait_time), max(model_data$wait_time), length.out = 100),  
  reply_time = mean(model_data$reply_time),  
  # wait_time = mean(model_data$wait_time),  
  replies = mean(model_data$replies),  
  bad_reopen = mean(model_data$bad_reopen),  
  good_reopen = mean(model_data$good_reopen) 
)

predicted_probs <- as.data.frame(predict(model, newdata = new_data, type = "probs"))
predicted_probs$wait_time <- new_data$wait_time

predicted_probs_long <- pivot_longer(predicted_probs, 
                                     cols = -wait_time, 
                                     names_to = "CSAT", 
                                     values_to = "Probability")

ggplot(predicted_probs_long, aes(x = wait_time, y = Probability, color = CSAT)) +
  geom_line(size = 1) +
  labs(x = "Reply Time (hours)", y = "Probability", title = "Predicted Probabilities of CSAT Levels by Reply Time") +
  theme_minimal()
