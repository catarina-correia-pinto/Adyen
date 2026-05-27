# Load libraries

options(scipen = 999)
set.seed(1)
library(tidyverse)
library(CausalImpact)
library(zoo)

#Looker Links
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/agent_actions?qid=Qx3jsU7KErXvDocvLuO9Ct&origin_space=8161
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/agent_actions?qid=Vgmn02211dTK5VikJCnDO1&origin_space=8161&toggle=vse
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/agent_actions?qid=hcRj9uPAOpbR1AucHDzpEV&origin_space=8161&toggle=vse

# Read and Load data ----

local_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
# nov13 <- read_csv(paste0(local_folder,"/email_summary_nov13.csv"))
# nov13_agents <- read_csv(paste0(local_folder,"/nov13_agents.csv"))
# nov15_agents <- read_csv(paste0(local_folder,"/nov15_agents.csv"))
dec06_agents <- read_csv(paste0(local_folder,"/dec06_agents.csv"))

agents <- dec06_agents

summary(agents)

clean_data <- agents %>% 
  rename(date = `Recorded at (local) -  Date`,
         handle_time = `Average Session Time (mins)`,
         total_handle_time = `Total Session Time (mins)`,
         cases_handled = `Total Cases with Handle Time`) %>% 
  mutate(avg_handle_time = total_handle_time / cases_handled) %>% 
  select(date,handle_time,avg_handle_time)

team_check <- agents %>% 
  group_by(`Agent Email`) %>% 
  summarise(count = n())

team_data <- agents %>% 
  mutate(team = case_when(`Agent Subdomain` == 'Global Operational Support' ~ 'Ops',
                          `Agent Subdomain` == 'Global Technical Support' & `Agent Team` == 'Account Setup Operations' ~ 'ASO',
                          `Agent Subdomain` == 'Global Technical Support' ~ 'Tech')) %>% 
  rename(date = `Action at Local -  Date`,
         handle_time = `Average Session Time (mins)`) %>% 
  filter(!(`Agent Email` %in% c('artturi.antila@adyen.com','julia.jagtman@adyen.com'))) %>% 
  group_by(date,team) %>% 
  summarise(handle_time = mean(handle_time, na.rm = T)) %>% 
  ungroup() %>% 
  na.omit()

all_data <-  agents %>% 
  filter(!(`Agent Email` %in% c('artturi.antila@adyen.com','julia.jagtman@adyen.com'))) %>%
  rename(date = `Recorded at (local) -  Date`,
         handle_time = `Average Session Time (mins)`,
         total_handle_time = `Total Session Time (mins)`,
         cases_handled = `Total Cases with Handle Time`) %>% 
  mutate(avg_handle_time = total_handle_time / cases_handled) %>% 
  select(date,handle_time,avg_handle_time) %>% 
  group_by(date) %>% 
  # summarise(handle_time = mean(handle_time, na.rm = T)) %>% 
  summarise(handle_time = mean(avg_handle_time, na.rm = T)) %>%
  # filter(date <= '2024-11-15') %>% 
  ungroup() %>% 
  na.omit()
  

# Run model (all data) ----

all_data$date <- as.Date(all_data$date)

model_data <- zoo(all_data$handle_time, order.by = all_data$date)

pre_period <- as.Date(c("2024-09-01", "2024-10-23"))
post_period <- as.Date(c("2024-10-24", "2024-12-05"))

impact <- CausalImpact(data = model_data, 
                       pre.period = pre_period, 
                       post.period = post_period)
plot(impact)
summary(impact)

# Ops Support ----

ops_data <- team_data %>% filter(team == 'Ops') %>% select(-team)
ops_data$date <- as.Date(ops_data$date)
model_data <- zoo(ops_data$handle_time, order.by = ops_data$date)

pre_period <- as.Date(c("2024-09-01", "2024-10-23"))
post_period <- as.Date(c("2024-10-24", "2024-11-12"))

impact <- CausalImpact(data = model_data, 
                       pre.period = pre_period, 
                       post.period = post_period)
plot(impact)
summary(impact)

# ASO ----

aso_data <- team_data %>% filter(team == 'ASO') %>% select(-team)
aso_data$date <- as.Date(aso_data$date)
model_data <- zoo(aso_data$handle_time, order.by = aso_data$date)

pre_period <- as.Date(c("2024-09-01", "2024-10-23"))
post_period <- as.Date(c("2024-10-24", "2024-11-12"))

impact <- CausalImpact(data = model_data, 
                       pre.period = pre_period, 
                       post.period = post_period)
plot(impact)
summary(impact)

# Tech ----

tech_data <- team_data %>% filter(team == 'Tech') %>% select(-team) 
tech_data$date <- as.Date(tech_data$date)
model_data <- zoo(tech_data$handle_time, order.by = tech_data$date)

pre_period <- as.Date(c("2024-09-01", "2024-10-23"))
post_period <- as.Date(c("2024-10-24", "2024-11-12"))

impact <- CausalImpact(data = model_data, 
                       pre.period = pre_period, 
                       post.period = post_period)
plot(impact)
summary(impact)

# Individual agents ----

all_data <-  agents %>% 
  rename(date = `Recorded at (local) -  Date`,
         handle_time = `Average Session Time (mins)`,
         total_handle_time = `Total Session Time (mins)`,
         cases_handled = `Total Cases with Handle Time`,
         agent_email = `Agent Email`) %>% 
  mutate(avg_handle_time = total_handle_time / cases_handled) %>% 
  filter(!agent_email %in% c('patryk.jachimowski@adyen.com','sonny.luu@adyen.com')) %>% 
  na.omit()

agent_emails <- unique(all_data$agent_email)
all_data$date <- as.Date(all_data$date)

results_df <- data.frame(
  agent_email = character(),
  actual = numeric(),
  pred = numeric(),
  effect = numeric(),
  pvalue = numeric(),
  prob = numeric(),
  stringsAsFactors = FALSE
)

for (email in agent_emails) {
  agent_data <- subset(all_data, agent_email == email)
  
  if (nrow(agent_data) == 0) {
    next  
  }
  
  model_data <- zoo(agent_data$avg_handle_time, order.by = agent_data$date)
  
  data_start_date <- min(agent_data$date, na.rm = TRUE)
  data_end_date <- max(agent_data$date, na.rm = TRUE)
  
  pre_period <- as.Date(c("2024-09-01", "2024-10-23"))
  post_period <- as.Date(c("2024-10-24", "2024-12-05"))
  
  pre_period[1] <- max(pre_period[1], data_start_date)  
  pre_period[2] <- min(pre_period[2], data_end_date)    
  post_period[1] <- max(post_period[1], data_start_date)  
  post_period[2] <- min(post_period[2], data_end_date) 
  
  if (pre_period[1] >= pre_period[2] || post_period[1] >= post_period[2]) {
    next  
  }
  
  impact <- CausalImpact(data = model_data, 
                         pre.period = pre_period, 
                         post.period = post_period)
  
  actual <- impact$summary$Actual[1]
  pred <- impact$summary$Pred[1]
  effect <- impact$summary$RelEffect[1]
  pvalue <- impact$summary$p[1]
  prob <- 1 - pvalue
  
  results_df <- rbind(results_df, data.frame(
    agent_email = email,
    actual = actual,
    pred = pred,
    effect = effect,
    pvalue = pvalue,
    prob = prob,
    stringsAsFactors = FALSE
  ))
}

