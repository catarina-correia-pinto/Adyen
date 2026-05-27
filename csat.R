# Load libraries ----

options(scipen = 999)
library(tidyverse)
library(scales)
library(corrplot)

adyen_colors <- c('#0ABF53','#0088FF','#FFC200','#00112C','#DE3686','#919191')

#Looker Links ----

# https://looker.is.adyen.com/explore/spark_support_tooling_pii/cases_pii?qid=rcpGVJKbd4aAoVJqNiIJnb&toggle=fil
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/tickets_pii?qid=uJYfJgjkGMXvgyk7oyt4Dg&toggle=fil

# Read and Load data ----

local_folder <- dirname(rstudioapi::getSourceEditorContext()$path)

# csat_data <- read_csv(paste0(local_folder,"/CSAT_17_01_2024.csv"))
# csat_data <- read_csv(paste0(local_folder,"/CSAT_26_03_2024.csv"))
csat_data <- read_csv(paste0(local_folder,"/CSAT_04_04_2024.csv"))
csat_data <- csat_data[-c(1, 2), ]
csat_data %>% str
summary(csat_data)

# sf_data <- read_csv(paste0(local_folder,"/salesforce_cases.csv"))
# sf_data <- read_csv(paste0(local_folder,"/salesforce_cases_26_03_2024.csv"))
sf_data <- read_csv(paste0(local_folder,"/salesforce_cases_04_04_2024.csv"))
sf_data %>% str
summary(sf_data)

# zd_data <- read_csv(paste0(local_folder,"/zd_data.csv"))
zd_data <- read_csv(paste0(local_folder,"/zd_data_26_03_2024.csv"))
zd_data %>% str
summary(zd_data)

# Data transformation ----

clean_csat <- csat_data %>% 
  mutate(survey_time = as_datetime(StartDate),
         survey_date = date(StartDate),
         survey_month = month(StartDate),
         survey_year = year(StartDate)) %>% 
  select(survey_time,survey_date, survey_month, survey_year, TicketID, Q1, Q2, Q3, Q4, Q5, Q6) %>% 
  filter(nchar(TicketID) > 2,
         TicketID != '{{ticket.id}}') %>% 
  rename(case_id = TicketID) %>% 
  group_by(case_id) %>% 
  arrange(survey_time) %>% 
  mutate(row_n = row_number()) %>% 
  filter(row_n == 1) %>% 
  select(- row_n, - survey_time) %>% 
  ungroup() %>% 
  unique()

clean_sf <- sf_data %>% 
  mutate(case_id = substr(`Case Case ID`,1,15)) %>% 
  rename(create_date = `Case Case Created at - Date`,
         closed_date = `Case Case Closed at - Date`,
         user_domain = `Case Case Owner - Domain`,
         user_subdomain = `Case Case Owner - Subdomain`,
         user_team = `Case Case Owner - Team`,
         user_subteam = `Case Case Owner - Subteam`,
         user_region = `Case Case Owner - Region`,
         queue_domain = `Case Case Queue - Domain`,
         queue_subdomain = `Case Case Queue - Subdomain`,
         queue_team = `Case Case Queue - Team`,
         sf_name = `Account Account Name - Salesforce`,
         bo_name = `Account Account Name - Backoffice`,
         segment = `Account Account Customer Segmentation`,
         vertical = `Account Account Vertical`,
         pillar_sf = `Account Account Pillar - Salesforce`,
         pillar_fin = `Account Account Pillar - Finance`,
         replies = `Case Average Agent Replies`,
         reply_time = `Case Average first reply time (hrs)`,
         waiting_time = `Case Average customer waiting time (hrs)`) %>% 
  mutate(create_month = month(create_date),
         create_year = year(create_date),
         closed_month = month(closed_date),
         closed_year = year(closed_date)) %>% 
  select(- `Case Case Type`,
         - `Case Case Subtype`,
         - `Case Case Topic`,
         - `Case Case ID`) %>% 
  unique()

clean_zd <- zd_data %>% 
  rename(case_id = `Case Case ID`,
         create_date = `Case Case Created at - Date`,
         closed_date = `Case Case Closed at - Date`,
         user_domain = `Case Case Owner - Domain`,
         user_subdomain = `Case Case Owner - Subdomain`,
         user_team = `Case Case Owner - Team`,
         user_subteam = `Case Case Owner - Subteam`,
         user_region = `Case Case Owner - Region`,
         queue_domain = `Case Case Queue - Domain`,
         queue_subdomain = `Case Case Queue - Subdomain`,
         queue_team = `Case Case Queue - Team`,
         sf_name = `Account Account Name - Salesforce`,
         bo_name = `Account Account Name - Backoffice`,
         segment = `Account Account Customer Segmentation`,
         vertical = `Account Account Vertical`,
         pillar_sf = `Account Account Pillar - Salesforce`,
         pillar_fin = `Account Account Pillar - Finance`,
         replies = `Case Average Agent Replies`,
         reply_time = `Case Average first reply time (hrs)`,
         waiting_time = `Case Average customer waiting time (hrs)`) %>% 
  mutate(create_month = month(create_date),
         create_year = year(create_date),
         closed_month = month(closed_date),
         closed_year = year(closed_date)) %>% 
  unique()

all_cases <- rbind(clean_zd,clean_sf) %>% unique()

# Join data ----

final_data <- clean_csat %>% 
  left_join(all_cases,  by = "case_id") %>% 
  filter(!is.na(create_date),
         queue_domain == 'Support') %>% 
  mutate(Q1_text = case_when(Q1 == 1 ~ 'Very dissatisfied',
                             Q1 == 2 ~ 'Somewhat dissatisfied',
                             Q1 == 3 ~ 'Neither satisfied nor dissatisfied',
                             Q1 == 4 ~ 'Somewhat satisfied',
                             Q1 == 5 ~ 'Very satisfied')) %>% 
  mutate(question = paste0(Q1," - ",Q1_text)) %>% 
  # filter(survey_date < "2024-03-01") %>% 
  unique()

final_data %>% str

march_2024 <- final_data %>% 
  filter(closed_year == 2024,
         closed_month == 3)

latam <- final_data %>% 
  filter(user_region == 'LATAM',
         closed_year == 2024,
         closed_month == 3)

write_csv(latam,paste0(local_folder,"/latam_csat_03_2024.csv"))

# Warehouse CSAT ----

warehouse <- clean_csat %>% 
  left_join(all_cases,  by = "case_id") %>% 
  filter(!is.na(create_date),
         queue_domain == 'Strategy & Enablement') %>% 
  mutate(Q1_text = case_when(Q1 == 1 ~ 'Very dissatisfied',
                             Q1 == 2 ~ 'Somewhat dissatisfied',
                             Q1 == 3 ~ 'Neither satisfied nor dissatisfied',
                             Q1 == 4 ~ 'Somewhat satisfied',
                             Q1 == 5 ~ 'Very satisfied')) %>% 
  mutate(question = paste0(Q1," - ",Q1_text)) %>% 
  unique()

write_csv(warehouse,paste0(local_folder,"/warehousing_csat.csv"))

# Exploration ----

# Total bar chart
final_data %>% 
  group_by(question) %>% 
  summarise(count = n())  %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = question, y = count, fill = question)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(label = comma(count)), 
            position = position_dodge(width = 0),
            vjust = -2,
            size = 3, 
            color = "black") +
  geom_text(aes(label = sprintf("(%.1f%%)", percentage)), 
            position = position_dodge(width = 1),
            vjust = -0.1,
            size = 3, 
            color = "black") +
  scale_fill_manual(values = adyen_colors) +
  theme_bw() +
  labs(x = 'Responses',
       y = '') +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

# CSAT per month
final_data %>% 
  group_by(survey_month,survey_year,Q1,question) %>% 
  summarise(count = n()) %>%
  group_by(survey_month,survey_year) %>% 
  mutate(total = sum(count)) %>% 
  filter(Q1 == 5 | Q1 == 4) %>% 
  group_by(survey_month,survey_year) %>% 
  mutate(csat = sum(count)) %>% 
  mutate(score = csat / total * 100) %>% 
  select(survey_month,survey_year,score) %>% 
  unique() %>% 
  mutate(month_year = as.Date(paste0('01-', survey_month, '-', survey_year), format = '%d-%m-%Y')) %>% 
  ggplot(aes(x = month_year, y = score)) +
  geom_line() +
  geom_text(aes(label = sprintf("%.1f%%", score)), vjust = -0.5, size = 3) +
  scale_x_date(breaks = scales::pretty_breaks(n = 24)) +
  scale_fill_manual(values = adyen_colors,
                    name = "") +
  expand_limits(y = 50) +
  theme_bw() +
  labs(x = 'Month',
       y = 'CSAT',
       color = '') 

# CSAT per month by team
final_data %>% 
  group_by(survey_month,survey_year,queue_team,Q1,question) %>% 
  summarise(count = n()) %>%
  group_by(survey_month,survey_year,queue_team) %>% 
  mutate(total = sum(count)) %>% 
  filter(Q1 == 5 | Q1 == 4) %>% 
  group_by(survey_month,survey_year,queue_team) %>% 
  mutate(csat = sum(count)) %>% 
  mutate(score = csat / total * 100) %>% 
  select(survey_month,survey_year,queue_team,score) %>% 
  filter(queue_team != 'Platform Monitoring',
         queue_team != 'Platform Monitoring Operations',
         queue_team != 'Support Unclassified',
         !(queue_team == 'Disputes' & survey_year == 2022)) %>% 
  unique() %>% 
  mutate(month_year = as.Date(paste0('01-', survey_month, '-', survey_year), format = '%d-%m-%Y')) %>% 
  ggplot(aes(x = month_year, y = score, color = queue_team)) +
  geom_line() +
  geom_text(aes(label = sprintf("%.1f%%", score), color = queue_team), vjust = -0.5, size = 3) +
  scale_x_date(breaks = scales::pretty_breaks(n = 24)) +
  scale_fill_manual(values = adyen_colors,
                    name = "") +
  expand_limits(y = 50) +
  theme_bw() +
  labs(x = 'Month',
       y = 'CSAT',
       color = '') +
  theme(legend.position = "bottom")

# CSAT per month by segment
final_data %>% 
  group_by(survey_month,survey_year,segment,Q1,question) %>% 
  summarise(count = n()) %>%
  group_by(survey_month,survey_year,segment) %>% 
  mutate(total = sum(count)) %>% 
  filter(Q1 == 5 | Q1 == 4) %>% 
  group_by(survey_month,survey_year,segment) %>% 
  mutate(csat = sum(count)) %>% 
  mutate(score = csat / total * 100) %>% 
  select(survey_month,survey_year,segment,score) %>% 
  filter(segment == 'Focus Account' | segment == 'Key Account' | segment == 'Top Account') %>% 
  unique() %>% 
  mutate(month_year = as.Date(paste0('01-', survey_month, '-', survey_year), format = '%d-%m-%Y')) %>% 
  ggplot(aes(x = month_year, y = score, color = segment)) +
  geom_line() +
  geom_text(aes(label = sprintf("%.1f%%", score), color = segment), vjust = -0.5, size = 3) +
  scale_x_date(breaks = scales::pretty_breaks(n = 24)) +
  scale_fill_manual(values = adyen_colors,
                    name = "") +
  expand_limits(y = 60) +
  theme_bw() +
  labs(x = 'Month',
       y = 'CSAT',
       color = '') +
  theme(legend.position = "bottom")

# Reply time vs score
final_data %>% 
  group_by(Q1) %>% 
  summarise(avg_reply_time = mean(reply_time, na.rm = TRUE),
            med_reply_time = median(reply_time, na.rm = TRUE)) %>%
  gather(key = "metric", value = "value", -Q1) %>% 
  ggplot(aes(x = Q1, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.7, color = "white") +
  geom_text(aes(label = round(value,1)),
            position = position_dodge(width = 0.7),
            vjust = -0.3,
            size = 4) +
  labs(x = "Please rate your overall experience with Adyen Support",
       y = "1st Reply Time (hrs)") +
  scale_fill_manual(values = adyen_colors,
                    name = "",
                    labels = c("Average Reply Time (hrs)", "Median Reply Time (hrs)")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box = "horizontal")

# Waiting time vs. score
final_data %>% 
  group_by(Q1) %>% 
  summarise(avg_waiting_time = mean(waiting_time, na.rm = TRUE),
            med_waiting_time = median(waiting_time, na.rm = TRUE)) %>%
  gather(key = "metric", value = "value", -Q1) %>% 
  ggplot(aes(x = Q1, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.7, color = "white") +
  geom_text(aes(label = round(value,1)),
            position = position_dodge(width = 0.7),
            vjust = -0.3,
            size = 4) +
  labs(x = "Please rate your overall experience with Adyen Support",
       y = "Customer Waiting Time (hrs)") +
  scale_fill_manual(values = adyen_colors,
                    name = "",
                    labels = c("Average Customer Waiting Time (hrs)", "Median Customer Waiting Time (hrs)")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box = "horizontal")

# Agent replies vs score
final_data %>% 
  group_by(Q1) %>% 
  summarise(avg_replies = mean(replies, na.rm = TRUE),
            med_replies = median(replies, na.rm = TRUE)) %>%
  gather(key = "metric", value = "value", -Q1) %>% 
  ggplot(aes(x = Q1, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.7, color = "white") +
  geom_text(aes(label = round(value,1)),
            position = position_dodge(width = 0.7),
            vjust = -0.3,
            size = 4) +
  labs(x = "Please rate your overall experience with Adyen Support",
       y = "Agent Replies") +
  scale_fill_manual(values = adyen_colors,
                    name = "",
                    labels = c("Average Agent Replies", "Median Agent Replies")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

# Boxplot customer wait
final_data %>%
  filter(segment == 'Focus Account') %>% 
  ggplot(aes(x = factor(Q1), y = waiting_time, fill = factor(Q1))) +  
  geom_boxplot(alpha = 0.5) +
  labs(y = "Customer Wait Time (hrs)", 
       x = "Please rate your overall experience with Adyen Support") +  
  theme_minimal() +
  coord_flip() + 
  coord_cartesian(ylim = c(0, 200)) +  
  scale_fill_manual(values = adyen_colors, name = 'Score') +
  scale_x_discrete(labels = levels(factor(final_data$Q1))) +
  theme(legend.position = "none")

# Boxplot reply time
final_data %>%
  filter(segment == 'Focus Account') %>% 
  ggplot(aes(x = factor(Q1), y = reply_time, fill = factor(Q1))) +  
  geom_boxplot(alpha = 0.5) +
  labs(y = "1st Reply Time (hrs)", 
       x = "Please rate your overall experience with Adyen Support") +  
  theme_minimal() +
  coord_flip() + 
  coord_cartesian(ylim = c(0, 100)) +  
  scale_fill_manual(values = adyen_colors, name = 'Score') +
  scale_x_discrete(labels = levels(factor(final_data$Q1))) +
  theme(legend.position = "none")

test <- final_data %>% 
  filter(queue_team == 'Account Setup Operations') %>% 
  select(Q1, reply_time, waiting_time, replies) %>% 
  mutate_all(as.numeric) %>% 
  na.omit()

corrplot(cor(test),method = "number", tl.col = 'black')

