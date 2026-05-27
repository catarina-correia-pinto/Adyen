# Load libraries

options(scipen = 999)
set.seed(1)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(stats)
library(scales)
library(tseries)
library(forecast)
library(prophet)

#Looker Links
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/cases_pii?toggle=fil&qid=AaCY2hw0Ps7KNesCbaZQvr SF historical
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/tickets_pii?toggle=fil&qid=jcKHqOwbYeDHn8hWApSWIJ ZD historical
# https://looker.is.adyen.com/explore/compliance_clone/company_account?qid=jVTwQ3nPnmokJw86LNiMnn&toggle=fil BO historical
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/tickets_pii?toggle=fil&qid=J51jF0xZDzDsBZgB7AzCUy ZD MID
# https://looker.is.adyen.com/explore/spark_support_tooling_pii/tickets_pii?toggle=fil&qid=SRSfRU6VLaRyvTuCNUpHjn ZD calls
# https://looker.is.adyen.com/sql/cfhzskppf6bcb6 company accounts
# https://looker.is.adyen.com/sql/fsgyg4zpp3mwpj country/region mapping

# Read and Load data ----

local_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
sf_df <- read_csv(paste0(local_folder,"/salesforce_historical.csv"))
zd_df <- read_csv(paste0(local_folder,"/zendesk_historical.csv"))
zd_mid <- read_csv(paste0(local_folder,"/zendesk_mid.csv"))
zd_calls <- read_csv(paste0(local_folder,"/zendesk_calls.csv"))
bo_sf <- read_csv(paste0(local_folder,"/backoffice_historical.csv"))
ca_sf <- read_csv(paste0(local_folder,"/company_accounts.csv"))
rg_map <- read_csv(paste0(local_folder,"/countryregionmapping.csv"))

# Account data transformation ----

clean_bo_sf <- bo_sf %>% 
  rename(company_account_code = `Company Account Account Code`,
         company_creation_date = `Company Account Creation Date`,
         company_closure_date = `Company Account Closure Date`,
         country_code = `Company Country Data1`) %>% 
  left_join(rg_map, by = 'country_code') %>% 
  rename(region = adyen_region) %>% 
  mutate(region = case_when(is.na(region) ~ 'NA',
                            TRUE ~ region))

clean_accounts <- ca_sf %>% 
  mutate(pillar_clean = coalesce(pillar,pillar_bmboad,pillar_cbm)) %>% 
  left_join(clean_bo_sf, by = "company_account_code") %>% 
  select(region,country_code,company_account_code,company_creation_date,company_closure_date,first_live_transaction_date,account_creation_date_salesforce,pillar_clean) %>% 
  mutate(company_creation_date = as.Date(company_creation_date),
         company_closure_date = if_else(is.na(company_closure_date), Sys.Date(), as.Date(company_closure_date)),
         first_live_transaction_date = as.Date(first_live_transaction_date),
         account_creation_date_salesforce = as.Date(account_creation_date_salesforce)) %>% 
  filter(!is.na(company_creation_date))

active_accounts <- clean_accounts %>%
  rowwise() %>%
  mutate(month_seq = list(seq(floor_date(company_creation_date, "month"), 
                              floor_date(company_closure_date, "month"), 
                              by = "month"))) %>%
  unnest(month_seq) %>%
  mutate(active = if_else(!is.na(first_live_transaction_date) & month_seq >= floor_date(first_live_transaction_date, "month") & month_seq <= floor_date(company_closure_date, "month"), 1, 0)) %>% 
  mutate(stage = case_when(
    is.na(first_live_transaction_date) ~ NA_character_,
    year(month_seq) == year(first_live_transaction_date) ~ "new",
    year(month_seq) == year(first_live_transaction_date) + 1 ~ "ramping",
    year(month_seq) > year(first_live_transaction_date) + 1 ~ "existing",
    TRUE ~ NA_character_
  ))

length(unique(active_accounts$company_account_code))

open_accounts_summary <- active_accounts %>% 
  group_by(month_seq,region,country_code,pillar_clean,stage) %>%
  summarise(open_number_accounts = n_distinct(company_account_code))

active_accounts_summary <- active_accounts %>% 
  filter(active == 1) %>% 
  group_by(month_seq,region,country_code,pillar_clean,stage) %>% 
  summarise(active_number_accounts = n_distinct(company_account_code))

combine_accounts <- open_accounts_summary %>% left_join(active_accounts_summary, by = c('month_seq','region','country_code','pillar_clean','stage'))

combine_accounts %>%
  ggplot(aes(x = month_seq)) +
  geom_line(aes(y = open_number_accounts, color = "Open Accounts"), size = 1) +
  geom_line(aes(y = active_number_accounts, color = "Active Accounts"), size = 1) +
  labs(title = "Open vs Active Accounts Over Time",
       x = "Month",
       y = "Number of Accounts") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +  
  scale_color_manual(values = c("Open Accounts" = "#4285f4", "Active Accounts" = "#0abf53")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10))

# Ticket data transformation ----

sf_clean <- sf_df %>% 
  rename(month = `Case Case Created at - Month`,
         subdomain = `Case Case Queue - Subdomain`,
         team = `Case Case Queue - Team`,
         squad = `Case Case Queue - Support Team`,
         category = `Case Case Category`,
         origin = `Case Case Origin`,
         hours = `Case Case Created at - Hour of Day`,
         cases = `Case Cases`,
         daily_cases = `Case Daily Created Cases`) %>% 
  mutate(month = as.Date(paste0(month, "-01")))

zd_clean <- left_join(zd_df %>% rename(cases = `Case Cases`, daily_cases = `Case Daily Created Cases`),
                      zd_calls %>% rename(calls = `Case Cases`, daily_calls = `Case Daily Created Cases`)) %>% 
  mutate(new_cases = cases - coalesce(calls,0),
         new_daily_cases = daily_cases - coalesce(daily_calls,0)) %>% 
  select(-cases,-daily_cases) %>% 
  rename(month = `Case Case Created at - Month`,
         subdomain = `Case Case Queue - Subdomain`,
         team = `Case Case Queue - Team`,
         queue = `Case Case Queue`,
         category = `Case Case Category`,
         origin = `Case Case Origin`,
         hours = `Case Case Created at - Hour of Day`,
         cases = new_cases,
         daily_cases = new_daily_cases) %>% 
  mutate(month = as.Date(paste0(month, "-01"))) %>% 
  select(-calls,-daily_calls)

zd_mid_clean <- zd_mid %>% 
  rename(month = `Case Case Created at - Month`,
         subdomain = `Case Case Queue - Subdomain`,
         team = `Case Case Queue - Team`,
         category = `Case Case Category`,
         origin = `Case Case Origin`,
         hours = `Case Case Created at - Hour of Day`,
         cases = `Case Cases`,
         daily_cases = `Case Daily Created Cases`) %>% 
  mutate(month = as.Date(paste0(month, "-01")))

total_tickets <- rbind(sf_clean %>% select(-squad), zd_clean %>% select(-queue))
total_tickets_mid <- rbind(sf_clean %>% select(-squad) %>% filter(origin== 'MID Service'),zd_mid_clean)

non_latam_tickets <- rbind(sf_clean %>% 
                             filter(squad != 'LATAM',
                                    squad != 'Japan & China') %>% 
                             select(-squad),
                           zd_clean %>% 
                             filter(queue != 'Technical Support LATAM') %>% 
                             select(-queue))
  
latam_tickets <- rbind(sf_clean %>% 
                         filter(squad == 'LATAM') %>% 
                         select(-squad),
                       zd_clean %>% 
                         filter(queue == 'Technical Support LATAM') %>% 
                         select(-queue))

latam_accounts <- combine_accounts %>% 
  filter(region == 'LATAM') %>% 
  mutate(month = as.Date(month_seq)) %>% 
  drop_na()

non_latam_accounts <- combine_accounts %>% 
  filter(region != 'LATAM') %>% 
  mutate(month = as.Date(month_seq)) %>% 
  group_by(month) %>% 
  summarise(active_number_accounts = sum(active_number_accounts,na.rm = T))

account_month_summary <- non_latam_accounts %>% 
  group_by(month) %>% 
  summarise(
    # open_number_accounts = sum(open_number_accounts,na.rm = T),
            active_number_accounts = sum(active_number_accounts,na.rm = T))

combined_df <- non_latam_tickets %>% 
  # filter(subdomain == 'Operational Support') %>%
  filter(team == 'Disputes') %>%
  filter(subdomain != 'Platform Operations',
         team != 'Support Unclassified',
         category == 'Internal',
         month < '2025-05-01'
         # , origin == 'Call'
         # , origin == 'MID Service'
         ,!origin %in% c('Side Conversation','Call','MID Service')
         ) %>% 
  group_by(month) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  left_join(account_month_summary, by = 'month')

ticket_split <- total_tickets %>%
  filter(subdomain != 'Platform Operations',
         team != 'Support Unclassified',
         month < '2025-0-01',
         !origin %in% c('Side Conversation','Call')) %>% 
  group_by(month,category) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  spread(key = category, value = cases)

# Exploration plots ----

total_tickets %>% 
  filter(subdomain != 'Platform Operations',
         month < '2025-01-01',
         !origin %in% c('Side Conversation','Call')) %>% 
  group_by(month,category) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  ggplot(aes(x = month, y = cases, fill = category)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
  labs(title = "Total Cases by Month and Category",  
       x = "Month",  
       y = "Number of Cases") +  
  scale_fill_manual(values = c("Internal" = "#0abf53", "Service" = "#4285f4")) + 
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

total_tickets %>% 
  filter(subdomain != 'Platform Operations',
         team != 'Support Unclassified',
         month < '2025-01-01',
         !origin %in% c('Side Conversation','Call')) %>% 
  group_by(month,team) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  ggplot(aes(x = month, y = cases, fill = team)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
  labs(title = "Total Cases by Month and Category",  
       x = "Month",  
       y = "Number of Cases") +  
  scale_fill_manual(values = c("Technical Support" = "#0abf53", "Disputes" = "#4285f4", "Account Setup Operations" = "#FFC200", "Operational Support" = "#DE3686")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

combined_df_test <- combined_df %>%
  mutate(month_num = as.numeric(format(month, "%m")), 
         year = as.numeric(format(month, "%Y"))) %>%  
  group_by(year) %>%      
  mutate(scaled_cases = (cases / max(cases)) * 100) %>%  
  ungroup()  %>% 
  filter(year >= 2018)

ggplot(combined_df_test, aes(x = month_num, y = scaled_cases, group = year, color = as.factor(year))) +
  # geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  
  geom_smooth(aes(group = year), method = "loess", se = FALSE, size = 1) +  # Add smoothed lines
  labs(title = "Monthly Scaled Cases by Year (0 to 100)",
       x = "Month",
       y = "Scaled Number of Cases",
       color = "Year") +  
  theme_minimal()

# Correlation plots ----

combined_df %>%
  mutate(contact_rate = cases/active_number_accounts) %>% 
  ggplot(aes(x = month)) +
  geom_line(aes(y = open_number_accounts, color = "Open Accounts"), size = 1) +
  geom_line(aes(y = active_number_accounts, color = "Active Accounts"), size = 1) +
  geom_bar(aes(y = cases), stat = "identity", fill = "darkgrey", alpha = 0.5) +  
  labs(title = "Service Tickets vs Accounts Over Time",
       x = "Month",
       y = "") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_y_continuous(breaks = seq(from = floor(min(combined_df$cases, na.rm = TRUE) / 1000) * 1000,
                                  to = ceiling(max(combined_df$cases, na.rm = TRUE) / 1000) * 1000,  
                                  by = 5000)) +
  scale_color_manual(values = c("Open Accounts" = "#4285f4", "Active Accounts" = "#0abf53")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10))


plot_ly(combined_df, x = ~month) %>%
  add_lines(y = ~open_number_accounts, name = "Open Accounts", line = list(color = "#4285f4", width = 2)) %>%
  add_lines(y = ~active_number_accounts, name = "Active Accounts", line = list(color = "#0abf53", width = 2)) %>%
  add_bars(y = ~cases, name = "Cases", marker = list(color = "darkgrey", opacity = 0.5)) %>%
  layout(title = "Tickets vs Accounts Over Time",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Number of Accounts"),
         yaxis2 = list(title = "Cases", overlaying = "y", side = "right"),
         showlegend = TRUE)

correlation_value <- cor(combined_df$active_number_accounts, combined_df$cases, use = "complete.obs")
r_squared <- correlation_value^2

combined_df %>%
  ggplot(aes(x = active_number_accounts, y = cases)) +
  geom_point(color = "#4285f4", alpha = 0.7, size = 3) +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  
  scale_x_continuous(breaks = seq(from = floor(min(combined_df$active_number_accounts, na.rm = TRUE) / 1000) * 1000,
                                  to = ceiling(max(combined_df$active_number_accounts, na.rm = TRUE) / 1000) * 1000,  
                                  by = 1000)) +
  labs(title = paste("Active Accounts vs Internal Tickets (Correlation:", round(correlation_value, 2), ") (R²:", round(r_squared, 2), ")"),
       subtitle = "(Ops & Tech, excluding Calls & MID)",
       x = "Number of Active Accounts",
       y = "Number of Internal Tickets") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

correlation_value <- cor(ticket_split$Service, ticket_split$Internal, use = "complete.obs")
r_squared <- correlation_value^2

ticket_split %>%
  ggplot(aes(x = Service, y = Internal)) +
  geom_point(color = "#4285f4", alpha = 0.7, size = 3) +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  
  labs(title = paste("Service vs Internal Tickets (Correlation:", round(correlation_value, 2), ") (R²:", round(r_squared, 2), ")"),
       subtitle = "(Ops & Tech, excluding Calls)",
       x = "Number of Service Tickets",
       y = "Number of Internal Tickets") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Linear model fitting ----

model_data <- combined_df %>%
  select(month,active_number_accounts, cases) %>%
  drop_na()

lm_model <- lm(cases ~ active_number_accounts, data = model_data)
summary(lm_model)

r_squared <- summary(lm_model)$r.squared
print(paste("R-squared:", round(r_squared, 3)))

predicted_values <- predict(lm_model, newdata = model_data, interval = "confidence")

model_data$predicted_cases <- predicted_values[, 1]  # Point estimate
model_data$lower_bound <- predicted_values[, 2]  # Lower bound of the confidence interval
model_data$upper_bound <- predicted_values[, 3]

# ggplot(model_data, aes(x = month)) +
#   geom_col(aes(y = cases, fill = "Actual Cases"), alpha = 0.7) +
#   geom_line(aes(y = predicted_cases, color = "Predicted Cases"), size = 1, linetype = "dashed") +  
#   labs(title = "Predicted vs Actual Service Tickets Over Time",
#        x = "Date",
#        y = "Number of Service Tickets") +
#   scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
#   scale_fill_manual(values = c("Actual Cases" = "#4285f4")) +  
#   scale_color_manual(values = c("Predicted Cases" = "red")) +
#   theme_minimal() +
#   theme(legend.title = element_blank())

ggplot(model_data, aes(x = month)) +
  geom_col(aes(y = cases, fill = "Actual Cases"), alpha = 0.7) + 
  geom_line(aes(y = predicted_cases, color = "Predicted Cases"), size = 1, linetype = "dashed") + 
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), 
              fill = "red", alpha = 0.2, color = NA) +  # Shaded area for prediction intervals
  labs(title = "Predicted vs Actual Service Tickets Over Time",
       x = "Date",
       y = "Number of Service Tickets") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  scale_fill_manual(values = c("Actual Cases" = "#4285f4")) +  
  scale_color_manual(values = c("Predicted Cases" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

new_df <- rbind(sf_clean %>% select(-squad),zd_clean) %>% 
  filter(subdomain != 'Platform Operations',
         category == 'Service',
         !origin %in% c('Side Conversation','Call')) %>% 
  group_by(month) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  left_join(combine_accounts, by = c('month'='month_seq')) 

# Test model for Jan 2025

new_df_jan_2025 <- new_df %>% filter(month == as.Date("2025-01-01"))
predicted_values <- predict(lm_model, newdata = new_df_jan_2025, interval = "prediction")

new_df_jan_2025$predicted_cases <- predicted_values[, "fit"]
new_df_jan_2025$lower_bound <- predicted_values[, "lwr"]
new_df_jan_2025$upper_bound <- predicted_values[, "upr"]

model_data_new <- bind_rows(model_data,new_df_jan_2025 %>% select(month, active_number_accounts, cases, predicted_cases, lower_bound, upper_bound))

model_data_new <- model_data_new %>%
  mutate(
    is_prediction = ifelse(month == as.Date("2025-01-01"), "Actuals (not in model)", "Actuals (in model)"),
    label_cases = ifelse(month >= as.Date("2023-01-01"), format(cases, big.mark = ","), NA),
    label_pred = ifelse(month >= as.Date("2023-01-01"), format(round(predicted_cases, 0), big.mark = ","), NA),
    error = ifelse(month == as.Date("2025-01-01"), round((cases - predicted_cases) / cases * 100, 1), NA)
  ) %>% 
  filter(!is.na(label_cases))

ggplot(model_data_new, aes(x = month)) +
  geom_col(aes(y = cases, fill = is_prediction), alpha = 0.7) +
  geom_line(aes(y = predicted_cases, color = "Predicted Cases"), size = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#0abf53", alpha = 0.2) +
  geom_text(aes(y = cases / 2, label = label_cases), size = 3, color = "black", na.rm = TRUE) +
  geom_text(aes(y = predicted_cases, label = label_pred), vjust = -1, size = 3, color = "#0abf53", na.rm = TRUE) +
  geom_text(aes(y = lower_bound, label = label_comma()(lower_bound)), vjust = -1.5, size = 3, color = "#0abf53", 
            data = subset(model_data_new, month == as.Date("2025-01-01"))) +
  geom_text(aes(y = upper_bound, label = label_comma()(upper_bound)), vjust = 1.5, size = 3, color = "#0abf53", 
            data = subset(model_data_new, month == as.Date("2025-01-01"))) +
  scale_color_manual(values = c("Predicted Cases" = "#0abf53")) +
  scale_fill_manual(values = c("Actuals (in model)" = "#4285f4", "Actuals (not in model)" = "orange")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 months") +
  scale_y_continuous(labels = label_comma()) +  
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10))

# Seasonality ----

combined_df$month <- as.Date(combined_df$month)
ts_cases <- ts(combined_df$cases, start = c(2018, 1), frequency = 12) 
decomposed_cases <- decompose(ts_cases)
plot(decomposed_cases$seasonal)

seasonal_df <- data.frame(
  month = seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = length(decomposed_cases$seasonal)),
  seasonal = decomposed_cases$seasonal) %>% 
  filter(month < '2025-01-01') 

ggplot(seasonal_df, aes(x = month, y = seasonal)) +
  geom_line(color = "#4285f4", size = 1) +
  geom_area(fill = "#4285f4", alpha = 0.3) +
  labs(title = "Seasonal Component of Service Tickets", 
       x = "Month", 
       y = "Seasonal Effect") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 months",  
               date_labels = "%b %Y",  
               limits = c(as.Date("2018-01-01"), max(seasonal_df$month))) 

# Prophet testing ----

test <- combined_df %>% select(month,cases) %>% rename(ds = month, y = cases)
m <- prophet(test)
future <- make_future_dataframe(m, periods = 12, freq = 'month')
forecast <- predict(m, future)

prophet_plot_components(m, forecast)

plot(m, forecast) +
  ggtitle("Tickets Forecast") +
  xlab("Month") +
  ylab("Number of Service Tickets") +
  theme_minimal(base_size = 14) +   # Minimal theme with larger text
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

# Accounts growth ----

combined_df$month <- as.Date(combined_df$month)

latam_accounts <- combine_accounts %>% 
  filter(region == 'LATAM',
         month_seq  < '2025-05-01') %>% 
  mutate(month = as.Date(month_seq)) %>% 
  drop_na()

non_latam_accounts <- combine_accounts %>% 
  filter(region != 'LATAM',
         month_seq  < '2025-05-01') %>% 
  mutate(month = as.Date(month_seq)) %>% 
  group_by(month) %>% 
  summarise(active_number_accounts = sum(active_number_accounts,na.rm = T))

ggplot(combined_df, aes(x = month, y = active_number_accounts)) +
  geom_line() +
  labs(title = "Active Number of Accounts Over Time", x = "Month", y = "Active Accounts") +
  theme_minimal()

ts_accounts <- ts(non_latam_accounts$active_number_accounts, start = c(year(min(non_latam_accounts$month)), month(min(non_latam_accounts$month))), frequency = 12)
log_ts_accounts <- log(ts_accounts + 1)
diff_log_ts_accounts <- diff(log_ts_accounts)

# Test stationary 
adf.test(diff_log_ts_accounts, alternative = "stationary") 

fitARIMA <- auto.arima(log_ts_accounts, seasonal = FALSE, trace = TRUE)
summary(fitARIMA)

future_forecast <- forecast(fitARIMA, h = 12, level = c(95))  

future_months <- seq(from = as.Date(paste0(year(max(non_latam_accounts$month)), "-", month(max(non_latam_accounts$month)), "-01")) + months(1), 
                     by = "1 month", length.out = 12)

forecasted_df <- data.frame(
  month = future_months,
  forecasted_active_accounts = exp(future_forecast$mean) - 1,  
  lower_bound = exp(future_forecast$lower[,1]) - 1,  
  upper_bound = exp(future_forecast$upper[,1]) - 1,
  type = "Forecast"
)

account_growth <- bind_rows(
  combined_df %>% 
    select(month, active_number_accounts) %>% 
    rename(forecasted_active_accounts = active_number_accounts) %>% 
    mutate(lower_bound = NA, upper_bound = NA, type = "Actual"),   
  
  forecasted_df)

ggplot(account_growth, aes(x = month, y = forecasted_active_accounts, color = type)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = account_growth %>% filter(type == "Forecast"), 
              aes(x = month, ymin = lower_bound, ymax = upper_bound), 
              fill = "#0abf53", alpha = 0.2, inherit.aes = FALSE) + 
  labs(title = "Active Accounts: Historical and Forecasted",
       x = "Month",
       y = "Active Accounts") + 
  scale_x_date(date_labels = "%Y-%m", breaks = account_growth$month[month(account_growth$month) %in% c(1, 7)]) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c("Actual" = "#4285f4", "Forecast" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Service tickets 2025 forecast ----

# Linear Regression

model_data <- combined_df %>%
  select(month,active_number_accounts, cases) %>%
  drop_na()

lm_model <- lm(cases ~ active_number_accounts, data = model_data)
summary(lm_model)

r_squared <- summary(lm_model)$r.squared
print(paste("R-squared:", round(r_squared, 3)))

predicted_values <- predict(lm_model, newdata = model_data, interval = "confidence")

model_data$predicted_cases <- predicted_values[, 1]  # Point estimate
model_data$lower_bound <- predicted_values[, 2]  # Lower bound of the confidence interval
model_data$upper_bound <- predicted_values[, 3]

df_2025 <- account_growth %>% 
  filter(month >= '2025-01-01') %>% 
  select(month,forecasted_active_accounts) %>% 
  rename(active_number_accounts = forecasted_active_accounts)

predicted_values_2025 <- predict(lm_model, newdata = df_2025, interval = "prediction")

df_2025$predicted_cases <- predicted_values_2025[, "fit"]
df_2025$lower_bound <- predicted_values_2025[, "lwr"]
df_2025$upper_bound <- predicted_values_2025[, "upr"]

df_2025 <- df_2025 %>% 
  mutate(cases = 0) 

model_data_2025 <- bind_rows(model_data,df_2025 %>% select(month, active_number_accounts, cases, predicted_cases, lower_bound, upper_bound))


# Prophet

full_2025_data <- account_growth %>% 
  select(month,forecasted_active_accounts) %>% 
  rename(active_number_accounts = forecasted_active_accounts) %>% 
  left_join(combined_df)
  
train_data <- full_2025_data %>% 
  select(month,cases,active_number_accounts) %>% 
  rename(ds = month, y = cases)

m <- prophet()
m <- add_regressor(m, 'active_number_accounts')
m <- fit.prophet(m,train_data)

future <- data.frame(ds = full_2025_data$month)
future$active_number_accounts <- full_2025_data$active_number_accounts

forecast <- predict(m, future)
prophet_plot_components(m, forecast)

plot(m, forecast) +
  ggtitle("Tickets Forecast") +
  xlab("Month") +
  ylab("Number of Service Tickets") +
  theme_minimal(base_size = 14) +   # Minimal theme with larger text
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

prophet_2025_df <- data.frame(forecast) %>% 
  select(ds,yhat,yhat_lower,yhat_upper)

comparison <- train_data %>%
  left_join(forecast %>% select(ds, yhat), by = "ds") %>% 
  drop_na()

comparison <- comparison %>%
  mutate(residuals = y - yhat)

ss_res <- sum(comparison$residuals^2)
ss_tot <- sum((comparison$y - mean(comparison$y))^2)
r_squared <- 1 - (ss_res / ss_tot) #  0.9631046

mape <- mean(abs((comparison$y - comparison$yhat) / comparison$y)) * 100 

# Internal tickets 2025 forecast ----

# Prophet

train_data <- combined_df %>% 
  select(month,cases) %>% 
  rename(ds = month, y = cases) 

m <- prophet(train_data)
future <- make_future_dataframe(m, periods = 12, freq = 'month')
forecast <- predict(m, future)

plot(m, forecast) +
  ggtitle("Tickets Forecast") +
  xlab("Month") +
  ylab("Number of Service Tickets") +
  theme_minimal(base_size = 14) +   # Minimal theme with larger text
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

int_prophet_2025_df <- data.frame(forecast) %>% 
  select(ds,yhat,yhat_lower,yhat_upper)

comparison <- train_data %>%
  left_join(forecast %>% select(ds, yhat), by = "ds") %>% 
  drop_na()

comparison <- comparison %>%
  mutate(residuals = y - yhat)

ss_res <- sum(comparison$residuals^2)
ss_tot <- sum((comparison$y - mean(comparison$y))^2)
r_squared <- 1 - (ss_res / ss_tot) #  0.7941179

mape <- mean(abs((comparison$y - comparison$yhat) / comparison$y)) * 100 # 55.7%


# DISPUTES ONLY

train_data <- combined_df %>%
  select(month, cases) %>%
  filter(month >= '2023-09-01') %>% 
  rename(ds = month, y = cases) %>%
  mutate(
    floor = 0,
    cap = max(y) * 1.5 
  )

m <- prophet(growth = "logistic")
m <- fit.prophet(m, train_data)

future <- make_future_dataframe(m, periods = 12, freq = 'month') %>%
  mutate(
    floor = 0,
    cap = max(train_data$cap)  # Use same cap
  )

forecast <- predict(m, future)

test <- data.frame(forecast) %>% 
  select(ds,yhat,yhat_lower,yhat_upper)

# Squads workload drivers ----

squads_df <- sf_clean %>% 
  mutate(region = case_when(hours >= 1 & hours < 9 ~ 'APAC',
                            hours >= 9 & hours < 17 ~ 'EMEA',
                            TRUE ~ 'Americas'
                            )) %>% 
  filter(month < "2025-04-01",
         !origin %in% c('Side Conversation','Call'))

service_df <- squads_df %>% 
  filter(
    category == 'Service') %>% 
  group_by(month,squad,region) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  drop_na()

internal_df <- squads_df %>% 
  filter(
    category == 'Internal') %>% 
  group_by(month,squad,region) %>% 
  summarise(cases = sum(cases,na.rm = T)) %>% 
  drop_na()

# Squads check

platforms_cases <- service_df %>% filter(squad == 'LATAM') %>% group_by(month) %>% summarise(cases = sum(cases,na.rm = T)) 

platforms_accounts <- combine_accounts %>% 
  filter(pillar_clean == 'Platforms') %>% 
  group_by(month_seq) %>% summarise(accounts = sum(active_number_accounts,na.rm = T)) 

platforms_df <- platforms_cases %>% left_join(platforms_accounts, by = c('month'='month_seq'))

correlation_value <- cor(platforms_df$accounts, platforms_df$cases, use = "complete.obs")
r_squared <- correlation_value^2

platforms_df %>%
  ggplot(aes(x = accounts, y = cases)) +
  geom_point(color = "#4285f4", alpha = 0.7, size = 3) +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  
  # scale_x_continuous(breaks = seq(from = floor(min(platforms_df$accounts, na.rm = TRUE) / 1000) * 1000,
  #                                 to = ceiling(max(platforms_df$accounts, na.rm = TRUE) / 1000) * 1000,  
  #                                 by = 1000)) +
  labs(title = paste("JP & CN - Active Accounts vs Internal Tickets (Correlation:", round(correlation_value, 2), ") (R²:", round(r_squared, 2), ")"),
       subtitle = "(Ops & Tech, excluding Calls)",
       x = "Number of Active Accounts",
       y = "Number of Internal Tickets") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Squads forecasting ----

accounts_forecast_df <- combine_accounts %>% 
  mutate(month = as.Date(month_seq)) %>% 
  group_by(month) %>% 
  summarise(active_number_accounts = sum(active_number_accounts,na.rm = T)) %>% 
  filter(month < '2025-04-01')
  
ts_accounts <- ts(accounts_forecast_df$active_number_accounts, start = c(year(min(accounts_forecast_df$month)), month(min(accounts_forecast_df$month))), frequency = 12)
log_ts_accounts <- log(ts_accounts + 1)
diff_log_ts_accounts <- diff(log_ts_accounts)

# Test stationary 
adf.test(diff_log_ts_accounts, alternative = "stationary") 

fitARIMA <- auto.arima(log_ts_accounts, seasonal = FALSE, trace = TRUE)
summary(fitARIMA)

future_forecast <- forecast(fitARIMA, h = 12, level = c(95))  

future_months <- seq(from = as.Date(paste0(year(max(accounts_forecast_df$month)), "-", month(max(accounts_forecast_df$month)), "-01")) + months(1), 
                     by = "1 month", length.out = 12)

forecasted_df <- data.frame(
  month = future_months,
  forecasted_active_accounts = exp(future_forecast$mean) - 1,  
  lower_bound = exp(future_forecast$lower[,1]) - 1,  
  upper_bound = exp(future_forecast$upper[,1]) - 1,
  type = "Forecast"
)

account_growth <- bind_rows(
  accounts_forecast_df %>% 
    select(month, active_number_accounts) %>% 
    rename(forecasted_active_accounts = active_number_accounts) %>% 
    mutate(lower_bound = NA, upper_bound = NA, type = "Actual"),   
  forecasted_df)

# Service cases forecasting

service_forecast_df <- service_df %>% 
  group_by(month,squad) %>%
  summarise(cases = sum(cases,na.rm = T)) %>%
  left_join(account_growth, by = 'month') %>% 
  select(-lower_bound,-upper_bound,-type) %>% 
  rename(ds = month, y = cases)

nested_df <- service_forecast_df %>%
  group_by(squad) %>%
  # group_by(squad, region) %>%
  nest()

run_prophet <- function(df) {
  # Ensure required columns are present
  required_cols <- c("ds", "y", "forecasted_active_accounts")
  if (!all(required_cols %in% colnames(df))) {
    return(NULL)
  }
  
  # Remove NAs and check row count
  df <- df %>% drop_na(ds, y, forecasted_active_accounts)
  if (nrow(df) < 10) return(NULL)  # Prophet needs a minimum of points
  
  # Fit the model
  m <- prophet()
  m <- add_regressor(m, 'forecasted_active_accounts')
  m <- fit.prophet(m, df)
  
  # Make future df
  future <- make_future_dataframe(m, periods = 12, freq = 'month')
  future <- future %>%
    left_join(account_growth, by = c('ds' = 'month')) %>%
    select(ds, forecasted_active_accounts)
  
  # Forecast
  forecast <- predict(m, future)
  
  # Evaluate model
  comparison <- df %>%
    left_join(forecast %>% select(ds, yhat), by = "ds") %>%
    drop_na() %>%
    mutate(residuals = y - yhat)
  
  ss_res <- sum(comparison$residuals^2)
  ss_tot <- sum((comparison$y - mean(comparison$y))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  mape <- mean(abs((comparison$y - comparison$yhat) / comparison$y)) * 100
  
  list(
    model = m,
    forecast = forecast,
    r_squared = r_squared,
    mape = mape
  )
}

results <- nested_df %>%
  mutate(results = map(data, run_prophet)) %>%
  filter(!map_lgl(results, is.null))  

forecast_df <- results %>%
  mutate(forecast = map(results, "forecast")) %>%
  # select(squad, region, forecast) %>%
  select(squad, forecast) %>%
  unnest(forecast)

metrics_df <- results %>%
  transmute(
    squad,
    # region,
    r_squared = map_dbl(results, "r_squared"),
    mape = map_dbl(results, "mape")
  )

comparison_df <- results %>%
  mutate(
    forecast = map(results, "forecast"),
    comparison = map2(data, forecast, ~ left_join(.x, .y, by = "ds") %>% drop_na(y, yhat))
  ) %>%
  select(squad, comparison) %>%
  unnest(comparison)

final_squads_df <- forecast_df %>% 
  # select(ds,squad,region,yhat_lower,yhat,yhat_upper) %>%
  select(ds,squad,yhat_lower,yhat,yhat_upper) %>%
  mutate(category = 'Service') %>% 
  filter(ds >= '2025-04-01',
         squad != 'LATAM',
         squad != 'Japan & China') %>% 
  mutate(ds = as.Date(ds))

write_csv(final_squads_df,paste0(local_folder,"/service_forecast.csv"))

# Internal cases forecasting

internal_forecast_df <- internal_df %>% 
  # group_by(month,squad) %>% 
  # summarise(cases = sum(cases,na.rm = T)) %>% 
  left_join(account_growth, by = 'month') %>% 
  select(-lower_bound,-upper_bound,-type) %>% 
  rename(ds = month, y = cases) %>% 
  filter(ds < '2025-04-01')

nested_df <- internal_forecast_df %>%
  # group_by(squad) %>% 
  group_by(squad, region) %>%
  nest()

run_prophet <- function(df) {
  # Ensure required columns are present
  required_cols <- c("ds", "y", "forecasted_active_accounts")
  if (!all(required_cols %in% colnames(df))) {
    return(NULL)
  }
  
  # Remove NAs and check row count
  df <- df %>% drop_na(ds, y, forecasted_active_accounts)
  if (nrow(df) < 10) return(NULL)  # Prophet needs a minimum of points
  
  # Fit the model
  m <- prophet()
  m <- fit.prophet(m, df)
  
  # Make future df
  future <- make_future_dataframe(m, periods = 12, freq = 'month')
  
  # Forecast
  forecast <- predict(m, future)
  
  # Evaluate model
  comparison <- df %>%
    left_join(forecast %>% select(ds, yhat), by = "ds") %>%
    drop_na() %>%
    mutate(residuals = y - yhat)
  
  ss_res <- sum(comparison$residuals^2)
  ss_tot <- sum((comparison$y - mean(comparison$y))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  mape <- mean(abs((comparison$y - comparison$yhat) / comparison$y)) * 100
  
  list(
    model = m,
    forecast = forecast,
    r_squared = r_squared,
    mape = mape
  )
}

results <- nested_df %>%
  mutate(results = map(data, run_prophet)) %>%
  filter(!map_lgl(results, is.null))  

forecast_df <- results %>%
  mutate(forecast = map(results, "forecast")) %>%
  select(squad, region, forecast) %>%
  # select(squad, forecast) %>%
  unnest(forecast)

metrics_df <- results %>%
  transmute(
    squad,
    # region,
    r_squared = map_dbl(results, "r_squared"),
    mape = map_dbl(results, "mape")
  )

comparison_df <- results %>%
  mutate(
    forecast = map(results, "forecast"),
    comparison = map2(data, forecast, ~ left_join(.x, .y, by = "ds") %>% drop_na(y, yhat))
  ) %>%
  select(squad, region, comparison) %>%
  unnest(comparison)

final_squads_df <- forecast_df %>% 
  select(ds,squad,region,yhat_lower,yhat,yhat_upper) %>%
  # select(ds,squad,yhat_lower,yhat,yhat_upper) %>% 
  mutate(category = 'Internal') %>% 
  filter(ds >= '2025-04-01',
         squad != 'LATAM',
         squad != 'Japan & China') %>% 
  mutate(ds = as.Date(ds))

write_csv(final_squads_df,paste0(local_folder,"/internal_forecast.csv"))

