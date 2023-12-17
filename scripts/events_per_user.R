# Packages ----
library(tidyverse)
library(bigrquery)

# Auth ----
bq_auth(path = "googlecloudrunner-auth-key.json")

# Retrieve table dimensions per user ----
project_id = "plumber-api-408220"
dataset_id = "plumber_api"
table_id = "2023_12_16_events_per_user"

bq_table_placeholder = bq_table(project = project_id, dataset = dataset_id, table = table_id)
events_per_user = bq_table_download(bq_table_placeholder)

# 1 Wrangle data ----
events_per_user = events_per_user %>% 
  
  mutate(add_to_cart = case_when(
    add_to_cart_events > 0 ~ 1,
    TRUE ~ 0)) %>% 
  
  select(-add_to_cart_events)

# 2 Save data dimensions per user -----
setwd("/Users/arbenkqiku/Desktop/GA4-ML-R-Docker-GCP/data")
saveRDS(events_per_user, "events_per_user.RDS")
    