# Packages ----
library(tidyverse)
library(bigrquery)

# Auth ----
bq_auth(path = "googlecloudrunner-auth-key.json")

# Retrieve table dimensions per user ----
project_id = "plumber-api-408220"
dataset_id = "plumber_api"
table_id = "2023_12_16_dimensions_per_user"

bq_table_placeholder = bq_table(project = project_id, dataset = dataset_id, table = table_id)
dimensions_per_user = bq_table_download(bq_table_placeholder)

# 1 Feature Engineering ----
# 1.1 Page titles
# Pages are sometimes composed of "category 1 | category 2 | page title". 
# So, we can extract the categories to create some additional features for our model
# Also, we will not use page_title as that would entail too many columns when we one-hot encode
dimensions_per_user = dimensions_per_user %>% 

  separate(col = "page_title", sep = "\\|", into = c("page_cat_1", "page_cat_2"), remove = FALSE) %>% 
  
  # Remove blank space at the beginning or at the end of a character vector
  mutate(page_cat_1 = page_cat_1 %>% str_replace_all("^\\s+|\\s+$", ""),
         page_cat_2 = page_cat_2 %>% str_replace_all("^\\s+|\\s+$", "")) %>% 
  
  # Sometimes, a value is both contained in page_cat_1 and page_cat_2, such as "Apparel"
  # if we don't differentiate between the two it is going to be complicated when we
  # use the function pivot_wider(), as the "Apparel" column will already exist
  mutate(page_cat_1 = str_c("cat_1_", page_cat_1),
         page_cat_2 = str_c("cat_2_", page_cat_2))

# 1.2 Browser languages ----
# Browser language can be composed of the language and a sub-region of that language (ex.: en-us)
# the subregion does not necessarily coincide with the region the country the person
# comes from, so it is important to extract this feature. For instance, someones from India
# may have a browser language that is "en-us".
dimensions_per_user = dimensions_per_user %>% 
  
  separate(col = "language", sep= "-", into = c("browser_lang_1", "browser_lang_2")) %>% 
  
  mutate(browser_lang_1 = str_c("lang_1_", browser_lang_1),
         browser_lang_2 = str_c("lang_2_", browser_lang_2))

# 2 One-hot enconding ----
# 2.1 Function to one-hot encode single variables
one_hot_encode_var = function(data_frame, column_to_encode){
  
  encoded_data_frame = data_frame %>% 
    # select only appropriate columns
    select(user_pseudo_id, {{column_to_encode}}) %>% 
    # create column to one-hot encode when we pivot
    mutate(value = 1) %>% 
    # avoid having duplicate values
    distinct() %>% 
    pivot_wider(names_from = {{column_to_encode}}, values_from = value, values_fill = 0) %>%
    # remove this column as it is not useful
    select(-`NA`)
  
  return(encoded_data_frame)
  
}

# 2.2 data without variables to one-hot encode
data_no_hot_encode = dimensions_per_user %>% 
  
  rename(device_category = category) %>% 

  select(-page_title, -page_cat_1, -page_cat_2, -browser_lang_1, -browser_lang_2) %>% 
  
  distinct()

  # keep only data of users that are unique
data_no_hot_encode %>% 
  filter(!(user_pseudo_id %in% duplicate_user_pseudo_id))
  
# 2.4 one-hot encode page_cat_1
one_hot_page_cat_1 = one_hot_encode_var(dimensions_per_user, "page_cat_1")

# 2.5 one-hot encode page_cat_2
one_hot_page_cat_2 = one_hot_encode_var(dimensions_per_user, "page_cat_2")

# 2.6 one-hot encode browser_lang_1
one_hot_browser_lang_1 = one_hot_encode_var(dimensions_per_user, "browser_lang_1")

# 2.7 one-hot encode browser_lang_2
one_hot_browser_lang_2 = one_hot_encode_var(dimensions_per_user, "browser_lang_2")

# 2.8 Join all one-hot encoded variables
all_data_frames = list(data_no_hot_encode, one_hot_page_cat_1,
                            one_hot_page_cat_2, one_hot_browser_lang_1, one_hot_browser_lang_2)

dimensions_per_user = all_data_frames %>% 
  reduce(left_join, by = "user_pseudo_id")

# 2.9 Remove duplicate users
  # identify user_pseudo_id that are duplicate
duplicate_user_pseudo_id = data_no_hot_encode %>% 
  mutate(value = 1) %>% 
  
  group_by(user_pseudo_id) %>% 
  mutate(duplicated = value %>% cumsum()) %>% 
  ungroup() %>% 
  
  filter(duplicated > 1) %>% 
  pull(user_pseudo_id)

dimensions_per_user = dimensions_per_user %>% 
  filter(!(user_pseudo_id %in% duplicate_user_pseudo_id))

# 3 Save data dimensions per user -----
setwd("/Users/arbenkqiku/Desktop/GA4-ML-R-Docker-GCP/data")
saveRDS(dimensions_per_user, "dimensions_per_user.RDS")
