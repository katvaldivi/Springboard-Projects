## upload refine_original file to workspace
refine_original <- read_csv("~/Desktop/Springboard/refine_original.csv")

## install dplyr and tidyr
library(dplyr)
library(tidyverse)

##Clean up brand names in 'company' column
refine_clean <- refine_original %>%
mutate(company = case_when(grepl("^un", refine_original$company, ignore.case = TRUE) ~ "Unilever", TRUE ~ company)) %>%
mutate(company = case_when(grepl("^van", refine_original$company, ignore.case = TRUE) ~ "Van Houten", TRUE ~ company)) %>%
mutate(company = case_when(grepl(".*\\ps", refine_original$company, ignore.case = TRUE) ~ "Philips", TRUE ~ company)) %>%
mutate(company = case_when(grepl("^ak", refine_original$company, ignore.case = TRUE) ~ "Akzo", TRUE ~ company))

#Separate product code and number and create new columns 'product_code' & 'product_number'
refine_clean <- separate(refine_clean, "Product code / number", c("product_code", "product_number"), sep = "-")

#Add product categories and create new column for it
refine_clean <- refine_clean %>%
mutate(product_categories = case_when(grepl("x", product_code) ~ "laptop",
                                      grepl("p", product_code) ~ "smartphone",
                                      grepl("v", product_code) ~ "TV",
                                      grepl("q", product_code) ~ "tablet"))

#Add full address in new column
refine_clean <- unite(refine_clean, address, address, city, country, sep = ", ", remove = TRUE)

#Create dummy variables for product and company category
refine_clean <- refine_clean %>%
mutate(company_philips = case_when(grepl("^Ph", refine_clean$company) ~ "1", TRUE ~ "0")) %>%
mutate(company_akzo = case_when(grepl("^Ak", refine_clean$company) ~ "1", TRUE ~"0")) %>%
mutate(company_van_houten = case_when(grepl("^Va", refine_clean$company) ~ "1", TRUE ~ "0")) %>%
mutate(company_unilever = case_when(grepl("^Un", refine_clean$company) ~ "1", TRUE ~ "0")) %>%
mutate(product_smartphone = case_when(grepl("^sm", refine_clean$product_categories) ~ "1", TRUE ~ "0")) %>%
mutate(product_tv = case_when(grepl("^T", refine_clean$product_categories) ~ "1", TRUE ~ "0")) %>%
mutate(product_laptop = case_when(grepl("^la", refine_clean$product_categories) ~ "1", TRUE ~ "0")) %>%
mutate(product_tablet = case_when(grepl("^tab", refine_clean$product_categories) ~ "1", TRUE ~ "0"))


