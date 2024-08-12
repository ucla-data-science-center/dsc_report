# Load necessary libraries
required_packages <- c("dplyr", "readr", "stringr", "janitor", "lubridate", "tidyr", "purrr", "tm")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(required_packages, library, character.only = TRUE)

# Load the standardize map of departments
source("src/standardize_map.R")

# Define categories and keywords
categories <- list(
  "Define the goal, scope, and plan" = c("goal", "scope", "plan", "proposal"),
  "Investigate diverse sources and perspectives" = c("search strategy", "diverse perspectives", "investigating"),
  "Gather and organize information and data" = c("finding", "data", "resources", "access"),
  "Evaluate and synthesize information and data" = c("evaluate", "synthesize", "literature review", "sources"),
  "Use information and data ethically" = c("citing", "copyright", "fair use", "ethically"),
  "Share the work and engage with audiences" = c("submitting", "journal", "conference", "publishing"),
  "Reflect on and refine the research process" = c("improve", "refine", "research process", "skills"),
  "Analyze and evaluate primary sources" = c("primary sources", "observations", "contextualizing", "inferences"),
  "Find and use primary sources" = c("search strategy", "defining primary sources"),
  "Define the data project goal, scope, and plan" = c("data project goal", "data project scope", "data project plan"),
  "Investigate diverse data sources and perspectives" = c("collecting data", "looking for data", "data sources", "data perspectives"),
  "Gather, wrangle, and organize data" = c("data wrangling", "cleaning data", "organizing data", "preprocessing data", "data gathering"),
  "Evaluate and synthesize data" = c("evaluate data", "synthesize data", "data trends", "data integration"),
  "Analyze and evaluate datasets" = c("data analysis", "evaluate datasets", "statistical analysis"),
  "Utilize coding, programming, and technical tools" = c("coding", "programming", "technical tools", "R", "Python", "LaTex", "web scraping"),
  "Apply geospatial data techniques and GIS" = c("geospatial data", "GIS", "spatial analysis", "ArcGIS"),
  "Plan, manage, and share data projects" = c("data management planning", "data sharing", "data projects", "data archiving"),
  "Publish and share data" = c("data publishing", "Dataverse", "data sharing platforms", "data publication"),
  "Use data ethically" = c("ethical data use", "data privacy", "ethical considerations"),
  "Provide troubleshooting and technical support" = c("troubleshooting", "technical support", "software issues", "coding issues"),
  "Teach and train on data tools and techniques" = c("training", "workshops", "data tools", "data techniques", "tutorials"),
  "Reflect on and refine the data analysis process" = c("reflect on data analysis", "refine data analysis", "improve data practices"),
  "Ensure reproducibility and optimize research workflows" = c("reproducibility", "optimize code", "research workflows", "version control", "documentation"),
  "Statistical support and modeling" = c("statistical support", "modeling", "statistical methods", "model selection", "interpreting results"),
  "Other" = c()
)

# Enhanced text preprocessing function
preprocess_text <- function(text) {
  if (is.na(text) || text == "") {
    return("")
  }
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  text <- removeWords(text, stopwords("en"))
  return(text)
}

# Define the classification function with keyword matching
classify_entry <- function(entry) {
  if (is.na(entry) || entry == "") {
    return("Other")
  }
  matches <- c()
  
  for (category in names(categories)) {
    keywords <- categories[[category]]
    if (length(keywords) > 0 && any(str_detect(entry, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)))) {
      matches <- c(matches, category)
    }
  }
  
  if (length(matches) == 0) {
    return("Other")
  } else {
    return(paste(matches, collapse = ", "))
  }
}

# Function to clean DataSquad Sign-in data
clean_ds_sign_in <- function(df) {
  df %>%
    clean_names() %>%
    select(timestamp, format, department, year, what_topic_do_you_need_help_with_today) %>%
    rename(
      start_date_time = timestamp,
      ucla_affiliation = year,
      appointment_reason = what_topic_do_you_need_help_with_today,
      location = format
    ) %>%
    mutate(
      ucla_affiliation = str_remove_all(ucla_affiliation, "\\s+\\d+(st|nd|rd|th)$"),
      start_date_time = mdy_hms(start_date_time),
      group = "Datasquad",
      user_name = "DataSquad Walk-in",
      appointment_reason = sapply(appointment_reason, preprocess_text),
      classification = sapply(appointment_reason, classify_entry)
    )
}

# Function to clean Calendly data
clean_calendly <- function(df) {
  dsc_members <- c("Jamie Jamison", "Tim Dennis", "Doug Daniels", "Ali, Ibraheem", 
                   "Leigh Phan", "Zhiyuan Yao", "Gillian Bailey", "Kristian Allen",
                   "Zhiyuan (Jee-Wan) Yao")
  
  datasquad_members <- c("JULIA WOOD", "KEONA MAE PABLO", "WILLIAM FOOTE", "deleted", 
                         "EMILY GONG", "Vincenty Front", "Shail Mirpuri", "Tristan Dewing", 
                         "Vince Front", "Lawrence Lee", "Hyerin Lee", "LORETTA HU", 
                         "LUKAS HAGER", "AIMEE XU", "Aditya Bharath")
  
  df %>%
    clean_names() %>%
    select(start_date_time, location, response_1, response_2, response_3, user_name) %>%
    rename(
      department = response_1,
      ucla_affiliation = response_2,
      appointment_reason = response_3
    ) %>%
    mutate(
      group = case_when(
        tolower(user_name) %in% tolower(dsc_members) ~ "DSC",
        tolower(user_name) %in% tolower(datasquad_members) ~ "Datasquad",
        TRUE ~ "Other"
      ),
      appointment_reason = sapply(appointment_reason, preprocess_text),
      classification = sapply(appointment_reason, classify_entry)
    )
}

# Function to clean Trello data
clean_trello <- function(df) {
  df %>%
    clean_names() %>%
    uncount(comment_count, .id = "instance") %>%
    mutate(location = NA_character_) %>%
    select(start_date_time = last_activity_date, location, 
           department, ucla_affiliation, 
           appointment_reason = card_description, 
           user_name = members) %>%
    mutate(group = "Datasquad",
           appointment_reason = sapply(appointment_reason, preprocess_text),
           classification = sapply(appointment_reason, classify_entry))
}

# Function to standardize department names
standardize_dept <- function(dept) {
  if (is.na(dept) || dept == "") {
    return(NA_character_)
  } else {
    dept <- str_trim(dept) %>% str_to_lower()  # Trim whitespace and convert to lower case
    standardized <- standardize_map[[dept]]
    if (is.null(standardized)) {
      return(NA_character_)
    } else {
      return(standardized)
    }
  }
}

# Function to standardize departments in a dataframe
standardize_departments <- function(df) {
  df %>%
    mutate(department = vapply(department, standardize_dept, character(1)))
}

# Load datasets
dsc_workshops <- read_csv('data/dsc_workshops.csv', na=c("N/A", "", "NA"))
dsc_calendly <- read_csv('data/libinsights-full21-24-dataframe.csv')
dsquad_sign_in <- read_csv('data/datasquad-sign-in.csv')
dsquad_trello <- read_csv('data/ucla-datasquad-projects-trello.csv')

# Clean datasets
dsc_calendly <- clean_calendly(dsc_calendly)
dsquad_sign_in <- clean_ds_sign_in(dsquad_sign_in)
dsquad_trello <- clean_trello(dsquad_trello)

# Combine datasets
dsc_consult <- bind_rows(dsc_calendly, dsquad_sign_in, dsquad_trello)

# Standardize departments
dsc_consult <- standardize_departments(dsc_consult)

# Save the cleaned and standardized data
saveRDS(dsc_consult, "data/dsc_consult_standardized.rds")

