## Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tm)
library(janitor)

# Define the path to the input file
input_file_path <- '/Users/timdennis/websites/dsc_reports/data/libinsights-full21-24-dataframe.csv' # Replace with your actual file path

# Read the input data and handle potential parsing issues
input_data <- read_csv(input_file_path, guess_max = 10000)
input_data <- clean_names(input_data)

# Display column names for verification
print(colnames(input_data))

# Ensure the correct column names for processing
if (!"response_3" %in% colnames(input_data)) {
  stop("The expected column 'response_3' does not exist in the input data.")
}

# List of DataSquad members in "Last Name, First Name" format
datasquad_members <- c("Wood, Julia", "Pablo, Keona Mae", "Foote, William", "Deleted, ", 
                       "Gong, Emily", "Front, Vincenty", "Mirpuri, Shail", "Dewing, Tristan", 
                       "Front, Vince", "Lee, Lawrence", "Lee, Hyerin", "Hu, Loretta", 
                       "Hager, Lukas", "Xu, Aimee", "Bharath, Aditya")

# Separate User Name into First Name and Last Name, then unite them in the desired format
input_data <- input_data %>%
  separate(user_name, into = c("First Name", "Last Name"), sep = " ", extra = "merge", fill = "right") %>%
  unite("Entered By", c("Last Name", "First Name"), sep = ", ")

# Further normalize "Entered By" to match the format
input_data <- input_data %>%
  mutate(`Entered By` = trimws(`Entered By`)) %>%
  mutate(`Entered By` = str_to_title(`Entered By`))

# Update the "Entered By" field for DataSquad members
input_data <- input_data %>%
  mutate(`Entered By` = ifelse(`Entered By` %in% datasquad_members, "Students, Deleted", `Entered By`))

# Replace NAs in "response_2" with "Unknown"
input_data <- input_data %>%
  mutate(response_2 = replace_na(response_2, "Unknown"))

# Define custom stop words (names to remove)
c_stop_words <- c("jamie", "tim", "doug", "ibraheem", "leigh", "zhiyuan", 
                  "gillian", "kristian", "ucla", "julia", "yao", "ms", "dennis", 
                  "jamison", "keona", "minute", "meeting", "and", "hi", "meeting", "min", 
                  "consultation appointment", "work hours", "one on one", "one off", 
                  "testing", "technical writer  interview", "exit interview",
                  "datasquad status report reschedule", "loretta", "diana", "on boarding for data squad",
                  "make an appointment with", "with", "grad lunch at luskin", 
                  "iranian census debrief", "project manager interview", 
                  "assistant technical writer interview", "rescheduling", "calendly",
                  "dataops engineer interview", "edvin", "into libinsight",
                  "developer interview", "https docs google com spreadsheets", "https docs google com document",
                  "https cran r project org web packages", "jerry liu", "bianca", "melina should participate in the", 
                  "pricing", "we want to talk about the blog topics datasquad blog discussion", "aditya",
                  "uc love data week recap assessment ", "interview project manager data squad", 
                  "interview discussion discussing interview candidates  selecting finalists  datasquad team", 
                  "technical writer interview   data science center", "looking forward to my interview   datasquad"
                  )

# Preprocess the text data: Remove custom stop words (names)
input_data <- input_data %>%
  filter(!is.na(response_3)) %>%
  mutate(response_3 = str_replace_all(response_3, "\\|", " "),
         response_3 = str_replace_all(response_3, "[^a-zA-Z\\s]", " "),
         response_3 = str_trim(response_3), 
         response_3 = tolower(response_3)) %>%
  rowwise() %>%
  mutate(response_3 = str_replace_all(response_3, paste0("\\b(", paste(c_stop_words, collapse = "|"), ")\\b"), "")) %>%
  ungroup()

# Define primary categories and keywords for classification
primary_categories <- list(
  "Define the goal, scope, and plan" = c("goal", "scope", "plan", "proposal"),
  "Investigate diverse sources and perspectives" = c("search strategy", "diverse perspectives", "investigating"),
  "Gather and organize information and data" = c("finding", "data", "resources", "access"),
  "Evaluate and synthesize information and data" = c("evaluate", "synthesize", "literature review", "sources"),
  "Use information and data ethically" = c("citing", "copyright", "fair use", "ethically"),
  "Share the work and engage with audiences" = c("submitting", "journal", "conference", "publishing"),
  "Reflect on and refine the research process" = c("improve", "refine", "research process", "skills"),
  "Analyze and evaluate primary sources" = c("primary sources", "observations", "contextualizing", "inferences"),
  "Find and use primary sources" = c("search strategy", "defining primary sources")
)

# Define secondary categories and keywords for classification
secondary_categories <- list(
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
  "Statistical support and modeling" = c("statistical support", "modeling", "statistical methods", "model selection", "interpreting results")
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

# Function to classify and format the session focus
classify_and_format <- function(entry) {
  if (is.na(entry) || entry == "") {
    return("")
  }
  
  entry <- preprocess_text(entry)
  primary_matches <- c()
  secondary_matches <- c()
  
  # Match primary categories
  for (category in names(primary_categories)) {
    keywords <- primary_categories[[category]]
    if (length(keywords) > 0 && any(str_detect(entry, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)))) {
      primary_matches <- c(primary_matches, category)
    }
  }
  
  # Match secondary categories
  for (category in names(secondary_categories)) {
    keywords <- secondary_categories[[category]]
    if (length(keywords) > 0 && any(str_detect(entry, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)))) {
      secondary_matches <- c(secondary_matches, category)
    }
  }
  
  # Combine matches
  if (length(primary_matches) > 0 && length(secondary_matches) > 0) {
    return(paste(c(primary_matches, "Other", secondary_matches), collapse = "; "))
  } else if (length(primary_matches) > 0) {
    return(paste(primary_matches, collapse = "; "))
  } else if (length(secondary_matches) > 0) {
    return(paste("Other", paste(secondary_matches, collapse = "; "), sep = "; "))
  } else {
    return("Other")
  }
}

# Apply the classification function to categorize and format each entry
input_data$session_focus <- sapply(input_data$response_3, classify_and_format, USE.NAMES = FALSE)

# Define the mapping
mapping <- list(
  "start_date_time" = "Event Date and Time",
  "45" = "Duration",
  "online_synchronous_zoom_skype" = "Interaction Mode",
  "data_science_center" = "Location",
  "1" = "Patron count",
  "response_2" = "Patron type",
  "research_assistance_technical" = "Question Type",
  "response_3" = "Question/Topic",
  "scheduled" = "Scheduled or Drop-in",
  "response_1" = "School/Department/Center",
  "off_desk" = "Service point"
)

# Apply the mapping
transformed_data <- input_data %>%
  mutate(
    `Event Date and Time` = start_date_time,
    `Course Number` = NA,
    `Developer name` = NA,
    `Duration` = 45,
    `Event or Class Title` = NA,
    `Faculty/TA/Staff Email` = NA,
    `Instructional Interaction Type` = NA,
    `Instructor/Co-instructor` = NA,
    `Interaction Mode` = "Online - synchronous (Zoom/Skype)",
    `Location` = "Data Science Center",
    `Patron count` = 1,
    `Patron type` = response_2,
    `Program Area/ Initiatives` = NA,
    `Question Type` = "Research Assistance,Technical",
    `Question/Topic` = response_3,
    `Referral to` = NA,
    `Scheduled or Drop-in` = "Scheduled",
    `School/Department/Center` = response_1,
    `Service point` = "Off desk",
    `Unit/Location` = "DSC",
    `WI+RE Resources Used` = NA,
    `Session Focus` = session_focus
  )

# Define columns to select, checking if they exist
selected_columns <- c(
  "Event Date and Time", "Entered By", "Course Number", "Developer name",
  "Duration", "Event or Class Title", "Faculty/TA/Staff Email", "Instructional Interaction Type",
  "Instructor/Co-instructor", "Interaction Mode", "Location", "Patron count", "Patron type",
  "Program Area/ Initiatives", "Question Type", "Question/Topic", "Referral to",
  "Scheduled or Drop-in", "School/Department/Center", "Service point", "Session Focus",
  "Unit/Location", "WI+RE Resources Used"
)

# Select only the columns that exist in the dataframe
selected_columns <- selected_columns[selected_columns %in% colnames(transformed_data)]

# Select the columns
transformed_data <- transformed_data %>%
  select(all_of(selected_columns))

# Define the path to the output file
output_file_path <- "data/calendly_to_libinsight.csv" # Replace with your actual file path

# Write the transformed data to a new CSV file
write_csv(transformed_data, output_file_path)
