
# LOADING LIBRARIES AND DATA ----------------------------------------------

library(tidyverse)
library(qualtRics)
library(here)
library(readxl)
library(openxlsx)
library(naniar)
library(janitor)
library(chron)
library(data.table)


survey_num_original = read_survey(here("data","raw","Justice Sleep Survey_March 27, 2025_numeric.csv")) %>%
    clean_names() %>%
    filter(q3 == 1)



survey_numeric = read_survey(here("data","raw","Justice Sleep Survey_March 27, 2025_numeric.csv"))
survey_label = read_survey(here("data","raw","Justice Sleep Survey_March 27, 2025_text.csv"))


# GLOBAL FUNCTIONS AND VARIABLES ------------------------------------------


get_invalid_ids <- function(data, id_column="q2") {
    invalid_ids <- unique(data[!grepl("^[1-9][0-9]*$", data[[id_column]]) |
                                  as.integer(data[[id_column]]) < 100, id_column])
    
    if (length(invalid_ids) > 0) {
        message("Invalid IDs found: ", paste(invalid_ids[[id_column]], collapse = ", "))
    } else {
        message("All participant IDs are valid.")
    }
    
    return(invalid_ids[[id_column]])
}


get_dup_ids <- function(data, columns = "q2", output_file = TRUE) {
    duplicates <- data %>%
        group_by(across({{ columns }})) %>%
        arrange(across({{ columns }})) %>%
        filter(n() > 1)
    
    dups <- duplicates %>% distinct({{ columns }}, .keep_all = TRUE) %>% pull({{ columns }})
    
    return(dups)
}


has_dups = function(data, id_column = q2) {
    total_rows = nrow(data)
    unique_rows = data %>% distinct({{id_column}}) %>% nrow()
    num_participants = data %>% distinct({{id_column}}) %>% summarise(count = n())
    
    if (total_rows != unique_rows) {
        message("Duplicates exist in the data.")
        message("Number of rows:", total_rows)
        message("Number of participants:", num_participants$count)
    } else {
        message("No duplicates found in the data.")
    }
    
    duplicates = data %>%
        group_by(across({{ id_column }})) %>%
        arrange(across({{ id_column }})) %>%
        filter(n() > 1)
    
    
    
    
    return(duplicates)
    
    
}



check_cols = function(data, id_column=q2, old_col, new_col)  {
    
    data = data %>%
        select(q2, {{old_col}}, {{new_col}}) %>%
        arrange({{new_col}},{{old_col}})
    
    return (data)
}





subtract_one <- function(data, old_vars, new_vars) {
    for (i in seq_along(old_vars)) {
        data[[new_vars[i]]] <- data[[old_vars[i]]] - 1
    }
    return(data)
}


str_to_time <- function(data, str_column, type) {
    # Convert the specified string column to lowercase
    data[[str_column]] <- str_to_lower(data[[str_column]])
    
    # Fixing midnight, updated 3/8/24
    data[[str_column]] <- ifelse(data[[str_column]] == "midnight", "12am", data[[str_column]])
    data[[str_column]] <- gsub("midnight", "", data[[str_column]])
    data[[str_column]] <- gsub("mid", "", data[[str_column]])
    data[[str_column]] <- gsub("(?<=[0-9])m", "", data[[str_column]], perl = TRUE)
    
    # Split the string column by "or" and extract the first occurrence
    split_column <- strsplit(data[[str_column]], "\\bor\\b")
    data[[str_column]] <- sapply(split_column, function(x) trimws(x[1]))
    
    
    # Extract the time using the pattern
    pattern <- "(\\b((?:0?[1-9]|1[0-2])(?!\\d| (?![ap]))[:.]?(?:(?:[0-5][0-9]))?(?:\\s?[ap]m)?)\\b)"
    data[[str_column]] <- str_extract(data[[str_column]], pattern)
    
    # Fill in missing zero for single-digit hour
    data[[str_column]] <- gsub("(?<!\\d)1:(?!\\d)", "1:00", data[[str_column]], perl = TRUE)
    
    # Create has_am_pm and has_colon flags
    data[["has_am_pm"]] <- grepl("(am|pm|a.m|p.m|a|p)", data[[str_column]], ignore.case = TRUE)
    data[["has_colon"]] <- grepl(":", data[[str_column]])
    
    # Create time_hhmm column (time in hh:mm format)
    data <- data %>%
        mutate(time_hhmm = case_when(
            has_am_pm == TRUE & has_colon == TRUE ~ gsub("\\s*([ap])\\s*(m)?\\b", "", data[[str_column]], ignore.case = TRUE),
            has_am_pm == TRUE & has_colon == FALSE ~ paste0(gsub("\\s*(am|pm)\\b", "", data[[str_column]], ignore.case = TRUE), ":00"),
            has_am_pm == FALSE & has_colon == FALSE ~ paste0(data[[str_column]], ":00"),
            TRUE ~ data[[str_column]]
        ))
    

    # Create time_ampm column
    if (type == "bedtime") {
        data[["time_ampm"]] <- case_when(
            grepl("pm|p.m|p", data[[str_column]], ignore.case = TRUE) ~ "PM",
            grepl("am|a.m|a|midnight|Midnight", data[[str_column]], ignore.case = TRUE) ~ "AM",
            grepl("between 11", data[[str_column]], ignore.case = TRUE) ~ "PM",
            grepl("6:00|6:30|7:00|7:30|8:00|8:30|9:00|9:30|10:00|10:30|11:00|11:30", data$time_hhmm, ignore.case = TRUE) ~ "PM",
            grepl("12:00|12:30|1:00|1:30|2:00|2:30|3:00", data$time_hhmm, ignore.case = TRUE) ~ "AM",
            TRUE ~ NA_character_
        )
    } else if (type == "waketime") {
        data[["time_ampm"]] <- case_when(
            grepl("pm|p.m|p", data[[str_column]], ignore.case = TRUE) ~ "PM",
            grepl("am|a.m|a|midnight", data[[str_column]], ignore.case = TRUE) ~ "AM",
            TRUE ~ "AM")
    }
    
    # Create time_full column
    data <- data %>%
        mutate(time_full = case_when(
            !is.na(time_ampm) ~ paste(time_hhmm, time_ampm),
            TRUE ~ NA_character_
        ))
    
    # Convert time_full to datetime format
    data[[str_column]] <- as.ITime(as.POSIXlt(data$time_full, format = "%I:%M %p"))
    

    
    time_column = data[[str_column]]
    
    
    
    return (time_column)
    
}

# Function to convert the free-text responses to numerical values
str_to_num <- function(str) {
    invalid_str <- str[!grepl("\\d+", str)]
    
    if (length(invalid_str) > 0) {
        message("Invalid responses found: ", paste(invalid_str, collapse = ", "))
    }
    
    # Remove any non-numeric characters and extra spaces
    str <- gsub("[^0-9.-]", "", str)
    str <- trimws(str)
    
    if (grepl("-", str)) {
        # If the str contains a range like "x-y", calculate the middle value
        range_values <- as.numeric(strsplit(str, "-")[[1]])
        middle_value <- mean(range_values)
        return(middle_value)
    } else {
        # If the str is a single number, convert it to numeric
        return(as.numeric(str))
    }
}



is_valid_time_str <- function(str, type) {
    
    str <- tolower(str)
    time <- strsplit(str, "[-/]")
    time <- sapply(time, function(x) x[1])
    time <- strsplit(time, ":")
    time <- sapply(time, function(x) x[1])
    # Split the time string at "-" or "/"
    
    
    if (type == "waketime") {
        invalid_times <- grepl("pm", time)
        
        if (any(invalid_times)) {
            message("Invalid times found: ", paste(str[invalid_times], collapse = ", "))
        } else {
            message("All times are valid.")
        }
    } else if (type == "bedtime") {
        invalid_times <- grepl("am", time) & grepl("9|10|11", time) |
            grepl("pm", time) & grepl("\\b[123]\\b|12", time)
        
        if (any(invalid_times)) {
            message("Invalid times found: ", paste(str[invalid_times], collapse = ", "))
        } else {
            message("All times are valid.")
        }
    } else {
        message("Invalid type specified.")
    }
}


is_valid_time <- function(data, time_col, str_col, type) {
    if (type == "bedtime") {
        valid_range <- c(18:24,0:4)
        
        # Define the valid range for bedtime (between 8:00 PM and 8:00 AM)
    } else if (type == "waketime") {
        valid_range <-c(0:11) #Not valid 13-24
    } else {
        stop("Invalid type argument. Please specify either 'bedtime' or 'wake time'.")
    }
    
    outliers <- data %>% 
        mutate(hrs = round(as.numeric({{time_col}}/3600)),0) %>%
        filter(!hrs %in% valid_range) %>% 
        select(q2,{{str_col}},{{time_col}}) %>%
        drop_na()
    
    if (nrow(outliers) > 0) {
        message("Possible outliers found in ", type)
    } else {
        message("All ", type, " values are within the valid range.")
    }
    
    return(outliers)
}


is_valid_ht <- function(data, height) {
    valid_range <- c(50, 90)  # Define the valid range for heights in inches
    pattern = "^\\d+(\\.\\d+)?$"
    
    outliers <- data %>%
        filter(!grepl(pattern, height) | height < valid_range[1] | height > valid_range[2]) %>%
        select(q2, height) %>%
        drop_na()
    
    if (nrow(outliers) > 0) {
        message("Possible outliers found in heights")
    } else {
        message("All heights are within the valid range.")
    }
    
    return(outliers)
}



is_valid_wt <- function(data, weight) {
    valid_range <- c(90, 400)  # Define the valid range for weights
    pattern = "^\\d+(\\.\\d+)?$"
    
    outliers = data %>% 
        filter(!grepl(pattern, weight) | weight < valid_range[1] | weight > valid_range[2]) %>% 
        select(q2, weight) %>%
        drop_na()
    
    
    if (nrow(outliers) > 0) {
        message("Possible outliers found in weights")
    } else {
        message("All weights are within the valid range.")
    }
    
    return (outliers)
 
}


is_valid_bmi <- function(data, bmi) {
    valid_range <- c(15, 40)  # Define the valid range for BMI
    pattern = "^\\d+(\\.\\d+)?$"
    
    outliers <- data %>%
        filter(!grepl(pattern, bmi) | bmi < valid_range[1] | bmi > valid_range[2]) %>%
        select(q2, bmi) %>%
        drop_na()
    
    if (nrow(outliers) > 0) {
        message("Possible outliers found in BMI values")
    } else {
        message("All BMI values are within the valid range.")
    }
    
    return(outliers)
}





get_psqi_scores = function(data) {
    
    PSQI_COMP_SCORES = paste0("psqi_score_c",1:7,sep="")
    
    #component 1: subjective sleep quality
    data$psqi_score_c1 = data$psqi_6
    
    
    #component 2: sleep latency
    data = data %>% 
        mutate(
            psqi_2_cat = case_when(
                psqi_2 <= 15 ~ 0,
                psqi_2 >= 16 & psqi_2 <= 30 ~ 1,
                psqi_2 >= 31 & psqi_2 <= 60 ~ 2,
                psqi_2 > 60 ~ 3
            )
        )
    
    
    data$psqi_2pls5a <- rowSums(data[c("psqi_2_cat", "psqi_5a")], na.rm = TRUE)
    
    data = data %>%
        mutate(psqi_score_c2 = case_when(
            psqi_2pls5a == 0 ~ 0,
            psqi_2pls5a >= 1 & psqi_2pls5a <= 2 ~ 1,
            psqi_2pls5a >= 3 & psqi_2pls5a <= 4 ~ 2,
            psqi_2pls5a >= 5 & psqi_2pls5a <= 6 ~ 3
        ))
    
    
    #component 3: sleep duration
    data = data %>%
        mutate(
            psqi_4_cat = case_when(
                psqi_4 > 7 ~ 0,
                psqi_4 >= 6 & psqi_4 <= 7 ~ 1,
                psqi_4 >= 5 & psqi_4 < 6 ~ 2,
                psqi_4 < 5 ~ 3
            )
        )
    
    data$psqi_score_c3 = data$psqi_4_cat
    
    #component 4: habitual sleep efficiency 
    data$psqi_hours_in_bed = ifelse(data$psqi_3 >= data$psqi_1, 
                                  data$psqi_3 - data$psqi_1, 
                                  data$psqi_3 + 24*60*60 - data$psqi_1) / (60*60)
    
    
    data$psqi_habitual_sleep_eff = (data$psqi_4/data$psqi_hours_in_bed)*100
    
    data = data %>%
        mutate(psqi_score_c4 = case_when(
            psqi_habitual_sleep_eff > 85 ~ 0,
            psqi_habitual_sleep_eff >= 75 & psqi_habitual_sleep_eff <= 84 ~ 1,
            psqi_habitual_sleep_eff >= 65 & psqi_habitual_sleep_eff <= 74 ~ 2,
            psqi_habitual_sleep_eff < 65 ~ 3,
            TRUE ~ NA_integer_
        ))
    
    
    #component 5: sleep disturbances
    data$psqi_5btoj = rowSums(data[, paste0("psqi_5", letters[2:10])], na.rm = TRUE)
    
    
    data$psqi_score_c5 = case_when(
        data$psqi_5btoj == 0 ~ 0,
        data$psqi_5btoj >= 1 & data$psqi_5btoj <= 9 ~ 1,
        data$psqi_5btoj >= 10 & data$psqi_5btoj <= 18 ~ 2,
        data$psqi_5btoj >= 19 & data$psqi_5btoj <= 27 ~ 3,
        TRUE ~ NA_integer_
    )
    
    #component 6: using sleep medication
    data$psqi_score_c6 = data$psqi_7
    
    data$psqi_8pls9 <- rowSums(data[, c("psqi_8", "psqi_9")], na.rm = TRUE)
    
    #component 7: daytime dysfunction
    data = data %>%
        mutate(psqi_score_c7 = case_when(
            psqi_8pls9 == 0 ~ 0,
            psqi_8pls9 >= 1 & psqi_8pls9 <= 2 ~ 1,
            psqi_8pls9 >= 3 & psqi_8pls9 <= 4 ~ 2,
            psqi_8pls9 >= 5 & psqi_8pls9 <= 6 ~ 3,
            TRUE ~ NA_integer_
        ))
    
    #global psqi score
    data = data %>%
        mutate(psqi_score_global = rowSums(.[PSQI_COMP_SCORES], na.rm = TRUE),
               psqi_score_global_cat = case_when(psqi_score_global >= 0 & psqi_score_global <=  5 ~ "Good sleep quality (PSQI ≤ 5)",
                                                 psqi_score_global > 5 & psqi_score_global <=  21 ~ "Poor sleep quality (PSQI > 5)"))
    
    
    
}



get_brisc_scores = function(data){
    
    brisc_vars=paste0("brisc_",1:4,sep="")
    
    data$brisc_score = rowMeans(data[, brisc_vars], na.rm=TRUE)
    
    data = data %>% mutate(brisc_score_cat = case_when(
        brisc_score >= 0 &  brisc_score <= 1.49 ~ "low control",
        brisc_score >= 1.50 &  brisc_score <= 2.99 ~ "some control",
        brisc_score >= 3.00 &  brisc_score <= 4.00 ~ "high control"))
    
}

get_stopbang_scores = function(data){
    
    stopbang_vars = c("stop_1", "stop_2", "stop_3", "stop_4", "bang_1", "bang_2", 
                      "bang_3", "bang_4")
    
    stop_vars = paste0("bang_", 1:4, sep="")
    
    data$stopbang_score = rowSums(data[, stopbang_vars], na.rm=TRUE)
    data$stop_score = rowSums(data[, stop_vars], na.rm=TRUE)
    
    data = data %>% mutate(
        bangosa_score_cat = case_when(
            stopbang_score >=0 & stopbang_score <= 2 ~ "Low risk",
            stopbang_score >= 3 & stopbang_score <= 4 ~ "Moderate risk",
            stopbang_score >= 5  & stopbang_score <= 8 ~ "High risk",
            stop_score >= 2 & bang_4 == 1 ~ "High risk",
            stop_score >= 2 & bang_1 == 1 ~ "High risk",
            stop_score >= 2 & bang_3 == 1 ~ "High risk",
            TRUE ~ NA_character_
        )
    )
}



get_sleep_med <- function(med) {
    med <- tolower(med)
    
    M1 <- "zolpidem"
    M2 <- "suvorexant"
    M3 <- "doxepin"
    M4 <- "estazolam"
    M5 <- "eszopiclone"
    M6 <- "ramelteon"
    M7 <- "temazepam"
    M8 <- "triazolam"
    M9 <- "zaleplon"
    M10 <- "amitriptyline"
    M11 <- "mirtazapine"
    M12 <- "trazodone"
    M13 <- "seroquel"
    M14 <- "benadryl"
    M15 <- "melatonin"
    M16 <- "other"

    result <- case_when(
        str_detect(med, M1) ~ 1,
        str_detect(med, M2) ~ 2,
        str_detect(med, M3) ~ 3,
        str_detect(med, M4) ~ 4,
        str_detect(med, M5) ~ 5,
        str_detect(med, M6) ~ 6,
        str_detect(med, M7) ~ 7,
        str_detect(med, M8) ~ 8,
        str_detect(med, M9) ~ 9,
        str_detect(med, M10) ~ 10,
        str_detect(med, M11) ~ 11,
        str_detect(med, M12) ~ 12,
        str_detect(med, M13) ~ 13,
        str_detect(med, M14) ~ 14,
        str_detect(med, M15) ~ 15,
        str_detect(med, M16) ~ 16,
        TRUE ~ NA
    )
    
    return(result)
}
    

get_sleep_where <- function(where) {
    where <- tolower(where)
    
    W1 <- "residential"
    W2 <- "shelter"
    W3 <- "homeless"
    W4 <- "family"
    W5 <- "spouse"
    W6 <- "own"
    
    result <- case_when(
        str_detect(where, W1) ~ 1,
        str_detect(where, W2) ~ 2,
        str_detect(where, W3) ~ 3,
        str_detect(where, W4) ~ 4,
        str_detect(where, W5) ~ 5,
        str_detect(where, W6) ~ 6,
        TRUE ~ NA)
    
    return(result)
}

get_sleep_who <- function(who) {
    who <- tolower(who)
    
    W1 <- "alone"
    W2 <- "roommate"
    W3 <- "spouse"
    W4 <- "children"
    W5 <- "family"
    W6 <- "other"
    
    result <- case_when(
        str_detect(who, W1) ~ 1,
        str_detect(who, W2) ~ 2,
        str_detect(who, W3) ~ 3,
        str_detect(who, W4) ~ 4,
        str_detect(who, W5) ~ 5,
        str_detect(who, W6) ~ 6,
        TRUE ~ NA)
    
    return(result)
}


    
# CLEANING BASELINE SLEEP DATA -----------------------------------------------------
sleep = survey_numeric 

names(sleep) = c("start_date", "end_date", "status", "ip_address", "progress", 
                 "duration_in_seconds", "finished", "recorded_date", "response_id", 
                 "recipient_last_name", "recipient_first_name", "recipient_email", 
                 "external_reference", "location_latitude", "location_longitude", 
                 "distribution_channel", "user_language", "q1", "q2", "location", 
                 "q3", "q4", "q5", "q6", "q7", "q8_1", "q8_2", "q8_3", "q8_4", 
                 "q8_5", "q8_6", "q8_7", "q8_8", "q8_9", "q8_10", "q8_10_text", 
                 "q8_11", "q8_12", "q9", "q10", "q11", "q20_1", "q20_2", "q20_3", 
                 "q20_4", "q21", "q22", "q23", "q24", "q25", "q26", "q27", "q28", 
                 "q29", "q30", "q31", "q32", "q33", "q34", "q35", "q36", "q12", 
                 "q13", "q14", "q15", "q16_1", "q16_2", "q16_3", "q16_4", "q16_5", 
                 "q16_6", "q16_7", "q16_8", "q16_9", "q16_10", "q16_10_text", 
                 "q16_11", "q16_12", "q17", "q18", "q19", "sc1", "sc2", "sc5")        



#using labels for multiselect questions
sleep$q10 = survey_label$Q10
sleep$q35 = survey_label$Q35
sleep$q36 = survey_label$Q36
sleep$q18 = survey_label$Q18

nrow(sleep)
table(sleep$q3)


#creating two dataframes based on survey type
sleep_bl = sleep %>% filter(q3 == 1)  
sleep_fu = sleep %>% filter(q3 == 2)


sleep_bl = sleep_bl[,colSums(is.na(sleep_bl))<nrow(sleep_bl)]
sleep_fu = sleep_fu[,colSums(is.na(sleep_fu))<nrow(sleep_fu)]

vars_bl = names(sleep_bl)
vars_fu = names(sleep_fu)

setdiff(vars_bl, vars_fu)
setdiff(vars_fu, vars_bl)
intersect(vars_fu, vars_bl)

INVALID_IDS = get_invalid_ids(sleep_bl)

#update: new participants since last time
a = survey_num_original$q2
b = sleep_bl$q2
new_ids = setdiff(b,a)


# Some participants entered dates instead of their ID; corrected manually


INVALID_IDS = get_invalid_ids(sleep_bl)
#sleep_bl %>% filter(q2 %in% INVALID_IDS) %>% write.csv("data/temp/invalid_ids.csv")

#of 306 baseline:  6 incomplete, 7 invalid (test responses, NA)
table(sleep_bl$finished)
sleep_bl %>% filter(q2 %in% INVALID_IDS) %>% nrow()

#dropping unfinished surveys and invalid
sleep_bl = sleep_bl %>% 
    filter(finished == 1) %>%
    filter(!q2 %in% INVALID_IDS)
    
#count number missing per row, and add as the first column
sleep_bl = sleep_bl %>%
    mutate(count_na = rowSums(is.na(.))) %>%
    select(q2, count_na, everything())

nrow(sleep_bl)
#check for duplicates
has_dups(sleep_bl)
DUPLICATED_IDS = get_dup_ids(sleep_bl)


#remove duplicates 
#keep survey with the least amount of missing 
#if the same amount of missing, use first survey
sleep_bl = sleep_bl %>%
    mutate(count_na = rowSums(is.na(.))) %>%
    arrange(count_na, recorded_date) %>%
    distinct(q2, .keep_all = TRUE)

sleep_bl$count_na = NULL
nrow(sleep_bl)

# BASELINE BRISC SCORING -----------------------------------------------------------
n=4
old_vars = paste0("q20_",1:n,sep="")
new_vars = paste0("brisc_",1:n,sep="")
sleep_bl = subtract_one(sleep_bl, old_vars, new_vars)
sleep_bl = get_brisc_scores(sleep_bl)

# BASELINE PSQI SCORING ------------------------------------------------------------

is_valid_time_str(sleep_bl$q4, "bedtime")

#check
sleep_bl$psqi_1 = str_to_time(sleep_bl, "q4", "bedtime")

OUTLIER_Q4 = is_valid_time(sleep_bl, time_col=psqi_1, str_col=q4, type="bedtime")

sleep_bl$psqi_2 = sapply(as.character(sleep_bl$q5), str_to_num)

#fixing text answers
sleep_bl$psqi_2 = ifelse(sleep_bl$q5 == "half an hour", 30, sleep_bl$psqi_2)
sleep_bl$psqi_2 = ifelse(sleep_bl$q5 == "couple hours", 120, sleep_bl$psqi_2)



sleep_bl$psqi_3 = str_to_time(sleep_bl, "q6", "waketime")
is_valid_time_str(sleep_bl$q6, type="waketime")

OUTLIER_Q6 = is_valid_time(sleep_bl, time_col=psqi_3, str_col=q6, "waketime")

# Replace "or", "\", and "/" with "-" 
sleep_bl$psqi_4 <- gsub("\\s*or\\s*|\\\\|/", "-", sleep_bl$q7, ignore.case = TRUE)
sleep_bl$psqi_4 <- gsub("\\b(?![-./\\\\])[A-Za-z]+\\b(?![-./\\\\])", "", sleep_bl$psqi_4, perl = TRUE)
sleep_bl$psqi_4 <- gsub("(?<![0-9])-|-{2,}", "-", sleep_bl$psqi_4, perl = TRUE)
sleep_bl$psqi_4 <- gsub("[^-./0-9]", "", sleep_bl$psqi_4)
sleep_bl$psqi_4 <- sapply(as.character(sleep_bl$psqi_4), str_to_num)
table(sleep_bl$q7, sleep_bl$psqi_4)

sleep_bl$psqi_5a = sleep_bl$q8_1 - 1 

n = 10
old_vars = paste0("q8_", 1:n, sep = "")
new_vars = paste0("psqi_5", letters[1:n])
sleep_bl = subtract_one(sleep_bl, old_vars, new_vars)

sleep_bl$psqi_6 = sleep_bl$q11 - 1
sleep_bl$psqi_7 = sleep_bl$q9 - 1
sleep_bl$psqi_8 = sleep_bl$q8_11 - 1
sleep_bl$psqi_9 = sleep_bl$q8_12 - 1

# PSQI COMPONENT SCORING --------------------------------------------------

sleep_bl = get_psqi_scores(sleep_bl)

# BASELINE STOP BANG OSA SCORING ---------------------------------------------------

n = 4
old_vars = paste0("q", c(21:24), sep = "")
new_vars = paste0("stop_", 1:n)
sleep_bl = subtract_one(sleep_bl, old_vars, new_vars)

n=4
old_vars = paste0("q", c(28:31), sep = "")
new_vars = paste0("bang_", 1:n)
sleep_bl = subtract_one(sleep_bl, old_vars, new_vars)
sleep_bl = get_stopbang_scores(sleep_bl)


# BASELINE NON-SCORING VARIABLES ---------------------------------------------------
sleep_bl$date_text = sleep_bl$q1
sleep_bl$participant_id = sleep_bl$q2
sleep_bl$location_tri = sleep_bl$location
sleep_bl$survey_type = sleep_bl$q3
sleep_bl$psqi_5j_text = sleep_bl$q8_10_text

#Fixing Height/Weight (Updated 6/15/2023)
ht_wt = read.csv(here("data","raw","outliers_ht_wt.csv"))
ht_wt$q2 = as.character(ht_wt$q2)
sleep_bl = left_join(sleep_bl, ht_wt, by="q2")

#Update outliers with corrected values. If the not outlier, leave as is. 
sleep_bl$height =  ifelse(!is.na(sleep_bl$q25_clean), sleep_bl$q25_clean, sleep_bl$q25)
sleep_bl$weight =  ifelse(!is.na(sleep_bl$q26_clean), sleep_bl$q26_clean, sleep_bl$q26)
sleep_bl$bmi = sleep_bl$q27
sleep_bl$height = ifelse(sleep_bl$q25 == "5\'11\"", 71, sleep_bl$height)


OUTLIER_Q25 = is_valid_ht(sleep_bl, height)
OUTLIER_Q26 = is_valid_wt(sleep_bl, weight)
OUTLIER_Q27 = is_valid_bmi(sleep_bl, bmi)


sleep_bl$apnea_tri = sleep_bl$q32
sleep_bl$dx_tri = sleep_bl$q33
sleep_bl$cpap_tri = sleep_bl$q34
sleep_bl$psqi_10 = sleep_bl$q10
sleep_bl$sleep_where = sleep_bl$q35
sleep_bl$sleep_who = sleep_bl$q36
sleep_bl$sleep_quality = sleep_bl$sc1
sleep_bl$sleep_latency = sleep_bl$sc2
sleep_bl$sleep_medication = sleep_bl$sc5


# CLEANING FOLLOWUP SHEET -------------------------------------------------

nrow(sleep_fu)

INVALID_IDS = get_invalid_ids(sleep_fu)

sleep_fu %>% 
    filter(q2 %in% INVALID_IDS) %>%
    select(q2, everything()) %>%
    write.csv("invalid_ids_fu.csv")

#dropping unfinished surveys and invalid
sleep_fu = sleep_fu %>% 
    filter(finished == 1) %>%
    filter(!q2 %in% INVALID_IDS)

#count number missing per row, and add as the first column
sleep_fu = sleep_fu %>%
    mutate(count_na = rowSums(is.na(.))) %>%
    select(q2, count_na, everything())


#check for duplicates
has_dups(sleep_fu)
DUPLICATED_IDS = get_dup_ids(sleep_fu)

#remove duplicates 
#keep survey with the least amount of missing 
#if the same amount of missing, use first survey
sleep_fu = sleep_fu %>%
    mutate(count_na = rowSums(is.na(.))) %>%
    arrange(count_na, recorded_date) %>%
    distinct(q2, .keep_all = TRUE)


sleep_fu$count_na = NULL
nrow(sleep_fu)
names(sleep_fu)

# FOLLOWUP BRISC SCORING -----------------------------------------------------------

n=4
old_vars = paste0("q20_",1:n,sep="")
new_vars = paste0("brisc_",1:n,sep="")
sleep_fu = subtract_one(sleep_fu, old_vars, new_vars)
sleep_fu = get_brisc_scores(sleep_fu)

# FOLLOWUP PSQI SCORING ---------------------------------------------------

#some people accidently completed baseline PSQI form instead of followup PSQI
#for people who have missing value in followup, we will fill in the value from the other column.

BL_PSQI_COLS = c(paste0("q",4:7),paste0("q8_",1:12),"q9","q11")
FU_PSQI_COLS = c(paste0("q",12:15),paste0("q16_",1:12),"q17","q19")

columns_to_replace <- FU_PSQI_COLS  # columns with NAs
replacement_columns <- BL_PSQI_COLS # columns used to fill in NAs

for (i in seq_along(columns_to_replace)) {
    col <- columns_to_replace[i]
    replacement_col <- replacement_columns[i]
   # print(paste(col, replacement_col))
    sleep_fu[[col]] <- ifelse(is.na(sleep_fu[[col]]), sleep_fu[[replacement_col]], sleep_fu[[col]])
}

is_valid_time_str(sleep_fu$q12, "bedtime")


sleep_fu$psqi_1 = str_to_time(sleep_fu, "q12", "bedtime")


OUTLIER_Q12 = is_valid_time(sleep_fu, time_col=psqi_1, str_col=q12, type="bedtime")

sleep_fu$psqi_2 = sapply(as.character(sleep_fu$q13), str_to_num)

is_valid_time_str(sleep_fu$q14, type="waketime")
sleep_fu$psqi_3 = str_to_time(sleep_fu, "q14", "waketime")
OUTLIER_Q14 = is_valid_time(sleep_fu, time_col=psqi_3, str_col=q14, type="waketime")



sleep_fu$psqi_4 <- gsub("\\s*or\\s*|\\\\|/", "-", sleep_fu$q15, ignore.case = TRUE)
sleep_fu$psqi_4 <- gsub("\\b(?![-./\\\\])[A-Za-z]+\\b(?![-./\\\\])", "", sleep_fu$psqi_4, perl = TRUE)
sleep_fu$psqi_4 <- gsub("(?<![0-9])-|-{2,}", "-", sleep_fu$psqi_4, perl = TRUE)
sleep_fu$psqi_4 <- gsub("[^-./0-9]", "", sleep_fu$psqi_4)
sleep_fu$psqi_4 <- sapply(as.character(sleep_fu$psqi_4), str_to_num)
table(sleep_fu$q15, sleep_fu$psqi_4)


sleep_fu$psqi_5a = sleep_fu$q16_1 - 1 

n = 10
old_vars = paste0("q16_", 1:n, sep = "")
new_vars = paste0("psqi_5", letters[1:n])
sleep_fu = subtract_one(sleep_fu, old_vars, new_vars)


sleep_fu$psqi_6 = sleep_fu$q19 - 1
sleep_fu$psqi_7 = sleep_fu$q17 - 1
sleep_fu$psqi_8 = sleep_fu$q16_11 - 1
sleep_fu$psqi_9 = sleep_fu$q16_12 - 1

# FOLLOWUP PSQI COMPONENT SCORING -----------------------------------------

sleep_fu = get_psqi_scores(sleep_fu)


# FOLLOWUP NONSCORING VARIABLES -------------------------------------------

sleep_fu$date_text = sleep_fu$q1
sleep_fu$participant_id = sleep_fu$q2
sleep_fu$location_tri = sleep_fu$location
sleep_fu$survey_type = sleep_fu$q3

sleep_fu$psqi_5j_text = sleep_fu$q16_10_text



sleep_fu$psqi_10 = sleep_fu$q18
sleep_fu$sleep_where = sleep_fu$q35
sleep_fu$sleep_who = sleep_fu$q36


sleep_fu$sleep_quality = sleep_fu$sc1
sleep_fu$sleep_latency = sleep_fu$sc2
sleep_fu$sleep_medication = sleep_fu$sc5



# MERGING SURVEYS ---------------------------------------------------------

KEEP_COLS = names(sleep_bl)[c(2:14,61:128)]
sleep_bl = sleep_bl %>% select(all_of(KEEP_COLS))

KEEP_COLS = names(sleep_fu)[c(2:14,66:116)]
sleep_fu = sleep_fu %>% select(all_of(KEEP_COLS))

a = names(sleep_bl)
b = names(sleep_fu)
intersect(a,b)
setdiff(a,b)
setdiff(b,a)

sleep_both = bind_rows(sleep_bl, sleep_fu)
sleep_both = sleep_both %>% select(participant_id, survey_type, everything())


# CLEANING MULTISELECT QUESTIONS ------------------------------------------
sleep_both = sleep_both %>% mutate(psqi_10_multi = case_when(grepl(",", sleep_both$psqi_10) ~ 1,
                                    !is.na(psqi_10) ~ 0,
                                    TRUE ~ NA))

sleep_both$psqi_10_copy = sleep_both$psqi_10

sleep_both = sleep_both %>%
    separate(psqi_10_copy, into = c("psqi_10_med_1", "psqi_10_med_2", "psqi_10_med_3"), sep = ",") 

sleep_both$psqi_10_med_1 = get_sleep_med(sleep_both$psqi_10_med_1)
sleep_both$psqi_10_med_2 = get_sleep_med(sleep_both$psqi_10_med_2)
sleep_both$psqi_10_med_3 = get_sleep_med(sleep_both$psqi_10_med_3)

sleep_both$sleep_where_copy = sleep_both$sleep_where


sleep_both <- sleep_both %>%
    mutate(sleep_where_copy = gsub("(street), (park), (bus/train station), (abandoned building)|(halfway house), (sober house), (program)",
                         "\\1 \\2 \\3 \\4\\5 \\6 \\7", sleep_where_copy, perl = TRUE))



sleep_both = sleep_both %>% mutate(sleep_where_multi = case_when(grepl(",", sleep_both$sleep_where_copy) ~ 1,
                                                             !is.na(sleep_where_copy) ~ 0,
                                                             TRUE ~ NA))

sleep_both = sleep_both %>%
    separate(sleep_where_copy, into = c("sleep_where_1", "sleep_where_2"), sep = ",") 


sleep_both$sleep_where_1 = get_sleep_where(sleep_both$sleep_where_1)
sleep_both$sleep_where_2 = get_sleep_where(sleep_both$sleep_where_2)

sleep_both$sleep_who_copy = sleep_both$sleep_who

sleep_both = sleep_both %>% mutate(sleep_who_multi = case_when(grepl(",", sleep_both$sleep_who_copy) ~ 1,
                                                                 !is.na(sleep_who_copy) ~ 0,
                                                                 TRUE ~ NA))


sleep_both = sleep_both %>%
    separate(sleep_who_copy, into = c("sleep_who_1", "sleep_who_2"), sep = ",") 


sleep_both$sleep_who_1 = get_sleep_who(sleep_both$sleep_who_1)
sleep_both$sleep_who_2 = get_sleep_who(sleep_both$sleep_who_2)


# ADDING MORE VARIABLES --------------------------------------------------
sleep_both <- sleep_both %>%
    mutate(psqi_1_hour = str_extract(psqi_1, "\\d{2}")) %>%
    mutate(psqi_1_cat = case_when(
        psqi_1_hour %in% c("18","19","20") ~ "6PM-9PM",
        psqi_1_hour %in% c("21","22","23", "00") ~ "9PM-1AM",
        is.na(psqi_1_hour) ~ NA,
        TRUE ~ "1AM or later"
    ))


sleep_both <- sleep_both %>%
    mutate(psqi_3_hour = str_extract(psqi_3, "\\d{2}")) %>%
    mutate(psqi_3_cat = case_when(
        psqi_3_hour %in% c("03","04","05") ~ "3AM-6AM",
        psqi_3_hour %in% c("06","07","08") ~ "6AM-9AM",
        is.na(psqi_3_hour) ~ NA,
        TRUE ~ "9AM or later"
    ))

sleep_both = sleep_both %>% 
            mutate(psqi_4_7hr = case_when(psqi_4 < 7 ~ 1,
                                           psqi_4 >= 7 ~ 0,
                                           TRUE ~ NA)) %>%
            mutate(psqi_2_30min = case_when(psqi_2 > 30 ~ 1,
                                            psqi_2 <= 30 ~ 0,
                                          TRUE ~ NA)) 
    

table(sleep_both$psqi_4, sleep_both$psqi_4_7hr, exclude=F)

table(sleep_both$psqi_2, sleep_both$psqi_2_30min, exclude=F)


write.csv(sleep_both, here("data/clean/cleaned_sleep_data.csv"), row.names=F)


list_of_datasets <- list("Q4" = OUTLIER_Q4, 
                         "Q6" = OUTLIER_Q6,
                         "Q12" = OUTLIER_Q12,
                         "Q14" = OUTLIER_Q14,
                         "Q25" = OUTLIER_Q25,
                         "Q26" = OUTLIER_Q26,
                         "Q27" = OUTLIER_Q27)

write.xlsx(list_of_datasets, file = "output/outliers.xlsx")
