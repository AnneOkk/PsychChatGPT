
# Data preprocessing on list of dataframes ------------------------------------------------------

load_excel_files <- function(directory){
  library(readxl)
  
  files <- list.files(path = directory, pattern = "*.xlsx$|*.xls$")
  
  data_list <- lapply(files, function(x){
    df <- read_excel(file.path(directory, x))[-1, ]
  })
  
  names(data_list) <- files
  
  data_list
}

load_sav_files <- function(directory){
  library(haven)
  
  files <- list.files(path = directory, pattern = "*.sav$")
  
  data_list <- lapply(files, function(x){
    df <- read_sav(file.path(directory, x))
  })
  
  names(data_list) <- files
  
  data_list
}


transform_date <- function(df) {
  df$startdate <- as.numeric(df$startdate) 
  df$startdate <- as.Date(df$startdate, origin = "1899-12-30") # Transforms the date column
  return(df)
} 


create_id_column <- function(df, ID){
  library(dplyr)
  library(tidyr)
  
  id_cols <- grep("t[1-8]ID[1-4]", names(df), value = TRUE)
  
  if(length(id_cols) == 0){
    message("No columns match the pattern, no ID column created")
    return(df)
  }
  
  df <- df %>%
    unite(ID, id_cols, remove = FALSE)
  
  return(df)
}


combineT1s <- function(data_list) {
  
  # Check if the list has at least two elements
  if(length(data_list) < 2) {
    stop("The data list should contain at least two data frames")
  }
  
  # Combine the first two data frames
  NLP_T1 <- rbind(data_list[[1]], data_list[[2]])
  
  # Replace the first data frame with the combined one
  data_list[[1]] <- NLP_T1
  
  # Remove the second data frame
  data_list <- data_list[-2]
  
  return(data_list)
}


remove_cols <- function(df) {
  patterns <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration", "Finished",
                "RecordedDate", "ResponseId", "Recipient", "Location", "ExternalReference",
                "DistributionChannel", "UserLanguage", "birthMMDD", "SC0", "first2letters",
                "SONA ID", "Weekly Goal", "SONA$", "t[1-9]ID")
  
  df <- df %>%
    select(!matches(patterns))
  
  return(df)
}

remove_cols2 <- function(df) {
  patterns <- c("sonaid", "t1comments", "sona_id", "weekly_goal")
  
  df <- df %>%
    select(!matches(patterns))
  
  return(df)
}


transform_notoolused <- function(df) {
  library(stringr)
  
  # Find the column that matches the pattern
  pattern_col <- grep("t[1-9]toolused", names(df), value = TRUE)
  
  # If no match found return the original data frame
  if (length(pattern_col) == 0) return(df)
  
  # Change all instances of "not applicable" or "NA" to NA
  df <- df %>%
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "1. ChatGPT 2. Bard 3. SciteAI", "ChatGPT, Bard, SciteAI"))) %>%
    
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "Chatgpt humata", "ChatGPT, Humata"))) %>%
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "Github Copilot Chat", "GitHub, Copilot, ChatGPT"))) %>%
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "chatgpt, github copilot", "ChatGPT, GitHub, Copilot"))) %>%
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "GitHub Copilot, ChatGPT", "GitHub, Copilot, ChatGPT"))) %>%
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "ChatGPT Nova", "ChatGPT, Nova"))) %>%
    
    
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "GitHub Copilot Chat", "GitHub, Copilot, ChatGPT"))) %>% 
    mutate(across(all_of(pattern_col), ~str_replace_all(.x, "Copilot/Bing Chat", "Copilot, Bing"))) %>% 
    mutate_at(vars(pattern_col), ~ifelse(str_detect(.x, "applicabe|applicale|applicaple|zutreffend|apllicable|plicable|NA|icht anwendbar"), NA, .x))
  
  return(df)
}


replace_ID_NA <- function(df){
  library(dplyr)
  
  # Find the column that matches the pattern
  sonaid_col <- grep("sonaid", names(df), value = TRUE)
  
  # If no match found return the original data frame
  if (length(sonaid_col) == 0) return(df)
  
  df <- df %>%
    mutate(id = ifelse(id == "NA_NA_NA", !!rlang::sym(sonaid_col), id))
  
  return(df)
}


remove_na_rows <- function(df) {
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  return(df)
}

replace_column_namesTool <- function(df) {
  
  # Rename the column
  df <- df %>%
    rename_with(~ gsub("Tool", "toolused", .x), .cols = contains("Tool"))
  
  return(df)
}

replace_id <- function(df, sonaid_col) {
  df$id[grepl("^\\d{4}$", df[[sonaid_col]])] <- df[[sonaid_col]][grepl("^\\d{4}$", df[[sonaid_col]])]
  return(df)
}

# Function to replace "__" with NA
replace_na_id <- function(df) {
  df$id[df$id == "__"] <- NA
  return(df)
}

separate_entries_toolused <- function(df) {
  library(tidyr)
  library(dplyr)
  
  tool_col <- grep("t[1-9]toolused", names(df), value = TRUE)
  
  if(length(tool_col) == 0) {
    return(df)
  }
  
  n <- df %>% 
    pull(!!rlang::sym(tool_col)) %>%  
    strsplit(",") %>% 
    lengths() %>% 
    max()
  
  new_cols <- paste0(tool_col, "_", seq_len(n))
  
  df <- df %>%
    separate(col = tool_col, 
             into = new_cols, 
             sep = ",", 
             fill = "right", 
             convert = TRUE)
  
  return(df)
}

transform_entries_toolused <- function(df){
  library(dplyr)
  library(stringr)
  
  # Find the columns that match the pattern
  tool_cols <- grep("t[1-9]toolused", names(df), value = TRUE)
  
  # Change all instances of "chatgpt" to "ChatGPT"
  df <- df %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Deepl", "DeepL"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "DeepLwrite", "DeepL"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "DeepLAPI", "DeepL"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "DeepLWrite", "DeepL"))) %>%

    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ScieteAI", "SciteAI"))) %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Notion-KI", "NotionAI"))) %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Bing Ai ", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Bing Ai", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BingChat", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BingAI", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BingAi", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "bingAI", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "bingai", "BingAI"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BingAI", "BingAI"))) %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "pi.ai", "Pi.ai"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "^Pi$", "Pi.ai"))) %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "characterai", "character.ai"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Character.ai", "character.ai"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CharacterAI", "character.ai"))) %>% 
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GoogleBard", "Bard"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BardAI", "Bard"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BARD", "Bard"))) %>% 
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "PerplrxityAI", "Perplexity"))) %>% 
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "OpenAI", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GPT-4ChartGPT", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GPT-4", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGpT", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "HatGPT", "ChatGPT"))) %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChapGPT", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CChatGPT", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGTP", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGPD", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CHatGPT", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chatgbt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "chatgpt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chatgpt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chatgtp", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chat GPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChartGPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGBT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGBt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chatgpt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GPT-4 ChartGPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GPT-4 ChatGPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CHATGPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CHATGTP", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chat gpt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CHat GPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chargpt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGpt", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Chat GPD", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "chatGPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "chatGPT", "ChatGPT"))) %>% 
    
    mutate(across(all_of(tool_cols), ~na_if(.x, "..")))  %>% 
    mutate(across(all_of(tool_cols), ~na_if(.x, "LanguageTool"))) %>% 
    mutate(across(all_of(tool_cols), ~na_if(.x, "/"))) %>% 
    
    

  
  return(df)
}


remove_white_spaces <- function(df) {
  library(dplyr)
  library(stringr)
  
  # Find the columns that match the pattern
  tool_cols <- grep("t[1-9]toolused", names(df), value = TRUE)
  
  # If no match found return the original data frame
  if (length(tool_cols) == 0) return(df)
  
  # Remove white spaces in columns
  df <- df %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, " ", "")))
  
  return(df)
}


replace_column_namest7t8 <- function(df){
  names(df) <- gsub("t7", "t8", names(df))
  return(df)
}



get_unique_values <- function(data) {
  uniques <- lapply(data, function(df) {
    tool_cols <- grep("toolused", names(df), value = TRUE)
    res <- sapply(df[, tool_cols], unique, USE.NAMES = FALSE)
    return(res)
  })
  uniques <- unname(uniques)
  return(unlist(uniques))
}

to_lower_case <- function(df){
  names(df) <- tolower(names(df))
  return(df)
}


library(stringr)  # for str_detect() function


remove_duplicates <- function(df) {
  df %>%
    distinct(id, .keep_all = TRUE)
}



# Function to reformat entry
reformat_entry <- function(id_old){
  if(nchar(id_old) > 4){
    split = strsplit(gsub("([A-Z]+)([0-9]+)([A-Z]+)", "\\1_\\2_\\3", id_old), "_")
    return(sprintf("%s_%04d_%s", split[[1]][1], as.numeric(split[[1]][2]), split[[1]][3]))
  } else {
    return(id_old)
  }
}

# Function to reformat id column in a dataframe
reformat_id <- function(df){
  df[['id']] = sapply(df[['id']], reformat_entry)
  return(df)
}


capitalize_id <- function(df){
  df[['id']] = toupper(df[['id']])
  return(df)
}

assign_anonymous_ids <- function(df_list) {
  # Create a vector of all unique ids across all dataframes
  all_ids <- unique(unlist(map(df_list, "id")))
  
  # Create a named vector with new ids as values and original ids as names
  new_ids <- setNames(1:length(all_ids), all_ids)
  
  # Inside function to replace original ids with new ids
  replace_with_new_ids <- function(df) {
    df$id <- new_ids[df$id]
    return(df)
  }
  # Apply the function to all dataframes
  return(map(df_list, replace_with_new_ids))
}


scale_selected_predictors <- function(data, predictors) {
  # Check if predictors are in data
  if(!all(predictors %in% colnames(data))) {
    stop("One or more predictors are not present in the data.")
  }
  
  # Only scale selected predictors
  data[predictors] <- lapply(data[predictors], scale)
  
  return(data)
}

# Create new wide df ------------------------------------------------------

join_dfs_by_id <- function(df_list) {
  # Convert dataframes to data.tables
  dt_list <- lapply(df_list, setDT)
  
  # Join all data tables by id
  df_joined <- Reduce(function(x, y) merge(x, y, by = "id", all = TRUE), dt_list)
  
  return(df_joined)
}


# Data preprocessing on wide dataframe ------------------------------------

reverse_likert_five <- function(df, cols) {
  for (col in cols) {
    if (!(paste0(col, "_r") %in% names(df))) {
      df$paste0(col, "_r") <- df$col
      #df[[col]] <- 6 - df[[col]]
    }
  }
  return(df)
}

get_unique_toolnames <- function(df) {
  # Get all column names that contain "toolused_"
  cols <- grep("toolused_", names(df), value = TRUE)
  
  # Combine the values from these columns into a single vector
  combined_values <- unlist(df[, cols])
  
  # Get unique values
  unique_values <- unique(combined_values)
  sorted_values <- sort(unique_values)
  
  return(sorted_values)

}


transform_entries_toolused_wide <- function(df){
  library(dplyr)
  library(stringr)
  
  # Find the columns that match the pattern
  tool_cols <- grep("toolused", names(df), value = TRUE)
  
  # Change all instances of "chatgpt" to "ChatGPT"
  df <- df %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, ".*askyourpdf.*", "ChatGPT"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BARD", "Bard"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "bingai", "BingAI"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "^Bing$", "BingAI"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGPT&Jasper", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatGPTChatGPT", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "^Consensus$", "ConsensusAI"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "DeeLWrite", "DeepL"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GithubCopilotX", "Copilot"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ResearchGPT", "ChatGPT"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "^GPT$", "ChatGPT"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "^SciSpace$", "SciSpaceAI"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "SCITEAI", "SciteAI"))) %>%   
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BingAI", "Bing Chat"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "character.ai", "Character.AI"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ChatSonic", "Chatsonic"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Claude2", "Claude 2"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "ConsensusAI", "Consensus"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GammaAI", "Gamma"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GrammarlyAI", "Grammarly"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GrammarlyAI", "Grammarly"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Kiko", "Kiko.AI"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "NotionAI", "Notion AI"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Pi.ai", "Pi"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "QuoraPoe", "Quora Poe"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "SciSpaceAI", "SciSpace"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "SciteAI", "Scite.AI"))) %>%   
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "WordtuneAI", "Wordtune"))) %>%   
   
    mutate(across(all_of(tool_cols), ~na_if(.x, "not")))  %>% 
    mutate(across(all_of(tool_cols), ~na_if(.x, "notapplicabble")))
  return(df)
}
    

concat_columns_gopro <- function(df) {
  patterns <- c("t2gopro1","t2gopro2","t2gopro3","t3gopro1","t3gopro2","t3gopro3","t4gopro1","t4gopro2","t4gopro3",
                "t5gopro1","t5gopro2","t5gopro3","t6gopro1","t6gopro2","t6gopro3")
  
  # Identify and remove columns containing string "t_8_text" and store in separate data frame
  t_8_text_cols <- grep("_8_text", names(df), value = TRUE)
  t_8_text_df <- df[t_8_text_cols]
  df[t_8_text_cols] <- NULL
  
  for (pattern in patterns) {
    main_part <- substring(pattern, 1, nchar(pattern)-1)
    numeric_ending <- substring(pattern, nchar(pattern))
    
    # Identify the columns match the pattern
    cols <- grep(pattern, names(df), value = TRUE)
    
    # Apply the coalesce function column-wise to replace NA's with the next column's value if present
    df[paste0(main_part, "_", numeric_ending)] <- do.call(dplyr::coalesce, df[cols])
    
    # Drop the original columns
    df[cols] <- NULL
  }
  
  # Add the removed columns back to the data frame
  df <- cbind(df, t_8_text_df)
  
  return(df)
}


concat_columns_toolused <- function(df) {
  # Extracting the maximum toolused forms for each tx
  max_toolused <- sapply(1:8, function(i) max(as.integer(gsub(paste0("^t",i,"toolused_"),"",
                                                              grep(paste0("^t",i,"toolused_"),names(df), value=TRUE)),na.rm = TRUE)
                                              )
  )
                         
# Iterating through t1 to t8
for(i in 1:8) {
 # Generate column names "t1toolused_1" to "t1toolused_10"
 cols <- paste0("t", i, "toolused_", 1:max_toolused[i])  
 
 # Generate new column name "txtoolused"
 new_col <- paste0("t", i, "toolused")
 
 # Checking if the columns exist in the dataframe
 existing_cols <- cols[cols %in% names(df)]
 
 # Apply function to paste non-NA tools used together
 if(length(existing_cols) > 0){
   df[[new_col]] <- apply(df[, existing_cols, drop=FALSE], 1, function(x){
     paste(x[!is.na(x)], collapse= ",")
   })
 }
}

# Handle leading/trailing/single commas arisen from NAs
for(i in 1:8){
 new_col <- paste0("t", i, "toolused")
 if(new_col %in% names(df)){
   df[[new_col]] <- gsub("(, )+", ", ", df[[new_col]])
   df[[new_col]] <- gsub("^,|,$", "", df[[new_col]])
 }
}
  
# Remove the original "txtoolused_n" columns
df <- df[, !grepl("t[0-9]toolused_[0-9]", names(df))]
  

return(df)
}


rename_values_training <-  function(df) {
  
  # Define substitution
  substitution <- c('1' = 'Reading the documentation', 
                    '2' = 'Free online tutorials or courses (e.g., on YouTube)', 
                    '3' = 'Paid online tutorials or courses',
                    '4' = 'Experimentation',
                    '5' = 'Online forums',
                    '6' = 'University course',
                    '7' = 'Other',
                    '8' = 'None')
  
  # List of target columns
  target_cols_t1 <- paste0("t1training_", 1:8)
  target_cols_t7 <- paste0("t7training_", 1:8)
  target_cols_t8 <- paste0("t8training_", 1:8)
  
  target_cols <- c(target_cols_t1, target_cols_t7, target_cols_t8)
  
  # Replace values in each column based on name
  for (target_col in target_cols) {
    col_num <- as.numeric(sub(".+_", "", as.character(target_col)))
    if(target_col %in% colnames(df)) {
      df[[target_col]] <- ifelse(df[[target_col]] == 1, substitution[col_num], df[[target_col]])
    }
  }
  
  return(df)
}

concat_values_training <- function(df) {
  for (i in c(1, 7,8)) {
    new_col_name <- paste0("t", i, "training")
    cols <- paste0(new_col_name, "_", 1:8)
    df[new_col_name] = apply(df[cols], 1, paste, collapse = ', ')
  }
  # now clean up all the t?training_? columns
  training_cols <- grep("^t[1, 7,8]training_[1-8]$", colnames(df), value = TRUE)
  df <- df[ , !(names(df) %in% training_cols)]
  
  return(df)
}


rename_values_task <-  function(df) {
  
  # Define substitution
  substitution <- c('1' = 'Writing or editing essays and papers', 
                    '2' = 'Conducting data analysis', 
                    '3' = 'Finding online information or content',
                    '4' = 'Crafting or editing emails',
                    '5' = 'Generating ideas',
                    '6' = 'Preparing presentations or class speeches',
                    '7' = 'Organization and making study plans',
                    '8' = 'Other')
  
  # List of target columns
  target_cols <- vector()
  for (i in 2:8) { 
    target_cols <- c(target_cols, paste0("t", i, "task_", 1:8))
  }

  # Replace values in each column based on name
  for (target_col in target_cols) {
    col_num <- as.numeric(sub(".+_", "", as.character(target_col)))
    if(target_col %in% colnames(df)) {
      df[[target_col]] <- ifelse(df[[target_col]] == 1, substitution[col_num], df[[target_col]])
    }
  }
  
  return(df)
}


concat_values_task <- function(df) {
  for (i in 2:6) {
    new_col_name <- paste0("t", i, "task")
    cols <- paste0(new_col_name, "_", 1:8)
    df[new_col_name] = apply(df[cols], 1, paste, collapse = ', ')
  }
  # now clean up all the t?training_? columns
  task_cols <- grep("^t[2:6]task[1-8]$", colnames(df), value = TRUE)
  df <- df[ , !(names(df) %in% task_cols)]
  
  # Remove the original "txtoolused_n" columns
  df <- df[, !grepl("t[2-6]task_[1-8]", names(df))]
  return(df)
}

normalize_countries <- function(df, text_column, num_column) {
  replacements <- c("United States", "United States", "United States",
                    "China", "Turkey", "Netherlands", "England", "Netherlands",
                    "China", "Greece", "Turkey", "Netherlands", "United States", "Bulgaria")
  corrected_text <- replacements[match(df[[text_column]], c("US", "USA", "America", "china", 
                                                            "Turkey", "The Netherlands", "England",
                                                            "Netherlands", "China", "Greece",
                                                            "Turkey", "Netherlands", "United States",
                                                            "and Bulgaria"))]
  
  values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  country_names <- c("Germany", "Austria", "Switzerland", "Greece", "Turkey", "Netherlands", 
                     "England", "China", "United States", "Bulgaria")
  
  df[[num_column]] <- ifelse(df[[num_column]] == 4, values[match(corrected_text, country_names)], df[[num_column]])
  
  df
}

normalize_studies <- function(df, text_column, num_column) {
  replacements <- c(NA, "Computational Linguistics", "Social Sciences", "Computational Linguistics",
                    "Social Sciences", "Social Sciences", "Social Sciences", "Social Sciences", "Social Sciences", "Social Sciences", 
                    "Social Sciences", "Business", "Social Sciences", "Social Sciences", "Social Sciences", "Social Sciences", 
                    "Social Sciences", "Neuroscience", "Social Sciences", "Social Sciences", "Education", "Computer Science", 
                    "Social Sciences", "Education", "Education", "Education")
  
  corrected_text <- replacements[match(df[[text_column]], c(NA, "Computational Linguistics", 
                                                            "Psychology", "German studies and Computational Linguistics",       
                                                            "Business Psychology (Wirtschaftspsychologie)", "Business Psychology",                                
                                                            "Business psychology", "Wirtschaftspsychologie",                             
                                                            "business psychology", "business psychologie",  
                                                            "Business & Psychology", "Business",          
                                                            "buisness psychology", "Businesspsychology",                              
                                                            "Business and Psychology", "Economics Psychology",                               
                                                            "Bussinespsychologi", "Neuroscience",
                                                            "Business Psychology: mix of Business and Psychology", "Wipsy",
                                                            "Lehramt Grundschule", "Computer Science",                                    
                                                            "Psychologie", "Education, School Psychology",
                                                            "Education, Psychology", "Teacher"))]
  
  values <- c(1, 2, 2, 3, 4, 4, 5, 6, 7, 8, 9, 9, 10, 11, 12)
  study_names <- c("Engineering", "Psychology", "Social Sciences", "Humanities", "Business & Law",
                   "Business", "Medicine & Health", "Natural Sciences", "Arts & Design",
                   "Language & German Studies", "Technical Courses (e.g., Data Science, AI and Machine Learning)",
                   "Computer Science", "Computational Linguistics", 
                   "Neuroscience", "Education")
  
  df[[num_column]] <- ifelse(df[[num_column]] == 10, values[match(corrected_text, study_names)], df[[num_column]])
  
  df
}

normalize_degree <- function(df, text_column, num_column) {
  replacements <- c(NA, NA, "Bachelor", "State Examination", "State Examination", 
                    "State Examination", "Bachelor", "State Examination", "State Examination")
  
  corrected_text <- replacements[match(df[[text_column]], c(NA, "Abgeschlossene Ausbildung", "Business Psychology",
                                                            "Staatsexamen", "State examination (Staatsexamen)", 
                                                            "state examination", "First degree", "Lehramt (state exam)", 
                                                            "State exam"))]
  
  values <- c(1, 2, 3, 4)
  study_names <- c("Bachelor", "Master", "PhD", "State Examination")
  
  df[[num_column]] <- ifelse(df[[num_column]] == 4, values[match(corrected_text, study_names)], df[[num_column]])
  
  df
}


# Data processing on composite dataframe ------------------------------------------------
#' Function to replace missing values with mean 
# Function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to replace NA values with mode if count of non-NA values >= 3
replace_na_with_mode <- function(df, column_names) {
  row_vals <- df[column_names]
  if (sum(!is.na(row_vals)) >= 3) {
    row_vals[is.na(row_vals)] <- Mode(row_vals)
  }
  row_vals
}


replace_na_with_mean <- function(df, column_names) {
  row_vals <- df[column_names]
  if (sum(!is.na(row_vals)) >= 3) {
    row_vals[is.na(row_vals)] <- mean(row_vals, na.rm = TRUE)
  }
  row_vals
}


mean_replace <- function(df, column_names) {
  new_data <- data.frame(t(apply(df[column_names], 1, replace_na_with_mean, column_names = column_names)))
  colnames(new_data) <- column_names
  df[ , column_names] <- new_data # Merge the new_data dataframe with the original data dataframe
  return(df)
}




# Mice --------------------------------------------------------------------


propplot <- function(x, formula, facet = "wrap", ...) {
  library(ggplot2)
  
  cd <- data.frame(mice::complete(x, "long", include = TRUE))
  cd$.imp <- factor(cd$.imp)
  
  r <- as.data.frame(is.na(x$data))
  
  impcat <- x$meth != "" & sapply(x$data, is.factor)
  vnames <- names(impcat)[impcat]
  
  if (missing(formula)) {
    formula <- as.formula(paste(paste(vnames, collapse = "+",
                                      sep = ""), "~1", sep = ""))
  }
  
  tmsx <- terms(formula[-3], data = x$data)
  xnames <- attr(tmsx, "term.labels")
  xnames <- xnames[xnames %in% vnames]
  
  if (paste(formula[3]) != "1") {
    wvars <- gsub("[[:space:]]*\\|[[:print:]]*", "", paste(formula)[3])
    # wvars <- all.vars(as.formula(paste("~", wvars)))
    wvars <- attr(terms(as.formula(paste("~", wvars))), "term.labels")
    if (grepl("\\|", formula[3])) {
      svars <- gsub("[[:print:]]*\\|[[:space:]]*", "", paste(formula)[3])
      svars <- all.vars(as.formula(paste("~", svars)))
    } else {
      svars <- ".imp"
    }
  } else {
    wvars <- NULL
    svars <- ".imp"
  }
  
  for (i in seq_along(xnames)) {
    xvar <- xnames[i]
    select <- cd$.imp != 0 & !r[, xvar]
    cd[select, xvar] <- NA
  }
  
  
  for (i in which(!wvars %in% names(cd))) {
    cd[, wvars[i]] <- with(cd, eval(parse(text = wvars[i])))
  }
  
  meltDF <- reshape2::melt(cd[, c(wvars, svars, xnames)], id.vars = c(wvars, svars))
  meltDF <- meltDF[!is.na(meltDF$value), ]
  
  
  wvars <- if (!is.null(wvars)) paste0("`", wvars, "`")
  
  a <- plyr::ddply(meltDF, c(wvars, svars, "variable", "value"), plyr::summarize,
                   count = length(value))
  b <- plyr::ddply(meltDF, c(wvars, svars, "variable"), plyr::summarize,
                   tot = length(value))
  mdf <- merge(a,b)
  mdf$prop <- mdf$count / mdf$tot
  
  plotDF <- merge(unique(meltDF), mdf)
  plotDF$value <- factor(plotDF$value,
                         levels = unique(unlist(lapply(x$data[, xnames], levels))),
                         ordered = T)
  
  p <- ggplot(plotDF, aes(x = value, fill = get(svars), y = prop)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(legend.position = "bottom", ...) +
    ylab("proportion") +
    scale_fill_manual(name = "",
                      values = c("black",
                                 colorRampPalette(
                                   RColorBrewer::brewer.pal(9, "Blues"))(x$m + 3)[1:x$m + 3])) +
    guides(fill = guide_legend(nrow = 1))
  
  if (facet == "wrap")
    if (length(xnames) > 1) {
      print(p + facet_wrap(c("variable", wvars), scales = "free"))
    } else {
      if (is.null(wvars)) {
        print(p)
      } else {
        print(p + facet_wrap(wvars, scales = "free"))
      }
    }
  
  if (facet == "grid")
    if (!is.null(wvars)) {
      print(p + facet_grid(paste(paste(wvars, collapse = "+"), "~ variable"),
                           scales = "free"))
    }
}

