
# Data preprocessing ------------------------------------------------------

load_excel_files <- function(directory){
  library(readxl)
  
  files <- list.files(path = directory, pattern = "*.xlsx$|*.xls$")
  
  data_list <- lapply(files, function(x){
    df <- read_excel(file.path(directory, x))[-1, ]
  })
  
  names(data_list) <- files
  
  data_list
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

df <- mydata[[1]]
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
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "pi.ai", "Pi.ai"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "^Pi$", "Pi.ai"))) %>%
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "characterai", "character.ai"))) %>%
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "Character.ai", "character.ai"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "CharacterAI", "character.ai"))) %>% 
    
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "GoogleBard", "Bard"))) %>% 
    mutate(across(all_of(tool_cols), ~str_replace_all(.x, "BardAI", "Bard"))) %>% 
    
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


# Create new wide df ------------------------------------------------------

join_dfs_by_id <- function(df_list) {
  # Join all dataframes by id
  df_joined <- Reduce(function(x, y) merge(x, y, by = "id", all = TRUE), df_list)
  return(df_joined)
}


# Dealing with missingness ------------------------------------------------
fill_na_duplicates <- function(df) {
df <- df %>%
  # Group by 'id'
  group_by(id) %>%
  # Use `summarise_all` to combine information from all columns
  summarise_all(~ifelse(any(!is.na(.)), na.omit(.), NA))
return(df)
}

