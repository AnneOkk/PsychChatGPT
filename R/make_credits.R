###########################
## Creating credits SONA ##
###########################

source("R/data_preprocessing.R")

credit_df <- load_sav_files("data/raw_010224") 

df1 <- c(credit_df[[1]]$t1sonaid, credit_df[[2]]$t1sonaid) %>% unique(.)
df2 <- credit_df[[3]]$t2SONAID %>% unique(.)
df3 <- credit_df[[4]]$t3SONAID %>% unique(.)
df4 <- credit_df[[5]]$t4SONAID %>% unique(.)
df5 <- credit_df[[6]]$t5SONAID %>% unique(.)
df6 <- credit_df[[7]]$t6SONAID %>% unique(.)
df7 <- credit_df[[8]]$t7sonaid %>% unique(.)
df8 <- credit_df[[9]]$t7sonaid %>% unique(.)

# Combine the separate lists into one list
combined_list <- list(df1, df2, df3, df4, df5, df6, df7, df8)

# Merge into a single vector
vec <- unlist(combined_list)

# Count occurrences
count <- table(vec) %>% data.frame(.)

# print the count
count$credits <- count$Freq*0.25

count <- count %>% dplyr::mutate(credits = ifelse(credits == 2, credits+0.5, credits))

# Die naechsten Listen in der ersten Januarwoche und in der letzten Januarwoche 
library(readxl)
df_131223 <- readxl::read_excel("data/credits/credits_131223.xlsx")
df_130124 <- count[!count$vec %in% df_131223$id, ]
df_130124_sona <- df_130124[grepl("^\\d{4}$", df_130124$vec), ]
library(openxlsx)
write.xlsx(df_130124_sona, 'docs/df_130124.xlsx')


