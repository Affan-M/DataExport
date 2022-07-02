# Packages
library(tidyverse) # Main data manipulation
library(httr) # Web requests (used for API)
library(jsonlite) # JSON (used for API)
library(glue) # String manipulation
library(zoo) # Date manipulation
library(lubridate) # Date manipulation
library(glue) # Text manipulation
library(readxl) # Read excel files
library(openxlsx) # Write excel files
library(fpp3) # Forecast package

data_start_date <- 1992

source("1 Functions.R")

#### Series IDs ####
series_id_df <- read_excel("Series list.xlsx") %>% filter(!is.na(series_id))

series_id_vec <- series_id_df %>% pull(series_id) %>% unique()

#### User input ####

## Sort out the query db series list and check which requires user input. But ensure that there's a check
## done to see whether there is an existing file and only add any new series. If conversion method has
## already been decided, then it should require no further decision.

sorted_data_list <- Query_DataSort(series_id_list = series_id_vec)

sorted_df <- sorted_data_list[["sorted_list"]]

directory_files <- getwd() %>% list.files() %>% str_detect(., "^Inconvertible.xlsx$") %>% sum()

if (directory_files == 1) {
  
  # Reading the existing file, checking if there are any inconvertible IDs, whether conversion method has been
  # specified or if it is missing
  existing_file <- read_xlsx("Inconvertible.xlsx")
  
  existing_file_id <- existing_file %>% pull(id)
  
  new_inconvertible_list <- list()
  
  new_inconvertible_list[["existing"]] <- existing_file
  
  # Checking the inconvertible dataframe created from the sorted_df function and checking if there are new
  # inconvertible IDs that require a conversion method. If new IDs not present, create an empty dataframe with
  # the same column characteristics, otherwise put the new dataframe in the list
  
  inconvertible_candidates <- sorted_df[["NotAnnual_Inconvertible"]] %>% pull(id)
  
  if (length(inconvertible_candidates[!(inconvertible_candidates %in% existing_file_id)]) >= 1) {
    
    new_inconvertible_list[["new"]] <- sorted_df[["NotAnnual_Inconvertible"]] %>%
      filter(id %in% inconvertible_candidates[!(inconvertible_candidates %in% existing_file_id)])
    
  } else {
    
    new_inconvertible_list[["new"]] <- tibble("id" = integer(),
                                              "name" = character(),
                                              "frequency" = character(),
                                              "freq_convertible" = logical(),
                                              "conversion_method" = character(),
                                              "unit" = character(),
                                              "is_flow" = logical(),
                                              "is_stock" = logical())
    
  }
  
  # Rewrite file if there are any new inconvertibles rearranging the new_inconvertibles at the top of the file
  excel_list <- list()
  
  for (n in names(new_inconvertible_list)) {
    
    if (nrow(new_inconvertible_list[[n]]) >= 1) {
      
      excel_list[[n]] <- new_inconvertible_list[[n]]
      
    }
    
  }
  
  new_excel_file_check <- bind_rows(excel_list)
  
  # Rewrite excel file if there is anything new in the excel file
  if (nrow(new_inconvertible_list[["new"]]) >= 1) {
    
    new_excel_file_check %>% arrange(match(conversion_method, NA)) %>%
      write.xlsx(., file = "Inconvertible.xlsx", overwrite = TRUE, colWidths = "auto")
    
    print("Inconvertible.xlsx updated.")
    
  } else {
    
    print("Inconvertible.xlsx does not require any updates.")
    
  }
  
} else {
  
  write.xlsx(sorted_df[["NotAnnual_Inconvertible"]], file = "Inconvertible.xlsx", overwrite = TRUE, colWidths = "auto")
  
}

# Final check to stop the script if there are any inconvertibles missing.
# Additionally, if no inconvertible is missing, it would ensure that user input is done correctly and
# follow the standards if number of rows in the updated_inconvertible file is more than one.
updated_inconvertibles <- read_excel("Inconvertible.xlsx")

# Replacing the sorted_df with the updated inconvertible gotten from the excel file
sorted_df[["NotAnnual_Inconvertible"]] <- updated_inconvertibles

if (nrow(updated_inconvertibles) >= 1) {
  
  # Check for missing conversion methods in the inconvertibles file
  missing_inconvertibles <- updated_inconvertibles %>% filter(is.na(conversion_method))
  
  if (nrow(missing_inconvertibles) >= 1) {
    
    print("Conversion method not specified for the series below:")
    missing_inconvertibles %>% pull(name) %>% print()
    
    stop("Please fill the Inconvertibles.xlsx with the conversion methods before proceeding.")
    
  }
  
  # Ensure conversion_method column is in the right case (lower case)
  updated_inconvertibles <- updated_inconvertibles %>% mutate(conversion_method = as.character(conversion_method) %>% str_to_lower())
  
  # Check whether conversion method is one of sum, avg, ep
  cm_vec <- updated_inconvertibles %>% pull(conversion_method) %>% unique()
  
  check_cm_vec <- sum(!(cm_vec %in% c("sum", "avg", "ep")))
  
  if (check_cm_vec >= 1) {
    
    stop("Please check the conversion method specified in Inconvertible.xlsx. It should be either sum, avg or ep.")
    
  } else {
    
    print("No issue found in the conversion methods specified in Inconvertible.xlsx. Proceeding with the rest of the process.")
    
  }
  
} else {
  
  print("No inconvertible series found. Proceeding with the rest of the process.")
  
}

#### Data ####

# Remove all the lists that are empty/less than 1 row
sorted_series_id_list <- sorted_df[sapply(sorted_df, function(x) nrow(x) >= 1)]

data_df <- Query_Process(data_list = sorted_data_list[["data_list"]],
                         series_id_list = sorted_series_id_list,
                         target_frequency = "Annual",
                         include_incomplete = FALSE)

tidy_list <- Tidy_db(data_series = data_df,
                     series_id = series_id_df,
                     from = data_start_date)

#### Export ####

# Header style
headerstyle <- createStyle(fontName = "Roboto", fontSize = 10,
                           fgFill = "#0d1d4f", halign = "center", textDecoration = "Bold", fontColour = "white",
                           border = c("top", "left", "right", "bottom"), borderStyle = "medium", borderColour = "black")

# Body style: With decimals
style_dec <- createStyle(fontName = "Roboto", fontSize = 10, fontColour = "black", numFmt = "#,##0.00")

# Body style: Percentage
style_perc <- createStyle(fontName = "Roboto", fontSize = 10, fontColour = "black", numFmt = "0.00%")

# Workbook
wb <- createWorkbook()
# wb <- loadWorkbook(paste0(file_path, "/FP CAEM - MDV.xlsm"))

# Sheet
for (n in names(tidy_list)) {
  
  sheet_name <- paste("R", n)
  
  sh <- addWorksheet(wb, sheet_name)
  
  start_row <- 1
  
  headers <- tidy_list[[n]] %>% select(!category) %>% slice(0)
  
  if (n == "Data") {
    
    header_index <- as_tibble(headers)
    colnames(header_index) <- 1:ncol(headers)
    
    writeData(wb, sh, x = header_index, startCol = 1, startRow = start_row,
              borders = "none", borderStyle = "medium", borderColour = "black",
              headerStyle = style_dec)
    
    start_row <- start_row + 1
    
  }
  
  writeData(wb, sh, x = headers, startCol = 1, startRow = start_row,
            borders = "none", borderStyle = "medium", borderColour = "black",
            headerStyle = headerstyle)
  
  categories <- tidy_list[[n]] %>% distinct(category) %>% pull()
  
  start_row <- start_row + 2
  
  for (c in categories) {
    
    writeData(wb, sh, x = c, startCol = 2, startRow = start_row)
    addStyle(wb, sh, style = headerstyle, cols = 2, rows = start_row, stack = TRUE)
    
    start_row <- start_row + 1
    
    data_tb <- tidy_list[[n]] %>% filter(category == c) %>% select(!category)
    
    writeData(wb, sh, x = data_tb, startCol = 1, startRow = start_row,
              borders = "none", borderStyle = "medium", borderColour = "black",
              colNames = FALSE)
    
    addStyle(wb, sh,
             style = style_dec,
             rows = start_row:(start_row + nrow(data_tb)),
             cols = 3:ncol(data_tb),
             gridExpand = TRUE,
             stack = TRUE)
    
    start_row <- start_row + nrow(data_tb) + 1
    
  }
  
  setColWidths(wb, sh,
               cols = (1:ncol(headers)),
               widths = "auto", hidden = FALSE)
  
  freezePane(wb, sh, firstActiveRow = 3, firstActiveCol = 3)
  
}

saveWorkbook(wb, file = "Data.xlsx", overwrite = TRUE)

print("Done! All data and meta data can be found in Data.xlsx!")
