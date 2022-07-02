# Functions

# This file will contain the main API functions used

Query_DataSort <- function(series_id_list) {
  
  ## This function is meant to sort out all series ID based on whether they are already annual,
  ## and if not, whether it is convertible or not. If not convertible, the resulting list will
  ## then be used to ask for user input on the conversion method to be used.
  
  ## Function arguments:
  # series_id_list:   Series IDs of the series you want. Easiest to check series IDs using Viya/Database/Metafile.
  #                   Can be a vector or a single series ID.
  
  # Setting the authorization header. Change bearer token to your API given by MMA database website.
  bearer_token <- "9|kfbjltsPbaApuvycKBG5R7NK2NHRPvfnob5WuFnF"
  auth_headers <- c(Authorization = glue("Bearer {bearer_token}"))
  
  # Combining links with series IDs
  collapsed_series_id <- paste(series_id_list, collapse = ",")
  link <- glue("https://database.mma.gov.mv/api/series?ids={collapsed_series_id}") %>% as.character()
  
  # Requesting data
  data_list <- list()
  
  page = 1
  
  data_list[[page]] <- GET(url = link, add_headers(.headers = auth_headers)) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON()
  
  next_page <- data_list[[page]][["links"]][["next"]]
  
  while (!is.null(next_page)) {
    
    page = page + 1
    
    data_list[[page]] <- GET(url = next_page, add_headers(.headers = auth_headers)) %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON()
    
    next_page <- data_list[[page]][["links"]][["next"]]
    
  }
  
  # Sorting the list of series IDs
  sorted_list <- list()
  
  for (p in 1:length(data_list)) {
    
    id <- data_list[[p]][["data"]][["id"]]
    
    for (i in id) {
      
      id_tibble <- data_list[[p]][["data"]] %>% as_tibble() %>% filter(id == i)
      
      # Checks whether it is annual, monthly/quarterly or inconvertible
      sort_freq <- id_tibble %>% pull(frequency)
      sort_convertible <- id_tibble %>% pull(freq_convertible)
      
      if (sort_freq == "Annual") {
        
        sorted_list[["Annual"]][[as.character(i)]] <- id_tibble %>%
          select(id, name, frequency, freq_convertible, conversion_method, unit, is_flow, is_stock)
        
      } else if (sort_freq != "Annual" & sort_convertible == FALSE) {
        
        sorted_list[["NotAnnual_Inconvertible"]][[as.character(i)]] <- id_tibble %>%
          select(id, name, frequency, freq_convertible, conversion_method, unit, is_flow, is_stock)
        
      } else if (sort_freq != "Annual" & sort_convertible == TRUE) {
        
        sorted_list[["NotAnnual_Convertible"]][[as.character(i)]] <- id_tibble %>%
          select(id, name, frequency, freq_convertible, conversion_method, unit, is_flow, is_stock)
        
      }
      
    }
    
  }
  
  # Tidying the sorted lists
  for (i in names(sorted_list)) {
    
    sorted_list[[i]] <- bind_rows(sorted_list[[i]])
    
  }
  
  # Creating an empty tibble in the unlikely case that any of the list names are missing. To avoid
  # potential problems down the line as there may be bind_rows on NULL/NA objects otherwise.
  names <- c("Annual", "NotAnnual_Inconvertible", "NotAnnual_Convertible")
  
  for (n in names) {
    
    if (!(n %in% names(sorted_list))) {
      
      sorted_list[[n]] <- tibble("id" = integer(),
                                 "name" = character(),
                                 "frequency" = character(),
                                 "freq_convertible" = logical(),
                                 "conversion_method" = character(),
                                 "unit" = character(),
                                 "is_flow" = logical(),
                                 "is_stock" = logical())
      
    }
    
  }
  
  # Tidying the data list
  processed_data_list <- list()
  
  for (p in 1:length(data_list)) {
    
    id <- data_list[[p]][["data"]][["id"]]
    
    for (i in 1:length(id)) {
      
      # Processing
      selected_id <- id[i]
      name <- data_list[[p]][["data"]][["name"]][i] %>% str_squish()
      
      id_name <- glue("{selected_id} - {name}") %>% as.character()
      
      # data_frequency <- data_list[[p]][["data"]][["frequency"]][i]
      
      processed_data_list[[id_name]] <- data_list[[p]][["data"]][["data"]][[i]] %>%
        as_tibble() %>%
        mutate(name = name,
               id = selected_id)
      
    }
    
  }
  
  processed_data_list <- bind_rows(processed_data_list)
  
  
  sorted_data_list <- list("sorted_list" = sorted_list,
                           "data_list" = processed_data_list)
  
  return(sorted_data_list)
  
}

Query_Process <- function(data_list, series_id_list, target_frequency = NULL, include_incomplete = FALSE) {
  
  ## Function description: Uses query database API to get the data, provides frequency conversion.
  ## In the case of incomplete data, it would also allow you to aggregate up to the most recent period,
  ## or to drop the incomplete period.
  
  ## Function arguments:
  # 
  # data_list:  Data list generated by the Query_DataSort function.
  #
  # series_id_list: List of Series IDs generated by the Query_DataSort function.
  # 
  # frequency:  Specify the frequency you would like the data in. Options for frequency are "Quarterly" and "Annual".
  #             Default is NULL, in which case the data is provided at base website frequency. In the following
  #             cases, there would be a warning provided and a dataframe with base frequency would be provided:
  #               - The data is not convertible
  #               - The data is already in the frequency you want (i.e. available in Quarterly frequency
  #                 and you asked for it to be converted to Quarterly frequency)
  #               - The data is not available in a higher frequency (i.e. available in Quarterly frequency
  #                 and you asked for it to be converted to Monthly frequency)
  # 
  # include_incomplete: Specify whether you would like the aggregate of incomplete periods to be included or dropped.
  #                     By default, only complete quarters or years will be shown. In the case that you want to see
  #                     Year to Date data for example, you would have to specify TRUE for include_incomplete.
  #                     
  # force_convert:      Even though Query database has deemed it inconvertible, you can ask it to be force converted.
  #                     However, you need to provide a conversion method in this case. FALSE by default.
  #                     Not recommended unless certain.
  # 
  # force_coversion_method: Provides a conversion method to the function even though database sees the series as
  #                         inconvertible or has another method specified. Not recommended unless certain.
  #                         Default is NULL. Possible options for this argument are:
  #                           - "sum":  Aggregation by summing
  #                           - "avg":  Aggregation by finding the average
  #                           - "ep":   Aggregation by finding end of the period value
  
  # Processing
  processed_list <- list()
  
  for (n in names(series_id_list)) {
    
    frequencies <- series_id_list[[n]] %>% distinct(frequency) %>% pull()
    
    for (data_frequency in frequencies) {
      
      ids <- series_id_list[[n]] %>% filter(frequency == data_frequency) %>% pull(id)
      
      if (data_frequency == "Monthly") {
        
        df <- data_list %>%
          filter(id %in% ids) %>%
          mutate(date = as.yearmon(date))
        
      } else if (data_frequency == "Quarterly") {
        
        df <- data_list %>%
          filter(id %in% ids) %>%
          mutate(date = as.yearmon(date) %>% as.yearqtr() %>% as.yearmon())
        
      } else if (data_frequency == "Annual") {
        
        df <- data_list %>%
          filter(id %in% ids) %>%
          mutate(date = year(date) %>% as.yearmon())
        
      }
      
      if (!is.null(target_frequency)) {
        
        ### Frequency conversion
        conversion_table <- tibble("Frequency" = c("Monthly", "Quarterly", "Annual"),
                                   "Frequency_numerical" = c(12, 4, 1))
        
        data_frequency_numerical <- conversion_table %>%
          filter(Frequency == data_frequency) %>%
          pull(Frequency_numerical)
        
        target_frequency_numerical <- conversion_table %>%
          filter(Frequency == target_frequency) %>%
          pull(Frequency_numerical)
        
        # Checking if frequency required is lower than the frequency the data is in
        if (target_frequency_numerical < data_frequency_numerical) {
          
          ## Year to date check
          last_year <- df %>% pull(date) %>% year() %>% unique() %>% max() %>% as.character()
          check_dates <- df %>% filter(str_detect(date, last_year)) %>% pull(date)
          
          conversion_methods <- sorted_series_id_list[[n]] %>%
            filter(frequency == data_frequency) %>%
            distinct(conversion_method) %>%
            pull()
          
          converted_list <- list()
          
          for (cm in conversion_methods) {
            
            # Aggregating data to the target frequency Quarterly
            if (target_frequency == "Quarterly") {
              
              # Aggregation based on conversion method
              if (cm == "sum") {
                
                cm_ids <- sorted_series_id_list[[n]] %>%
                  filter(frequency == data_frequency) %>%
                  filter(conversion_method == cm) %>%
                  pull(id)
                
                converted_df <- df %>%
                  filter(id %in% cm_ids) %>%
                  mutate(date = as.yearqtr(date)) %>%
                  group_by(date, name, id) %>%
                  summarise(amount = sum(amount), .groups = "keep") %>%
                  ungroup() %>%
                  mutate(date = as.yearmon(date))
                
              } else if (cm == "avg") {
                
                cm_ids <- sorted_series_id_list[[n]] %>%
                  filter(frequency == data_frequency) %>%
                  filter(conversion_method == cm) %>%
                  pull(id)
                
                converted_df <- df %>%
                  filter(id %in% cm_ids) %>%
                  mutate(date = as.yearqtr(date)) %>%
                  group_by(date, name, id) %>%
                  summarise(amount = mean(amount), .groups = "keep") %>%
                  ungroup() %>%
                  mutate(date = as.yearmon(date))
                
              } else if (cm == "ep") {
                
                cm_ids <- sorted_series_id_list[[n]] %>%
                  filter(frequency == data_frequency) %>%
                  filter(conversion_method == cm) %>%
                  pull(id)
                
                converted_df <- df %>%
                  filter(id %in% cm_ids) %>%
                  mutate(qtr = as.yearqtr(date)) %>%
                  group_by(id, name, qtr) %>%
                  arrange(date) %>%
                  summarise(amount = last(amount), .groups = "drop") %>%
                  mutate(date = as.yearmon(qtr)) %>%
                  select(!qtr)
                
              }
              
              
              if (length(check_dates) %% 3 != 0 & include_incomplete == FALSE) {
                
                drop_period <- converted_df %>% pull(date) %>% unique() %>% max() %>% as.character()
                converted_df <- converted_df %>% filter(!str_detect(date, drop_period))
                
              }
              
              # Aggregating data to the target frequency Annual
            } else if (target_frequency == "Annual") {
              
              # Aggregation based on conversion method
              if (cm == "sum") {
                
                cm_ids <- sorted_series_id_list[[n]] %>%
                  filter(frequency == data_frequency) %>%
                  filter(conversion_method == cm) %>%
                  pull(id)
                
                converted_df <- df %>%
                  filter(id %in% cm_ids) %>%
                  mutate(date = year(date)) %>%
                  group_by(date, name, id) %>%
                  summarise(amount = sum(amount), .groups = "keep") %>%
                  ungroup() %>%
                  mutate(date = as.yearmon(date))
                
              } else if (cm == "avg") {
                
                cm_ids <- sorted_series_id_list[[n]] %>%
                  filter(frequency == data_frequency) %>%
                  filter(conversion_method == cm) %>%
                  pull(id)
                
                converted_df <- df %>%
                  filter(id %in% cm_ids) %>%
                  mutate(date = year(date)) %>%
                  group_by(date, name, id) %>%
                  summarise(amount = mean(amount), .groups = "keep") %>%
                  ungroup() %>%
                  mutate(date = as.yearmon(date))
                
              } else if (cm == "ep") {
                
                cm_ids <- sorted_series_id_list[[n]] %>%
                  filter(frequency == data_frequency) %>%
                  filter(conversion_method == cm) %>%
                  pull(id)
                
                converted_df <- df %>%
                  filter(id %in% cm_ids) %>%
                  mutate(year = year(date)) %>%
                  group_by(id, name, year) %>%
                  arrange(date) %>%
                  summarise(amount = last(amount), .groups = "drop") %>%
                  mutate(date = as.yearmon(year)) %>%
                  select(!year)
                
              }
              
              # Dropping the most recent period if incomplete and include_incomplete is not required (Monthly data)
              if (data_frequency == "Monthly" & length(check_dates) %% 12 != 0 & include_incomplete == FALSE) {
                
                drop_period <- converted_df %>% pull(date) %>% unique() %>% max() %>% as.character()
                converted_df <- converted_df %>% filter(!str_detect(date, drop_period))
                
              }
              
              # Dropping the most recent period if incomplete and include_incomplete is not required (Quarterly data)
              if (data_frequency == "Quarterly" & length(check_dates) %% 4 != 0 & include_incomplete == FALSE) {
                
                drop_period <- converted_df %>% pull(date) %>% unique() %>% max() %>% as.character()
                converted_df <- converted_df %>% filter(!str_detect(date, drop_period))
                
              }
              
            }
            
            converted_list[[cm]] <- converted_df
            
          }
          
          df <- bind_rows(converted_list)
          
        }
        
      }
      
      processed_list[[n]][[data_frequency]] <- df %>% arrange(id, date)
      
    }
    
  }
  
  # Simplifying the list by binding the rows together
  processed_df <- lapply(processed_list, bind_rows) %>% bind_rows() %>%
    select(date, id, name, amount)
  
  # Changing the dates if there is a target frequency provided
  if (!is.null(target_frequency)) {
    
    if (target_frequency == "Quarterly") {
      
      processed_df <- processed_df %>% mutate(date = as.yearqtr(date))
      
    } else if (target_frequency == "Annual") {
      
      processed_df <- processed_df %>% mutate(date = year(date))
      
    }
    
  }
  
  # Creating a dataframe for meta info
  meta_df <- bind_rows(series_id_list) %>%
    select(id, name, unit, frequency, conversion_method) %>%
    mutate(converted_frequency = target_frequency) %>%
    arrange(id)
  
  final_list <- list("Data" = processed_df,
                     "Meta" = meta_df)
  
  return(final_list)
  
}

Tidy_db <- function(data_series, series_id, from = NULL) {
  
  # Tidying the dataframes to do the formatting - getting the denominations we want and the
  # wide format. It would also add denomination to the meta file.
  
  # data_series: the dataframes containing all the data
  # series_id: the excel file containing all the series ID and denominations, etc. that we imported previously
  # from: only displays date after this
  
  series_id_vec <- series_id %>% pull(series_id)
  
  proc_list <- list()
  
  for (s in series_id_vec) {
    
    # Filtered data series
    df <- data_series[["Data"]] %>% filter(id == s)
    
    # Filter series ID's name and denomination that we want
    filtered_series_id <- series_id %>% filter(series_id == s) %>% distinct()
    new_name <- filtered_series_id %>% pull(name)
    denomination <- filtered_series_id %>% pull(denomination)
    
    # In case the denomination is left blank in the Excel file, it would set to 1
    if (is.na(denomination)) {
      denomination <- 1
    }
    
    # Mutating to change name and denomination as required
    proc_list[[as.character(s)]] <- df %>% mutate(name = new_name,
                                                  amount = amount/denomination)
    
  }
  
  proc_df <- bind_rows(proc_list)
  
  # Filtering the start date if 
  if (!is.null(from)) {
    
    proc_df <- proc_df %>% filter(date >= from)
    
  }
  
  row_order <- series_id %>% pull(series_id)
  col_order <- proc_df %>% distinct(date) %>% pull() %>% sort() %>% as.character() %>% c("category", "id", "name", .)
  
  tidy_df <- full_join(proc_df,
                       series_id %>% select(series_id, category),
                       by = c("id" = "series_id")) %>%
    pivot_wider(id_cols = c(category, id, name), names_from = date, values_from = amount) %>%
    arrange(match(id, row_order)) %>%
    relocate(all_of(col_order))
  
  meta_df <- full_join(data_series[["Meta"]] %>% select(!name),
                       series_id %>% select(category, series_id, denomination),
                       by = c("id" = "series_id")) %>%
    full_join(., tidy_df %>% select(id, name),
              by = "id")%>%
    relocate(category, .before = "id") %>%
    relocate(name, .after = "id") %>%
    arrange(match(id, row_order))
  
  tidy_list <- list("Data" = tidy_df,
                    "Meta" = meta_df)
  
  return(tidy_list)
  
}
