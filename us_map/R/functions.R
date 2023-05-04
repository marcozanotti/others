# Utils
# use Joe Biden as reference for margin calculation since lower Standard Error

# function that list files in multiple directories
list.files.multi <- function(paths, ...) {
  
  files <- unlist(map(paths, list.files, ...))
  
  return(files)
  
}
 
 
# function to compute percentage changes
conversion <- function(x, perc) {
  perc <- perc / 100
  res <- round(x * (1 - perc), 0)
  return(res)
}


# function that import and processes one state
state_import <- function(state_path, index = 1, features_db) {
	
	state_name <- basename(state_path) %>% str_remove("\\.csv") %>% str_to_title()
	
	cat(paste0(index, ": Importing data from ", state_name))
	cat("\n")
	
	df <- suppressMessages(read_csv(state_path)) %>% 
		rename_all(str_to_lower) %>% 
		mutate(date = as.Date(date),
		       key = if_else(key == "Joe Biden", "democratic", "republican"))

	y <- df$date %>% year() %>% max()
	m <- df$date %>% month() %>% max()
	start_date <- as.Date(paste(y, m, 1, sep = "-"))
	end_date <- ceiling_date(as.Date(paste(y, m, 1, sep = "-")), "month") - days(1)
	
	df <- df %>% 
		pivot_wider(names_from = key, values_from = value) %>% 
		complete(date = seq.Date(start_date, end_date, by = "day")) %>% 
		mutate(
		  name = state_name,
		  democratic = imputeTS::na_mean(democratic, option = "median"),
		  democratic = round(democratic, 2),
		  republican = round(if_else(is.na(republican), 100 - democratic, republican), 2),
		  week = week(date),
		  month = month(date, label = TRUE, abbr = FALSE, locale = "us"), 
			year = year(date),
			weekmon = paste(week, month, year),
			yearmon = paste(month, year)
		) %>% 
	  left_join(features_db, by = "name") %>% 
		dplyr::select(date, week, month, year, yearmon, weekmon, name, code, region, country, everything())
	
	return(df)
	
}


# wrapper that reads in all the files
states_merge <- function(states_dirs, features_db) {
	
	files <- list.files.multi(states_dirs, full.names = TRUE)
	
	features_db <- features_db %>%
	  rename_all(str_to_lower) %>% 
	  mutate(name = str_to_title(name))
	
	res <- map2_df(files, seq_along(files), state_import, features_db)
	
	return(res)
  
}


# function that merges volumes to db
volumes_merge <- function(df_polls, df_volumes) {
  
  df_volumes <- df_volumes %>% 
    rename_all(str_remove_all, pattern = ",.*") %>% 
    rename_all(str_to_lower) %>% 
    pivot_longer(-c(date, off_topic), names_to = "name", values_to = "volume") %>%
    mutate(date = as.Date(date),
           name = str_to_title(name),
           volume = conversion(volume, off_topic)) %>%
    select(-off_topic)
    
  df_polls <- df_polls %>% 
    left_join(df_volumes, by = c("date", "name"))
  
  return(df_polls)
  
}


# function that computes party margin 
compute_margin <- function(df, party_margin = 2) {
  
  #party_colors <- c("#2E74C0", "#999999", "#CB454A")
  
  df <- df %>%
    group_by(name) %>% 
    mutate(
      margin = round(democratic - republican, 2),
      party = factor(case_when(margin > party_margin ~ "Democratic",
                               margin <= party_margin & margin >= -party_margin ~ "Uncertain",
                               margin < -party_margin ~ "Republican"), 
                     levels = c("Democratic", "Uncertain", "Republican")),
      description = paste0("Trump ", republican, "%\nBiden ", democratic, "%"),
      description_full = paste0(name, ":\nTrump ", republican, 
                                "%\nBiden ", democratic, "%", 
                                "\nMarigin ", round(abs(margin), 2), "%"),
      # colors = case_when(party == "Republican" ~  party_colors[1],
      #                    party == "Uncertain" ~ party_colors[2],
      #                    party == "Democratic" ~ party_colors[3], 
      #                    TRUE ~ NA_character_)
    ) %>% 
    ungroup()
  
  return(df)
  
}


# USA feature db with geometry
usa_geomdb <- function(features_db) {
  
  geom_db <- get_urbn_map(map = "states", sf = TRUE)
  geom_db <- geom_db %>% 
    rename(name = state_name, geoid = state_fips, code = state_abbv) %>% 
    rename_all(str_to_lower) %>% 
    mutate(name = str_to_title(name))
  
  features_db <- features_db %>% 
    rename_all(str_to_lower)
  
  usa_geom <- geom_db %>% 
    left_join(features_db, by = c("name", "code")) %>% 
    select(name, code, geoid, region, country, everything())
  
  return(usa_geom)
  
}


# function that computes the aggregate results
compute_polls <- function(df, electors = TRUE, aggregate = "national") {
  
  if (electors) {
    
    if (aggregate == "national") {
      res <- df %>% 
        group_by(party) %>% 
        summarise(votes = sum(electors)) %>% 
        mutate(proportion = round(votes / sum(votes) * 100, 2)) %>% 
        ungroup()
    } else {
      res <- df %>% 
        group_by(region, party) %>% 
        summarise(votes = sum(electors)) %>% 
        mutate(proportion = round(votes / sum(votes) * 100, 2)) %>% 
        ungroup()
    }
    
  } else  {
    
    if (aggregate == "national") {
      res <- df %>% 
        filter(party %in% c("Democratic")) %>% 
        summarise(Democratic = round(mean(democratic), 2)) %>% 
        mutate(Republican = 100 - Democratic) %>% 
        pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "proportion")
    } else {
      res <- df %>% 
        filter(party %in% c("Democratic", "Republican")) %>% 
        group_by(region) %>% 
        summarise(Democratic = round(mean(democratic), 2)) %>% 
        mutate(Republican = 100 - Democratic) %>% 
        pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "proportion")
    }
    
  }
  
  return(res)
  
}


# function to compute daily polls
compute_daily_polls <- function(df, aggregate = "national") {
  
  if (aggregate == "national") {
    res <- df %>% 
      group_by(date, party) %>% 
      summarise(votes = sum(electors)) %>% 
      mutate(proportion = round(votes / sum(votes) * 100, 2)) %>% 
      ungroup()
  } else {
    res <- df %>% 
      group_by(date, region, party) %>% 
      summarise(votes = sum(electors)) %>% 
      mutate(proportion = round(votes / sum(votes) * 100, 2)) %>% 
      ungroup()
  }
  
  return(res)
  
}

