library(rvest)
library(tidyverse)
library(lubridate)

# Functions for scraping NBA schedule results data from basketball-reference.com

# seasons: years for nba season, the later year in the season is used e.g 2018 => 2017-18 season
# RETURN: urls for schedule page for each month of play in each year
nba_urls <- function(seasons) {
  urls <- list()
  for (y in seasons) {
    base_url <- paste0("https://www.basketball-reference.com/leagues/NBA_", y, "_games.html")
    
    # Make sure we have the right months for each season
    months <- read_html(base_url) %>% 
      html_nodes("div.filter > div > a") %>% 
      html_text() %>%
      tolower()
    
    #Append all month urls to list
    month_urls <- paste0("https://www.basketball-reference.com/leagues/NBA_", y, "_games-", months, ".html")
    urls[[as.character(y)]] <- month_urls
  }
  return(urls)
}

# urls: Urls for schedule page for each month of play in each year
# include_playoffs: Whether to include playoff games
# RETURN: Full clean data set for each season concatenated
nba_dataset <- function(urls, include_playoffs = TRUE) {
  season_list <- list()
  for (season in names(urls)) {
    print(sprintf("scraping the %s season", season))
    idx <- 1
    playoff_start <- character(0)
    
    for (month in urls[[season]]) {
      page <- read_html(month)
      
      # Extract column names
      cols <- page %>%
        html_nodes("table#schedule > thead > tr > th") %>% 
        html_attr("data-stat")
      
      # Extract dates column
      dates <- page %>%
        html_nodes("table#schedule > tbody > tr > th") %>%
        html_text()
      
      #If playoffs start is not assigned, attempt to assign
      if (length(playoff_start) == 0) {
        playoff_start <- dates[which(dates == "Playoffs") + 1]
      }
      
      # Remove playoff signal column and convert all to date
      dates <- dates[dates != "Playoffs"]
      
      # Get row data
      data <- page %>%
        html_nodes("table#schedule > tbody > tr > td") %>%
        html_text() %>%
        matrix(ncol = length(cols) - 1, byrow = TRUE)
      
      # Bind together dates and row data
      df <- data.frame(cbind(season, dates, data), stringsAsFactors = FALSE)
      names(df) <- c('season', cols)
      season_list[[season]][[idx]] <- df
      idx <- idx + 1
    }
    
    #I select my variables here, there are variables for attendance and game_remarks that I just remove
    season_list[[season]] <- bind_rows(season_list[[season]]) %>%
      rename('game_date' = date_game,
             'away' = visitor_team_name, 'away_score' = visitor_pts,
             'home' = home_team_name, 'home_score' = home_pts) %>%
        mutate(
          game_id = seq.int(nrow(.)),
          playoff_start = as.Date(playoff_start, "%a, %b %d, %Y"),
          game_date = as.Date(game_date, "%a, %b %d, %Y"),
          playoff = game_date >= playoff_start,
          home_score = as.numeric(home_score),
          away_score = as.numeric(away_score),
          home_win = home_score > away_score,
          home_mov = home_score - away_score
        ) %>%
      select(season, game_id, game_date, playoff, home, away, home_score, away_score, home_win, home_mov, overtimes)
  }
  
  
  full_df <- bind_rows(season_list)
  
  if (!include_playoffs) {
    return(full_df %>% filter(!playoff))
  }
  
  return(full_df)
}

# # Example Run
# urls_1999_2019 <- nba_urls(years = 1999:2019)
# nba_1999_2019 <- nba_dataset(urls = urls_1999_2019, include_playoffs = FALSE)
# write_csv(x = nba_1999_2019, path = "nba_1999_2019.csv")
