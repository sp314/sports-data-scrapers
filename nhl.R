library(rvest)
library(tidyverse)
library(lubridate)

# Functions for scraping and cleaning NHL schedule results data from hockey-reference.com

# seasons: years for nhl season, the later year in the season is used e.g 2018 => 2017-18 season
# RETURN: Full uncleaned data set for each season concatenated
nhl_scrape <- function(seasons) {
  season_list <- list()

  for (season in seasons) {
    print(sprintf("scraping the %s season", season))
    
    # 2005 season was canceled due to a lockout
    if(season == 2005) {
      next()
    }
    
    # Connect to page html
    page <- read_html(sprintf(fmt = "https://www.hockey-reference.com/leagues/NHL_%s_games.html", season))
    
    # Collect column names
    cols <- page %>% 
      html_nodes("table#games > thead > tr > th") %>% 
      html_attr("data-stat")
  
    # Collect date columns and include playoffs table
    playoff_date <-  page %>%
      html_nodes("table#games_playoffs > tbody > tr > th") %>%
      html_text()
    
    game_date <- page %>%
      html_nodes("table#games > tbody > tr > th") %>%
      html_text() %>%
      append(x = ., values = playoff_date)
    
    # Grab data from table body and read as matrix, including playoffs table
    playoff_data <- page %>%
      html_nodes("table#games_playoffs > tbody > tr > td") %>%
      html_text() %>%
      matrix(ncol = length(cols) - 1, byrow = TRUE) %>% 
      cbind(., TRUE)
    
    data <- page %>%
      html_nodes("table#games > tbody > tr > td") %>%
      html_text() %>%
      matrix(ncol = length(cols) - 1, byrow = TRUE) %>% 
      cbind(., FALSE) %>%
      rbind(., playoff_data)
    
    df <- data.frame(cbind(season, game_date, data), stringsAsFactors = FALSE) %>%
      mutate(game_date = as.Date(game_date, origin = "1970-01-01"),
             game_id = seq.int(nrow(.))
             )
    names(df) <- c('season', 'game_date', cols[-1], 'playoff', 'game_id')
    
    season_list[[as.character(season)]] <- df
  }
  return(season_list %>% bind_rows())
}

# scraped: output from nhl_scrape()
# include_playoffs: Whether to include playoff games
# ...: additional variables to keep that were left out
# RETURN: Full cleaned data set for each season concatenated
nhl_dataset <- function(scraped, include_playoffs = TRUE, ...) {
  clean <- scraped %>%
    rename('away' = visitor_team_name, 'away_score' = visitor_goals, 'home' = home_team_name, 'home_score' = home_goals) %>%
    mutate(
      away_score = as.numeric(away_score),
      home_score = as.numeric(home_score),
      home_win = home_score > away_score,
      home_mov = home_score - away_score) %>%
    select(game_id, season, playoff, game_date, home, away, home_score, away_score, home_win, home_mov, overtimes)
    
    if(!include_playoffs) {
      clean <- clean %>%
        filter(!playoff)
    }
    return(clean)
}

# Example Run
scraped_1999_2019 <- nhl_scrape(1999:2019)
nhl_1999_2019 <- nhl_dataset(scraped = scraped_1999_2019, include_playoffs = TRUE)
write_csv(x = nhl_1999_2019, path = "nhl_1999_2018.csv")
