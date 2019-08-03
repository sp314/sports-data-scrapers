library(rvest)
library(tidyverse)
library(lubridate)

# Functions for scraping and cleaning NFL schedule results data from pro-football-reference.com

# seasons: years for nfl seasons
# RETURN: Full uncleaned data set for each season concatenated
nfl_scrape <- function(seasons) {
  season_list <- list()
  
  for (season in seasons) {
    print(sprintf("scraping the %s season", season))
    
    # Connect to page html
    page <- read_html(sprintf(fmt = "https://www.pro-football-reference.com/years/%s/games.htm", season))
    
    # Collect column names
    cols <- page %>% 
      html_nodes("table#games > thead > tr > th") %>% 
      html_attr("data-stat")
    
    # Grab data from table body and read as matrix
    data <- page %>%
      html_nodes("table#games > tbody > tr > td") %>%
      html_text() %>%
      matrix(ncol = length(cols) - 1, byrow = TRUE)
    
    # I remove the week and day columns (1 and 2) for simplicity
    df <- data.frame(cbind(season, data[, -1]), stringsAsFactors = FALSE)
    names(df) <- c('season', cols[-1:-2])
    df <- df %>%
      mutate(
        game_date = as.Date(game_date, "%B %d"),
        year = if_else(month(game_date) < 5, true = as.numeric(season) + 1, false = as.numeric(season)), # Take care of years rolling over without data knowning
        month = month(game_date),
        day = day(game_date),
        game_date = make_date(year = year, month = month, day = day),
        game_id = seq.int(nrow(.))
        )
    
    # Locate the date value after the playoff signal
    playoff_start <- df$game_date[which(is.na(df$game_date)) + 1]

    season_list[[as.character(season)]] <- df %>% 
      add_column(playoff_start) %>%
      filter(!is.na(game_date))
  }
  return(bind_rows(season_list))
}

# scraped: output from nfl_scrape()
# include_playoffs: Whether to include playoff games
# include_ties: Whether to include tied games
# ...: additional variables to keep that were left out
# RETURN: Full cleaned data set for each season concatenated
# NOTES: THERE WILL BE AN EXCEPTION FOR THE SUPER BOWL GAME AS THERE IS NO HOME-AWAY IN THAT GAME
# TIES ARE NOT CONSIDERED (UNCOMMON AFTER 1973)
nfl_dataset <- function(scraped, include_playoffs = TRUE, include_ties = FALSE, ...) {
  clean <- scraped %>%
    mutate(
       home_win  = game_location != "@", # The format is teamAway won @ ("at") teamHome
       home = if_else(home_win, true = winner, false = loser), # If home won, home is the winner column
       away = if_else(home_win, true = loser, false = winner), # If home won, away is the loser column
       home_score = as.numeric(if_else(home_win, true = pts_win, false = pts_lose)),
       away_score = as.numeric(if_else(home_win, true = pts_lose, false = pts_win)),
       home_turnovers = if_else(home_win, true = to_win, false = to_lose),
       away_turnovers = if_else(home_win, true = to_lose, false = to_win),
       home_yards = if_else(home_win, true = yards_win, false = yards_lose),
       away_yards = if_else(home_win, true = yards_lose, false = yards_win),
       playoff = game_date >= playoff_start,
       home_win = home_score > away_score, #Ties will be marked by NA
       home_mov = home_score - away_score
       ) %>%
    select(season, game_id, game_date, playoff, home, away, home_win, home_mov, home_score, away_score, home_turnovers, away_turnovers, home_yards, away_yards, ...)
  
  if (!include_playoffs) {
    clean <- clean %>% filter(!playoff)
  }
  
  if (!include_ties) {
    clean <- clean %>% filter(!is.na(home_win))
  }
  
  
  return(clean)
}


# Example Run
scraped_1999_2018 <- nfl_scrape(seasons = 1999:2018)
nfl_1999_2018 <- nfl_dataset(scraped = scraped_1999_2018, include_playoffs = FALSE)
write_csv(x = nfl_1999_2018, path = "nfl_1999_2018.csv")
