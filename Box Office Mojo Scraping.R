
# Libraries

library(rvest)
library(dplyr)
library(stringr)
library(magrittr)
library(readr)
library(priceR)

# Functions

extras_function_dplyr <- function(link){
  extra_page <- read_html(link)
  
  extra_text <- extra_page %>% html_nodes(extras_node) %>%
    html_text() 
  
  extra_df <- data.frame(category = str_squish(extra_text[seq(1,length(extra_text),2)]), 
                         statistic = str_squish(extra_text[seq(2,length(extra_text),2)])) %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(str_squish(extra_text[seq(1,length(extra_text),2)])) %>%
    set_rownames(1:2) %>%
    select(-c('Distributor')) %>%
    slice(2)
  
  if('Release Date' %in% colnames(extra_df)){
    extra_df <- select(extra_df, -c('Release Date'))
  }
  
  
  return(extra_df)
}

convert_to_minute <- function(string){
  hour = as.integer(sub('(\\d+) hr *(\\d+)* *m*i*n*','\\1',string))
  minute = ifelse(is.na(as.integer(sub('(\\d+) hr *(\\d+)* *m*i*n*','\\2',string))),
                  0,
                  as.integer(sub('(\\d+) hr *(\\d+)* *m*i*n*','\\2',string)))
  
  
  return_value <- as.integer(paste(hour*60+minute))
  return(return_value)
}

genre_function <- function(list,genre){
  return(ifelse(genre %in% list,'1','0'))
}

genre_df_function <- function(df){
  genre_list <- c()
  genre_by_film <- df[,'Genres']
  
  for (i in seq(1:length(df$Genres))){
    for (j in seq(1:length(df$Genres[[i]]))){
      if (!(df$Genres[[i]][j] %in% genre_list)){
        genre_list <- append(genre_list,df$Genres[[i]][j])
      }
    }
  }
  
  
  genre_df <- data.frame(x = 1:nrow(df))
  
  for(genre in genre_list){
    dummy_list <- sapply(genre_by_film,genre_function,genre=genre)
    genre_df[genre] <- dummy_list
  }
  
  genre_df <- select(genre_df, -c(x)) %>%
    mutate_all(as.factor)
  
  return_df <- bind_cols(df,genre_df)
  return(return_df)
}

# Initial Variables

table_node <- '#table'
link_node <- '.mojo-cell-wide .a-link-normal'
extras_node <- '.mojo-hidden-from-mobile .a-spacing-none > span'

df <- data.frame()

# Scraping parameters

year_list <- c(1980:2019)
films_per_year <- 3


# Scraper

for (year in year_list){
  print(year)
  extras_df <- data.frame()
  link <- paste('https://www.boxofficemojo.com/season/summer/', year, '/', sep = '')
  page <- read_html(link)
  
  table <- page %>% html_nodes(table_node) %>%
    html_table() %>% .[[1]] %>%
    slice(1:films_per_year)
  
  links <- page %>% html_nodes(link_node) %>%
    html_attr("href") %>% paste('https://www.boxofficemojo.com',.,sep = '')
  links <- links[1:films_per_year]
  
  for (link in links){
    extras_df <- bind_rows(extras_df, extras_function_dplyr(link))
  }
  
  table <- cbind(table,links) %>%
    mutate(Year = year) %>%
    rename(Title = Release) %>%
    select(-c(Genre,Budget, 'Running Time',)) %>%
    bind_cols(extras_df)
  
  
  df <- bind_rows(df,table) 
}


# Initial Data Cleaning

clean_df <- df %>%
  select(Title,Genres, Distributor,'Release Date',Year, MPAA, 'Running Time',Gross,'Total Gross',Budget, Theaters) %>%
  rename(Release_Date = 'Release Date',
         Running_Time = 'Running Time',
         Total_Gross = 'Total Gross') %>%
  mutate(Gross = as.integer(gsub('[\\$,]','', Gross)),
         Total_Gross = as.integer(gsub('[\\$,]','\\1', Total_Gross)),
         Budget = as.integer(gsub('[\\$,]','\\1', Budget)),
         Theaters = as.integer(gsub('[\\$,]','\\1', Theaters)),
         MPAA = as.factor(MPAA),
         Date_Formatted = as.Date(Release_Date, format = '%b %d'),
         Distributor = as.factor(Distributor),
         Running_Time = convert_to_minute(Running_Time),
         Year = as.factor(Year)) %>%
  mutate(Genres = strsplit(Genres, ' ')) %>%
  genre_df_function() %>%
  select(-c(Genres)) %>%
  arrange(desc(Gross))


# Decade Variable

year_to_decade <- function(year){
  return_decade <- ifelse(year >= 1980 & year <= 1989, 1980,
                          ifelse(year <= 1999, 1990,
                                 ifelse (year <= 2009, 2000,2010)))
  return(return_decade)
}

clean_df <- clean_df %>%
  mutate(Year = as.numeric(Year)+1979) %>%
  rowwise() %>%
  mutate(Decade = year_to_decade(Year)) %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010)))

# MPAA NAs

index_list <-c(15,31,54,55,57,73,81,86,87,89,92,
               94,97,98,101,102,103,104,105,106,
               107,109,111,112,114,115,116,117,118,119,120)

rating_list <- c('G','G','G','PG-13','PG-13','PG','PG','R','PG-13','R',
                 'PG','PG','PG','PG','PG-13','R','R','PG-13','PG-13','PG-13',
                 'R','PG','R','PG-13','R','PG','PG-13','PG','PG','R','R')

for (i in seq(1:length(index_list))){
  clean_df[index_list[i],'MPAA'] <- rating_list[i]
}

# Budget NAs

clean_df['Avengers: Infinity War' == clean_df$Title,'Budget'] <- 316000000
clean_df['Incredibles 2' == clean_df$Title,'Budget'] <- 200000000
clean_df['Finding Dory' == clean_df$Title,'Budget'] <- 200000000
clean_df['Harry Potter and the Deathly Hallows: Part 2' == clean_df$Title,'Budget'] <- 250000000
clean_df['Apollo 13' == clean_df$Title,'Budget'] <- 52000000
clean_df['Beverly Hills Cop II' == clean_df$Title,'Budget'] <- 27000000
clean_df['The Firm' == clean_df$Title,'Budget'] <- 42000000
clean_df['Rambo: First Blood Part II' == clean_df$Title,'Budget'] <- 25500000
clean_df['Deep Impact' == clean_df$Title,'Budget'] <- 80000000
clean_df['Gremlins' == clean_df$Title,'Budget'] <- 11000000
clean_df['Pocahontas' == clean_df$Title,'Budget'] <- 55000000
clean_df['Lethal Weapon 2' == clean_df$Title,'Budget'] <- 30000000
clean_df['Top Gun' == clean_df$Title,'Budget'] <- 15000000
clean_df['Sister Act' == clean_df$Title,'Budget'] <- 31000000
clean_df['The Fugitive' == clean_df$Title,'Budget'] <- 44000000
clean_df['Rocky III' == clean_df$Title,'Budget'] <- 17000000
clean_df['Crocodile Dundee II' == clean_df$Title,'Budget'] <- 14000000
clean_df['The Karate Kid Part II' == clean_df$Title,'Budget'] <- 13000000
clean_df['Stripes' == clean_df$Title,'Budget'] <- 10000000
clean_df['Trading Places' == clean_df$Title,'Budget'] <- 15000000
clean_df['Back to School' == clean_df$Title,'Budget'] <- 11000000
clean_df['The Untouchables' == clean_df$Title,'Budget'] <- 25000000
clean_df['The Cannonball Run' == clean_df$Title,'Budget'] <- 17000000
clean_df['Cocoon' == clean_df$Title,'Budget'] <- 17500000
clean_df['Octopussy' == clean_df$Title,'Budget'] <- 27500000
clean_df['Smokey and the Bandit II' == clean_df$Title,'Budget'] <- 17000000
clean_df['The Witches of Eastwick' == clean_df$Title,'Budget'] <- 22000000
clean_df['Octopussy' == clean_df$Title,'Budget'] <- 27500000
clean_df['The Blues Brothers' == clean_df$Title,'Budget'] <- 27500000
clean_df['Coming to America' == clean_df$Title,'Budget'] <- 36000000

# Return on Investment

clean_df <- clean_df %>%
  mutate(ROI = Total_Gross/Budget)


# Add Sequal Variable

Sequel <- c(1,1,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,0,1,1,
            1,1,1,1,0,1,1,0,1,1,
            0,1,1,0,1,0,1,0,1,1,
            1,0,0,0,1,1,0,0,0,0,
            1,1,1,0,0,0,0,0,0,0,
            1,0,1,1,1,1,0,1,1,0,
            0,1,0,0,1,0,1,0,0,0,
            1,0,1,0,1,1,0,0,1,1,
            1,0,0,0,0,1,0,0,0,0,
            0,0,0,0,1,1,1,1,1,1,
            0,0,1,0,0,0,1,1,0,0)


clean_df <- clean_df %>%
  bind_cols(as.data.frame(Sequel))

# Merge With Rotten Tomatoes

Rotten_Tomatoes <- read_csv("~/Blog/Summer Blockbusters/Excel Files/Rotten_Tomatoes.csv") %>%
  select(movie_title,tomatometer_rating,audience_rating) %>%
  rename(Title = movie_title)

clean_df <- left_join(clean_df,Rotten_Tomatoes, by = 'Title') %>%
  as.data.frame() %>%
  slice(-c(4,15,31,53,78,105,109))

# Rotten Tomatoes NAs


clean_df['Batman' == clean_df$Title,'tomatometer_rating'] <- 76
clean_df['Batman' == clean_df$Title,'audience_rating'] <- 84

clean_df['True Lies' == clean_df$Title,'tomatometer_rating'] <- 70
clean_df['True Lies' == clean_df$Title,'audience_rating'] <- 76

clean_df['The Cannonball Run' == clean_df$Title,'tomatometer_rating'] <- 29
clean_df['The Cannonball Run' == clean_df$Title,'audience_rating'] <- 61

clean_df['Cocoon' == clean_df$Title,'tomatometer_rating'] <- 76
clean_df['Cocoon' == clean_df$Title,'audience_rating'] <- 60

clean_df['The Avengers' == clean_df$Title,'tomatometer_rating'] <- 91
clean_df['The Avengers' == clean_df$Title,'audience_rating'] <- 91


# Adjust for Inflation


clean_df <- clean_df %>%
  mutate(Real_Gross = adjust_for_inflation(Total_Gross,Year, country= 'US',to_date = 2022),
         Real_Budget = adjust_for_inflation(Budget,Year, country= 'US',to_date = 2022),
         Real_Profit = Real_Gross - Real_Budget)

