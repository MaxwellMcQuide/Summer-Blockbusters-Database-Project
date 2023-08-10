# Libraries

library(extrafont)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(ggpubr)

# Loads

Blockbusters <- clean_df

# Genres By Decade

  # Data frame

genre_list <- colnames(Blockbusters)[12:30]

genre_count_by_decade <- data.frame()

for (genre in genre_list){
  frequency_table <- Blockbusters %>%
    group_by(Decade, .data[[genre]]) %>%
    count() %>%
    pivot_wider(names_from = .data[[genre]], values_from = n) %>%
    select(Decade, '1') %>%
    mutate(Genre = {{genre}})
  
  genre_count_by_decade <- bind_rows(genre_count_by_decade,frequency_table)
  
}

genre_count_by_decade["1"][is.na(genre_count_by_decade["1"])] <- 0

  
  # Visualization
genre_count_by_decade_visual <- genre_count_by_decade %>%
  rename(count = '1') %>%
  select(Decade, Genre, count) %>%
  filter(!(Genre %in% c('Music','Musical','Sport','Horror','Mystery','Western','History','Crime','War','Romance','Animation'))) %>%
  ggplot() +
  aes(x=Decade,y=count, color = Genre)+
  geom_jitter(height = 0, width = .1) +
  labs(title="Summer Blockbuster Genre Counts by Decade",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Decade',
       y= 'Count',
       color = "Genre",
       caption = 'Only Top 8 Genres listed') +
  #scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,30),
                     breaks= c(0,5,10,15,20,25,30)) +
  scale_color_brewer(palette = 'Set1')

  
genre_count_by_decade_visual

# Runtimes

Runtime_Visual <- Blockbusters %>%
  ggplot()+
  aes(x= Running_Time, y=Decade, group = Decade, fill = Decade)+
  geom_boxplot()+
  labs(title="Summer Blockbuster Runtimes by Decade",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Runtime',
       y= 'Decade') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  scale_color_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(90,195),
                     breaks= c(90,105,120,135,150,165,180,195),
                     labels = c('1:30','1:45','2:00','2:15','2:30','2:45','3:00','3:15')) +
  scale_fill_brewer(palette = 'Set1')

Runtime_Visual

# MPAA

  #Dataframe

Blockbusters_MPAA_Count <- Blockbusters %>%
  group_by(Decade) %>%
  count(MPAA) %>%
  rename(count = n) %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ungroup(Decade) %>%
  add_row(Decade='1980', 
          MPAA='G', 
          count=0)


  #Visualization

MPAA_Visual <- Blockbusters_MPAA_Count %>%
  ggplot() +
  aes(x=Decade, y= count, fill = MPAA) +
  geom_col(position = 'dodge') +
  labs(title="Summer Blockbuster MPAA Rating by Decade",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Decade',
       y= 'Count',
       caption ='G and PG-13 Ratings not used until 1984')+
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  scale_color_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_fill_brewer(palette = 'Set1')
  
  
MPAA_Visual

# Sequels by Decade


frequency_table_sequel <- Blockbusters %>%
  group_by(Decade, Sequel) %>%
  count() %>%
  mutate(Sequel = factor(Sequel, levels = c(0,1)))
  

sequels_by_decade_visual <- frequency_table_sequel %>%
  ggplot() +
  aes(x=as.numeric(Decade),y= n, color = Sequel)+
  geom_jitter(height = 0, width = .1) +
  geom_smooth(method = 'lm', se=FALSE,linewidth = .5, alpha = .2) + 
  labs(title="Summer Blockbuster Sequels by Decade",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Decade',
       y= 'Count',
       color = "Sequel") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(0,30),
                     breaks= c(0,10,20,30)) +
  scale_x_continuous(limits = c(.8,4.2),
                     breaks= c(1,2,3,4),
                     labels = c('1980','1990','2000','2010')) +
  scale_color_brewer(palette = 'Set1')

sequels_by_decade_visual

# Dates

Date_density <- Blockbusters %>%
  ggplot() +
  aes(x=Date_Formatted, y = ..scaled.., group = Decade, color = Decade) +
  geom_density(kernel='gaussian', linewidth = 1) +
  labs(title="Summer Blockbuster Release Dates by Decade",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Date',
       y= 'Density') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
   scale_x_continuous(limits = c(19478,19584),
                      breaks= c(19478,19492,19509,19523,19539,19553,19570,19584),
                      labels = c('May 1st','May 15','June 1', 'June 15','July 1','July 15','August 1','August 15')) +
  scale_color_brewer(palette = 'Set1')

Date_density