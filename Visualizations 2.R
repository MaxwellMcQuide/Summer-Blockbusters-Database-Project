# Loads

Blockbusters <- clean_df

  # Audience vs ROI

Audience_vs_ROI <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=audience_rating,y=ROI,color=Decade)+
  geom_point()+
  labs(title="Summer Blockbuster ROI by Audience Rating",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Audience Rating',
       y= 'Return on Investment',
       color = "Genre",
       caption = 'Not Pictured: E.T. the Extra-Terrestrial (1982)') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,15),
                     breaks= c(0,3,6,9,12,15),
                     labels = c('0x','3x','6x','9x','12x','15x')) +
  scale_x_continuous(limits = c(30,100),
                     breaks= c(30,40,50,60,70,80,90,100),
                     labels = c('30%','40%','50%','60%','70%','80%','90%','100%')) +
  scale_color_brewer(palette = 'Set1')

Audience_vs_ROI

Audience_vs_ROI_ET <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=audience_rating,y=ROI,color=Decade)+
  geom_point()+
  labs(title="Summer Blockbuster ROI by Audience Rating",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Audience Rating',
       y= 'Return on Investment',
       color = "Decade",) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,35),
                     breaks= c(0,3,6,9,12,15,18,21,24,27,30,33,36),
                     labels = c('0x','3x','6x','9x','12x','15x','18x','21x','24x','27x','30x','33x','36x')) +
  scale_x_continuous(limits = c(30,100),
                     breaks= c(30,40,50,60,70,80,90,100),
                     labels = c('30%','40%','50%','60%','70%','80%','90%','100%')) +
  scale_color_brewer(palette = 'Set1')

Audience_vs_ROI_ET

# Audience vs Profit

Audience_vs_Profit <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=audience_rating,y=Real_Profit,color=Decade)+
  geom_point()+
  labs(title="Summer Blockbuster Profit by Audience Rating",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Audience Rating',
       y= 'Profit (Real USD)',
       color = "Decade",
       caption = 'Profit Adjusted for Inflation (2022)') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_x_continuous(limits = c(30,100),
                     breaks= c(30,40,50,60,70,80,90,100),
                     labels = c('30%','40%','50%','60%','70%','80%','90%','100%')) +
  scale_y_continuous(limits = c(0, 1200000000),
                     breaks= c(0,200000000,400000000,600000000,800000000,1000000000,1200000000),
                     labels = c('0','$200M','$400M','$600M','$800M','$1B', '$1.2B')) +
  scale_color_brewer(palette = 'Set1')

Audience_vs_Profit

# Critic vs ROI

Critic_vs_ROI <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=tomatometer_rating,y=ROI,color=Decade)+
  geom_point()+
  labs(title="Summer Blockbuster ROI by Critic Rating",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Critic Rating',
       y= 'Return on Investment',
       color = "Genre",
       caption = 'Not Pictured: E.T. the Extra-Terrestrial (1982)') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,15),
                     breaks= c(0,3,6,9,12,15),
                     labels = c('0x','3x','6x','9x','12x','15x')) +
  scale_x_continuous(limits = c(0,100),
                     breaks= c(0,10,20,30,40,50,60,70,80,90,100),
                     labels = c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')) +
  scale_color_brewer(palette = 'Set1')

Critic_vs_ROI

Critic_vs_ROI_ET <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=tomatometer_rating,y=ROI,color=Decade)+
  geom_point()+
  labs(title="Summer Blockbuster ROI by Critic Rating",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Audience Rating',
       y= 'Return on Investment',
       color = "Decade",) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,35),
                     breaks= c(0,3,6,9,12,15,18,21,24,27,30,33,36),
                     labels = c('0x','3x','6x','9x','12x','15x','18x','21x','24x','27x','30x','33x','36x')) +
  scale_x_continuous(limits = c(0,100),
                     breaks= c(0,10,20,30,40,50,60,70,80,90,100),
                     labels = c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')) +
  scale_color_brewer(palette = 'Set1')

Critic_vs_ROI_ET

# Critic vs Profit

Critic_vs_Profit <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=tomatometer_rating,y=Real_Profit,color=Decade)+
  geom_point()+
  labs(title="Summer Blockbuster Profit by Critic Rating",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Audience Rating',
       y= 'Profit (Real USD)',
       color = "Decade",
       caption = 'Profit Adjusted for Inflation (2022)') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_x_continuous(limits = c(0,100),
                     breaks= c(0,10,20,30,40,50,60,70,80,90,100),
                     labels = c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')) +
  scale_y_continuous(limits = c(0, 1200000000),
                     breaks= c(0,200000000,400000000,600000000,800000000,1000000000,1200000000),
                     labels = c('0','$200M','$400M','$600M','$800M','$1B', '$1.2B')) +
  scale_color_brewer(palette = 'Set1')

Critic_vs_Profit

# Budget vs ROI

Budget_vs_ROI <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=Real_Budget,y=ROI,color=Decade)+
  geom_point() +
  labs(title="Summer Blockbuster ROI by Budget",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Budget (Real USD)',
       y= 'Return on Investment',
       color = "Decade",
       caption = 'Not Pictured: E.T. the Extra-Terrestrial (1982) \n Profits Adjusted for Inflation (2022)') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,15),
                     breaks= c(0,3,6,9,12,15),
                     labels = c('0x','3x','6x','9x','12x','15x')) +
  scale_x_continuous(limits = c(0,400000000),
                     breaks= c(0,50000000,100000000,150000000,200000000,250000000,300000000,350000000,400000000),
                     labels = c('0','$5M','$10M','$15M','$20M','$25M','$30M','$35M','$40M')) +
  scale_color_brewer(palette = 'Set1')

Budget_vs_ROI

Budget_vs_ROI_ET <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=Real_Budget,y=ROI,color=Decade)+
  geom_point() +
  labs(title="Summer Blockbuster ROI by Budget",
       subtitle = "1980-2019",
       x= 'Budget (Real USD)',
       y= 'Return on Investment',
       color = "Decade",
       caption = 'Budgets Adjusted for Inflation (2022)') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,35),
                     breaks= c(0,3,6,9,12,15,18,21,24,27,30,33,36),
                     labels = c('0x','3x','6x','9x','12x','15x','18x','21x','24x','27x','30x','33x','36x')) +
  scale_x_continuous(limits = c(0,400000000),
                     breaks= c(0,50000000,100000000,150000000,200000000,250000000,300000000,350000000,400000000),
                     labels = c('0','$5M','$10M','$15M','$20M','$25M','$30M','$35M','$40M')) +
  scale_color_brewer(palette = 'Set1')

Budget_vs_ROI_ET

# Budget vs Profit

Budget_vs_Profit <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  ggplot() +
  aes(x=Real_Budget,y=Real_Profit,color=Decade)+
  geom_point() +
  labs(title="Summer Blockbuster Profit by Budget",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Budget (Real USD)',
       y= 'Profit (Real USD)',
       color = "Decade",
       caption = 'Profits and Budgets adjusted for inflation') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0, 1200000000),
                     breaks= c(0,200000000,400000000,600000000,800000000,1000000000,1200000000),
                     labels = c('0','$200M','$400M','$600M','$800M','$1B', '$1.2B')) +
  scale_x_continuous(limits = c(0,400000000),
                     breaks= c(0,50000000,100000000,150000000,200000000,250000000,300000000,350000000,400000000),
                     labels = c('0','$5M','$10M','$15M','$20M','$25M','$30M','$35M','$40M')) +
  scale_color_brewer(palette = 'Set1')
  

Budget_vs_Profit

View(Blockbusters)

# Distributors vs Time

shorten_distributor = function(Distributor){
  return_value <- ifelse(Distributor == 'Walt Disney Studios Motion Pictures', 'Disney',
        ifelse(Distributor == 'Paramount Pictures', 'Paramount',
          ifelse(Distributor == 'Universal Pictures', 'Universal',
            ifelse(Distributor == 'Twentieth Century Fox', '20th Century Fox',
              ifelse(Distributor == 'Warner Bros.', 'Warner Bros',
                ifelse(Distributor == 'Sony Pictures Entertainment (SPE)', 'Sony',NA))))))
  
  return(return_value)
}

Distributor_vs_Time <- Blockbusters %>%
  mutate(Decade = factor(Decade, levels = c(1980,1990,2000,2010))) %>%
  filter(Distributor %in% c('Walt Disney Studios Motion Pictures',
                            'Warner Bros.',
                            'Paramount Pictures',
                            'Universal Pictures',
                            'Twentieth Century Fox',
                            'Sony Pictures Entertainment (SPE)')) %>%
  mutate(Distributor = shorten_distributor(Distributor)) %>%
  group_by(Distributor,Decade) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot() +
  aes(x=Decade, y=n, fill = Distributor)+
  geom_col(position = 'dodge') +
  labs(title="Summer Blockbuster Distributors by Decade",
       subtitle = "Top 3 Highest Grossing Summer Films Per Year From 1980-2019",
       x= 'Decade',
       y= 'Count',
       color = "Distributor") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = 'Cambria')) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  scale_y_continuous(limits = c(0,15),
                     breaks= c(0,5,10,15)) +
  scale_color_brewer(palette = 'Set1')

Distributor_vs_Time


#Summary Table of Audience and Critic Ratings

library(gtsummary)
library(gt)

Audience_df <- Blockbusters %>%
  select(Year,audience_rating) %>%
  rename(Rating = audience_rating) %>%
  mutate(Rating_Type = 'Audience')

Critics_df <-Blockbusters %>%
  select(Year,tomatometer_rating) %>%
  rename(Rating = tomatometer_rating) %>%
  mutate(Rating_Type = 'Critic')

Ratings_df <- bind_rows(Audience_df,Critics_df)

Ratings_Summary <- Ratings_df %>% 
  group_by(Rating_Type) %>%
  summarize(Mean = round(mean(Rating),2),
            SD = round(sd(Rating),2),
            Min = min(Rating),
            Q1 = quantile(Rating, 0.25),
            Median = median(Rating),
            Q3 = quantile(Rating, 0.75),
            Max = max(Rating)) %>%
  gt(groupname_col = 'Rating Type') |>
  tab_header(
    title = md("Rotten Tomatoes Summary Statistics")
  )

Ratings_Summary

# Rotten Tomatoes T Test

T_Test <- Ratings_df %>%
t.test(Rating~Rating_Type, data = ., mu = 0, alternative = "two.sided")

T_Test

