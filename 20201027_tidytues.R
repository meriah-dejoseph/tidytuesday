#Kristin's quick assignment for us this week 
#Only give 30 min max


# Get the Data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub
# Either ISO-8601 date or year/week works!
# Install via devtools::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-03-17')
tuesdata <- tidytuesdayR::tt_load(2020, week = 12)
office_ratings <- tuesdata$office_ratings

# Use office_ratings to plot the IMDB ratings by Season. 
#Plot each Season separately (hint, use facet_wrap(.~season)) with each episode plotted on its season's graph.

#make season factor first
office_ratings$season2 <- as.factor(office_ratings$season)

ggplot(data=office_ratings, aes(x=episode, y=imdb_rating, color=season2)) + 
  geom_point() + 
  facet_wrap(~season)+
  xlab("Office Episode")+
  ylab("IMDB Ratings")

# Get average IMDB ratings for each season, including SD 
office_ratings %>% 
  group_by(season) %>% 
  summarise(mean=mean(imdb_rating),
            sd=sd(imdb_rating))


# Plot average season ratings on one graph. Include error bars with SE. Make sure the title is centered and axes are labeled. 
ggplot(data=office_ratings,aes(x=season, y=imdb_rating, group=season, color=factor(season))) + 
  geom_line(aes(group = season), show.legend = FALSE)+
  stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() + 
  ylab("IMDB Ratings")+
  xlab("Season")


# Run an analysis to determine if there is a main effect of season, or trend (e.g. later season get higher ratings i.e. the show gets better)
lm.1 <- lm(imdb_rating ~ season + total_votes, data=office_ratings)
summary(lm.1)


#Get predicted values
office_ratings$yhat = predict(lm.1, newdata = office_ratings, re.form = NA)

#Plot 
ggplot(data = office_ratings, aes(x = season, y = yhat)) +
  geom_smooth(method="lm", lwd = 1.5, show.legend = FALSE) +
  geom_point(aes(y=imdb_rating), alpha = 0.3, show.legend = FALSE) +
  theme_bw() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9), labels = c(1:9), name="Season") +
  ylab("IMDB Ratings") 







