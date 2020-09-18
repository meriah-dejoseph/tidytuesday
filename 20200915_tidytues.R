# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-09-15')
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)

kids <- tuesdata$kids
#Codebook: https://jrosen48.github.io/tidykids/articles/tidykids-codebook.html

