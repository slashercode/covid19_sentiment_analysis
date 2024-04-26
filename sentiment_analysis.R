# Install required packages
system('sudo apt-get -y install libmagick++-dev', intern = TRUE)
install.packages('magick', verbose = TRUE)
install.packages('RWeka')
install.packages('rJava')

# LOAD LIBRARIES
################
library(readr)         # For reading data
library(ggplot2)       # For creating plots
library(tidyverse)     # For data manipulation
library(gridExtra)     # For arranging plots
library(magick)        # For image manipulation
library(scales)        # For formatting plot scales
library(ggrepel)       # For label repelling in ggplot2
library(repr)          # For setting plot dimensions
library(hexbin)        # For hexbin plots
library(naniar)        # For missing data visualization
library(lubridate)     # For date-time manipulation
library(tm)            # For text mining
library(wordcloud)     # For creating word clouds
library(wordcloud2)    # For interactive word clouds
library(tidytext)      # For text mining with tidy data
library(textdata)      # For text data sets
library(reshape2)      # For reshaping data
library(knitr)         # For dynamic report generation
library(grid)          # For grid graphics
library(igraph)        # For graph data manipulation
library(ggraph)        # For graph visualization
library(ggsci)         # For scientific journal color palettes
library(devtools)      # For package development
library(circlize)      # For circular visualization
library(radarchart)    # For radar charts
library(stringr)       # For string manipulation
library(sjmisc)        # For data manipulation
library(htmlwidgets)   # For interactive HTML widgets
library(VIM)           # For missing data visualization
library(colorspace)    # For manipulating and assessing color palettes
library(RWeka)         # For machine learning algorithms
library(textmineR)     # For text mining

# CUSTOM THEME
################
# Set plot size options
options(repr.plot.width = 15, repr.plot.height = 7)

# Custom Color Palette
my_colors <- c("#1982C4", "#1B512D", "#FED45B", "#F78104", "#CA3D3F", "#7209B7")
show_col(my_colors, labels = FALSE, borders = NA)

# Custom Theme Variable
my_theme <- theme(
  plot.background = element_rect(fill = "grey98", color = "grey20"),
  panel.background = element_rect(fill = "grey98"),
  panel.grid.major = element_line(colour = "grey87"),
  text = element_text(color = "grey20"),
  plot.title = element_text(size = 22),
  plot.subtitle = element_text(size = 17),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 15),
  legend.box.background = element_rect(color = "grey20", fill = "grey98", linewidth = 0.1),
  legend.box.margin = margin(t = 3, r = 3, b = 3, l = 3),
  legend.title = element_blank(),
  legend.text = element_text(size = 15),
  strip.text = element_text(size = 17)
)

# IMPORT DATA
################
# Import data information
data <- read_csv("covid-19-all.csv", 
                 col_types = cols(`Country/Region` = col_character(),
                                  `Province/State` = col_character(),
                                  Latitude = col_double(),
                                  Longitude = col_double(),
                                  Confirmed = col_double(),
                                  Recovered = col_double(),
                                  Deaths = col_double(),
                                  Date = col_date(format = "")))
data <- data %>% rename(c("Country" = "Country/Region", "State" = "Province/State"))

tweets <- read_csv("covid19_tweets.csv",
                   col_types = cols(user_name = col_character(),
                                    user_location = col_character(),
                                    user_description = col_character(),
                                    user_created = col_datetime(format = ""),
                                    user_followers = col_double(),
                                    user_friends = col_double(),
                                    user_favourites = col_double(),
                                    user_verified = col_logical(),
                                    date = col_datetime(format = ""),
                                    text = col_character(),
                                    hashtags = col_character(),
                                    source = col_character(),
                                    is_retweet = col_logical()))

# Inspect data
data %>% head(5)  # Display the first 5 rows of the data

# Inspect tweet data
tweets %>% head(2)  # Display the first 2 rows of the tweet data

# Import some additional data
worldcities <- read_csv("worldcitiespop.csv",
                        col_types = cols(Country = col_character(),
                                         City = col_character(),
                                         AccentCity = col_character(),
                                         Region = col_character(),
                                         Population = col_double(),
                                         Latitude = col_double(),
                                         Longitude = col_double()))

aggr(data)  # Aggregate data in the 'data' dataframe

# Delete values that have the 3 columns missing from the data
data <- na.omit(data, cols=c("Confirmed"))  # Remove rows with missing values in the 'Confirmed' column

aggr(tweets)  # Aggregate data in the 'tweets' dataframe


# INSPECTION
#################
# Set plot size options
options(repr.plot.width=15, repr.plot.height=9)

# Plot 1: Reported Cases in Time
data %>%
  select(Date, Confirmed, Recovered, Deaths) %>%
  gather(key = group_var, value = "Cases", -Date, na.rm = TRUE) %>%
  group_by(Date, group_var) %>%
  summarise(n = sum(Cases, na.rm = F), .groups = "drop_last") %>%
  mutate(label = if_else(Date == max(Date), as.character(group_var), NA_character_)) %>% 
  
  ggplot(aes(x=Date, y = n, color=group_var)) + 
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(values=c("solid", "twodash", "dotted")) +
  coord_cartesian(clip = 'off') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  my_theme + theme(axis.title.x = element_blank()) +
  labs(title = "Reported Cases in Time", subtitle = "2020", y = "Frequency")

# Plot 2: Top Countries per Case Type
data %>%
  select(Country, Confirmed, Recovered, Deaths) %>%
  gather(key = group_var, value = "Cases", -Country, na.rm = TRUE) %>%
  group_by(Country, group_var) %>%
  summarise(n = sum(Cases, na.rm = F), .groups = "drop_last") %>%
  arrange(desc(n)) %>% 
  group_by(group_var) %>% 
  slice(1:5) %>%
  
  ggplot(aes(x = Country, y = n, fill=Country)) +
  geom_bar(stat = "identity") +
  facet_grid(~ group_var, scales = "free") +
  scale_fill_manual(values = c(my_colors, "#BE82AF", "#9D4387", "#DEC0D7"), guide="none") +
  geom_label(aes(label=round(n/1000000, 1)), size=5, fill="white") +
  labs(title = "Top Countries per Case Type", subtitle = "Numbers in Millions") +
  my_theme + theme(axis.text.y = element_blank(),
                   axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
                   axis.title = element_blank())

# Plot 3: Top States per Case Type
data %>%
  filter(State != c("NA", "Unknown")) %>%
  select(State, Confirmed, Recovered, Deaths) %>%
  gather(key = group_var, value = "Cases", -State, na.rm = TRUE) %>%
  group_by(State, group_var) %>%
  summarise(n = sum(Cases, na.rm = F), .groups = "drop_last") %>%
  arrange(desc(n)) %>%
  group_by(group_var) %>% 
  slice(1:5) %>%
  
  ggplot(aes(x = State, y = n, fill=State)) +
  geom_bar(stat = "identity") +
  facet_grid(~ group_var, scales = "free") +
  scale_fill_manual(values = c(my_colors, "#BE82AF", "#9D4387", "#DEC0D7",
                               "#40BDC8", "#80D3DB", "#BFE9ED"), guide="none") +
  geom_label(aes(label=round(n/1000000, 1)), size=5, fill="white") +
  labs(title = "Top States per Case Type", subtitle = "Numbers in Millions") +
  my_theme + theme(axis.text.y = element_blank(),
                   axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
                   axis.title = element_blank())


# TEXT PREPARATION
#################
# Function to clean text data
cleanCorpus <- function(text){
  # Remove punctuation, whitespace, convert to lowercase, remove numbers
  text.tmp <- tm_map(text, removePunctuation)
  text.tmp <- tm_map(text.tmp, stripWhitespace)
  text.tmp <- tm_map(text.tmp, content_transformer(tolower))
  text.tmp <- tm_map(text.tmp, removeNumbers)
  
  # Remove stopwords
  stopwords_remove <- c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  text.tmp <- tm_map(text.tmp, removeWords, stopwords_remove)
  
  return(text.tmp)
}

# Function to extract frequent unigrams
frequentTerms <- function(text){
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  return(dm)
}

# Function to extract frequent bigrams
tokenizer_2 <- function(x){
  NGramTokenizer(x, Weka_control(min=2, max=2))
}

frequentBigrams <- function(text){
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize=tokenizer_2))
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  return(dm)
}

# Function to extract frequent trigrams
tokenizer_3 <- function(x){
  NGramTokenizer(x, Weka_control(min=3, max=3))
}

frequentTrigrams <- function(text){
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize=tokenizer_3))
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  
  return(dm)
}

# Read sentiment lexicons
afinn <- read_csv("Afinn.csv", col_types = cols(word = col_character(), value = col_double()))
bing <- read_csv("Bing.csv", col_types = cols(word = col_character(), sentiment = col_character()))
nrc <- read_csv("NRC.csv", col_types = cols(word = col_character(), sentiment = col_character()))

# Extract location information from tweets
tweets_location <- tweets %>%
  mutate(user_location = tolower(user_location)) %>%
  group_by(user_location) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n))

# Create a new column and fill it with NA
tweets_location$country <- NA

# Manually map some of the locations 
tweets_location <- tweets_location %>%
  mutate(country = ifelse(grepl("india", user_location), "India", country),
         country = ifelse(grepl("delhi", user_location), "India", country),
         country = ifelse(grepl("mumbai", user_location), "India", country),
         country = ifelse(grepl("bengaluru", user_location), "India", country),
         country = ifelse(grepl("bangalore", user_location), "India", country),
         country = ifelse(grepl("bhubaneswar", user_location), "India", country),
         country = ifelse(grepl("hyderabad", user_location), "India", country),
         country = ifelse(grepl("china", user_location), "China", country),
         country = ifelse(grepl("beijing", user_location), "China", country),
         country = ifelse(grepl("hong kong", user_location), "Hong Kong", country),
         country = ifelse(grepl("singapore", user_location), "Singapore", country),
         country = ifelse(grepl("australia", user_location), "Australia", country),
         country = ifelse(grepl("melbourne", user_location), "Australia", country),
         country = ifelse(grepl("sydney", user_location), "Australia", country),
         country = ifelse(grepl("canada", user_location), "Canada", country),
         country = ifelse(grepl("africa", user_location), "Africa", country),
         country = ifelse(grepl("england", user_location), "UK", country),
         country = ifelse(grepl("united kingdom", user_location), "UK", country),
         country = ifelse(grepl("london", user_location), "UK", country),
         country = ifelse(grepl("uk", user_location), "UK", country),
         country = ifelse(grepl("united states", user_location), "US", country),
         country = ifelse(grepl("usa", user_location), "US", country),
         country = ifelse(grepl("us", user_location), "US", country),
         country = ifelse(grepl("washington", user_location), "US", country),
         country = ifelse(grepl("new york", user_location), "US", country),
         country = ifelse(grepl("angeles", user_location), "US", country),
         country = ifelse(grepl("atlanta", user_location), "US", country),
         country = ifelse(grepl("california", user_location), "US", country),
         country = ifelse(grepl("chicago", user_location), "US", country),
         country = ifelse(grepl("boston", user_location), "US", country),
         country = ifelse(grepl("philadelphia", user_location), "US", country),
         country = ifelse(grepl("diego", user_location), "US", country),
         country = ifelse(grepl("seattle", user_location), "US", country),
         country = ifelse(grepl("texas", user_location), "US", country),
         country = ifelse(grepl("nyc", user_location), "US", country),
         country = ifelse(grepl("vegas", user_location), "US", country),
         country = ifelse(grepl("francisco", user_location), "US", country),
         country = ifelse(grepl("florida", user_location), "US", country),
         country = ifelse(grepl("dallas", user_location), "US", country),
         country = ifelse(grepl("denver", user_location), "US", country),
         country = ifelse(grepl("worldwide", user_location), "NoCountry", country),
         country = ifelse(grepl("global", user_location), "NoCountry", country),
         country = ifelse(grepl("earth", user_location), "NoCountry", country),
         country = ifelse(grepl("everywhere", user_location), "NoCountry", country),
         country = ifelse(grepl("nigeria", user_location), "Nigeria", country),
         country = ifelse(grepl("kenya", user_location), "Kenya", country),
         country = ifelse(grepl("switzerland", user_location), "Switzerland", country),
         country = ifelse(grepl("ireland", user_location), "Ireland", country),
         country = ifelse(grepl("canada", user_location), "Canada", country),
         country = ifelse(grepl("toronto", user_location), "Canada", country),
         country = ifelse(grepl("philippines", user_location), "Philippines", country),
         country = ifelse(grepl("malaysia", user_location), "Malaysia", country),)

# US Cities
us_cities <- worldcities %>%
  filter(Country == "us") %>%
  mutate(Country = "US") %>%
  select(Country, City, AccentCity)

# Cross locations with cities to extract the country
tweets_location$flag_us <- purrr::map_df(tweets_location, ~ .x %in% us_cities$City)$user_location

# Add the new `country` column
tweets <- tweets %>%
  left_join(tweets_location, by = "user_location") %>%
  select(-c(n, flag_us))

# Breaks the tweet into words on each row
# in order to append the "sentiment" of the tweet
unnest_tweets <- tweets %>% 
  mutate(text = as.character(tweets$text)) %>% 
  unnest_tokens(word, text)

# Create a dataframe with stopwords
stopwords_script <- tibble(word = c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                                       "will","can","cant","dont","youve","us",
                                                       "youre","youll","theyre","whats","didnt", "just")))

# Mutate Tweets
tweets <- tweets %>%
  mutate(day_of_month = mday(date),  # Extract day of the month from the date
         month = month(date),         # Extract month from the date
         season = ifelse(month %in% c(12, 1, 2), "Winter",             # Determine the season based on the month
                         ifelse(month %in% c(3, 4, 5), "Spring", 
                                ifelse(month %in% c(6, 7, 8), "Summer", "Winter"))))  # Season based on the month

# EDA
#################
# Set plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Plot number of tweets over time
tweets %>% 
  select(date) %>% 
  mutate(date = ymd_hms(date)) %>% 
  group_by(date) %>% 
  summarize(n = n(), .groups = "drop_last") %>%
  ggplot(aes(x=date, y = n)) + 
  geom_line(size = 1.5, color = my_colors[1]) +
  coord_cartesian(clip = 'off') +
  my_theme + theme(axis.title.x = element_blank()) +
  labs(title = "Number of Tweets in Time", subtitle = "2020", y = "Frequency")

# Set different plot dimensions
options(repr.plot.width=15, repr.plot.height=10)

# Plot distribution of tweets by country
tweets %>%
  group_by(country) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  filter(country != "NA") %>%
  ggplot(aes(x = reorder(country, n), y = n, fill=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low=my_colors[2], high=my_colors[6], guide="none") +
  geom_label(aes(label=n), size=5, fill="white") +
  labs(title = "Countries Location for Tweets", subtitle = "--work in progress--") +
  my_theme + theme(axis.text.x = element_blank(),
                   axis.title = element_blank())

# Set plot dimensions again
options(repr.plot.width=15, repr.plot.height=9)

# Define labels for boxplot
labels <- c("user_favourites" = "No. Favourites", "user_followers" = "No. Followers", 
            "user_friends" = "No. Friends")

# Plot boxplot for user profile data
tweets %>%
  select(user_followers, user_favourites, user_friends) %>%
  gather(key = group_var, value = "Cases", na.rm = TRUE) %>%
  ggplot(aes(x = Cases)) +
  geom_boxplot(aes(fill = group_var), outlier.fill = "grey35", outlier.shape = 18, 
               outlier.alpha = 0.1, outlier.size = 2) +
  facet_grid(~ group_var, scales = "free", labeller = as_labeller(labels)) +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = my_colors, guide = "none") +
  labs(title = "User Profile", subtitle = "Profile Size") +
  my_theme + theme(axis.text.y = element_blank(),
                   axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
                   axis.title = element_blank())

# SENTIMENT ANALYSIS
#################
# Set plot dimensions
options(repr.plot.width=15, repr.plot.height=15)

# Create word cloud based on Bing sentiment analysis
unnest_tweets %>% 
  inner_join(bing, by="word", relationship = "many-to-many") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # Plot word cloud
  comparison.cloud(colors=my_colors[c(5, 1)], max.words = 400, title.size = 2,
                   scale = c(3,.5))

# Reset plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Plot overall mood in tweets using NRC sentiment analysis
unnest_tweets %>% 
  inner_join(nrc, "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(sentiment, sort=T) %>% 
  
  ggplot(aes(x=reorder(sentiment, n), y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=F) +
  geom_label(aes(label=format(n, big.mark = ",")), size=5, fill="white") +
  labs(x="Sentiment", y="Frequency", title="What is the overall mood in Tweets?") +
  scale_fill_gradient(low = my_colors[3], high = my_colors[1], guide="none") +
  coord_flip() + 
  my_theme + theme(axis.text.x = element_blank())

# Reset plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Plot sentiment split by most frequent words using NRC sentiment analysis
unnest_tweets %>% 
  inner_join(nrc, "word", relationship = "many-to-many") %>% 
  count(sentiment, word, sort=T) %>%
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:7) %>% 
  
  # Plot
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2, ncol = 5) +
  coord_flip() +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x="Word", y="Frequency", title="Sentiment split by most frequent words") +
  scale_fill_manual(values = c(my_colors, "#BE82AF", "#9D4387", "#DEC0D7",
                               "#40BDC8", "#80D3DB", "#BFE9ED"))

# Reset plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Plot word count distribution over intensity of sentiment using Afinn sentiment analysis
unnest_tweets %>% 
  # Count how many words per value
  inner_join(afinn, "word") %>% 
  group_by(value) %>% 
  count(value, sort=T)  %>% 
  
  # Plot
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", show.legend = F, width = 0.5, fill = my_colors[1]) +
  geom_label(aes(label=format(n, big.mark = ",")), size=5) +
  scale_x_continuous(breaks=seq(-5, 5, 1)) +
  labs(x="Score", y="Frequency", title="Word count distribution over intensity of sentiment: Neg - Pos") +
  my_theme + theme(axis.text.y = element_blank())

# Filter only main 3 countries with most tweets
data <- unnest_tweets %>%
  filter(country %in% c("US", "UK", "India", "NoCountry"))

# Create totals dataframe for the 3 countries
total_bing <- data %>% 
  inner_join(bing, by="word") %>%
  count(country) %>% 
  group_by(country) %>% 
  summarise(total_tweets = sum(n), .groups = "drop_last")

# Reset plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Prepare data for chord diagram
to_plot <- data %>% 
  # Get 'bing' and filter the data
  inner_join(bing, by="word") %>%
  
  # Sum number of words per sentiment and country
  count(sentiment, country) %>% 
  group_by(country, sentiment) %>% 
  summarise(sentiment_sum = sum(n), .groups = "drop_last") %>% 
  inner_join(total_bing, by="country") %>% 
  mutate(sentiment_perc = sentiment_sum/total_tweets) %>% 
  select(country, sentiment, sentiment_perc)

# Plot chord diagram
circos.clear()
circos.par(gap.after = c(rep(2, length(unique(to_plot[[1]])) - 1), 15,
                         rep(2, length(unique(to_plot[[2]])) - 1), 15), gap.degree=2)

myColors = c("India" = my_colors[3], "UK" = my_colors[4], "US" = my_colors[5], "NoCountry" = my_colors[2], 
             "positive" = "#D7DBDD", "negative" = "#D7DBDD")

chordDiagram(to_plot, grid.col = myColors, transparency = 0.2, annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.03, 0.06))
title("Relationship between Sentiment and Countries")

# Filter only main 3 countries with most tweets
data <- unnest_tweets %>%
  filter(country %in% c("US", "UK", "India", "NoCountry"))

# Prepare data for radar chart
char_sentiment <- data %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  group_by(country, sentiment) %>% 
  count(country, sentiment) %>% 
  select(country, sentiment, char_sentiment_count=n)

# Total count of sentiments per countries
total_char <- data %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(country) %>% 
  select(country, total=n)

# Reset plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Plot radar chart
char_sentiment %>% 
  inner_join(total_char, by="country") %>% 
  mutate(percent = char_sentiment_count / total * 100 ) %>% 
  select(-char_sentiment_count, -total) %>% 
  spread(country, percent)  %>% 
  chartJSRadar(showToolTipLabel = T, main="Countries Tweets and Emotion", maxScale=25, responsive=T,
               addDots = T, 
               colMatrix = grDevices::col2rgb(my_colors[c(3, 5, 4, 2)]),
               lineAlpha = 0.7, polyAlpha = 0.2)

# Reset plot dimensions
options(repr.plot.width=15, repr.plot.height=9)

# Plot words with biggest contributions in positive/negative sentiments using Afinn sentiment analysis
unnest_tweets %>% 
  # By word and value count number of occurrences
  inner_join(afinn, "word") %>% 
  count(word, value, sort=T) %>% 
  mutate(contribution = n * value,
         sentiment = ifelse(contribution<=0, "Negative", "Positive")) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20)  %>% 
  
  # Plot
  ggplot(aes(x=reorder(word, contribution), y=contribution, fill=sentiment)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  labs(x="Word", y="Contribution", title="Words with biggest contributions in positive/negative sentiments") +
  coord_flip() +
  scale_fill_manual(values=my_colors[c(3, 2)]) + 
  my_theme

