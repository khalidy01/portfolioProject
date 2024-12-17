library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)

netflix_data <- read.csv("netflix_transformed.csv")
netflix_data <- netflix_transformed
# Data Cleaning - Fixing the 'duration' column issue
netflix_data_cleaned <- netflix_data %>%
  mutate(
    movie_duration = ifelse(grepl("min", duration), as.numeric(gsub(" min", "", duration)), NA),
    tv_show_duration = ifelse(grepl("Season", duration), as.numeric(gsub(" Season.*", "", duration)), NA),
    type = ifelse(grepl("Season", duration), "TV Show", "Movie")  # Create a type column for grouping
  )

# Calculate the average durations for movies and TV shows
average_duration <- netflix_data_cleaned %>%
  summarise(
    avg_movie_duration = mean(movie_duration, na.rm = TRUE),
    avg_tv_show_duration = mean(tv_show_duration, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "average_duration")

# Plot the bar chart
ggplot(average_duration, aes(x = type, y = average_duration, fill = type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Comparison of Average Duration Between Movies and TV Shows", 
       x = "Type", y = "Average Duration (min)") +
  scale_fill_manual(values = c("avg_movie_duration" = "#FF9999", "avg_tv_show_duration" = "#9999FF"),
                    labels = c("Movies", "TV Shows")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

# Examine the Release Year Trend for TV Shows and Movies
release_year_trend <- netflix_data_cleaned %>%
  group_by(release_year, type) %>%
  summarise(count = n(), .groups = 'drop')

# Plot: Release Year Trend
ggplot(release_year_trend, aes(x = release_year, y = count, color = type)) +
  geom_line(size = 1.2) +
  labs(title = "Release Year Trend for TV Shows and Movies", x = "Release Year", y = "Count of Titles") +
  scale_color_manual(values = c("Movie" = "#FF6666", "TV Show" = "#6666FF")) +
  theme_minimal()

# Analyze the Distribution of Content Ratings
rating_distribution <- netflix_data_cleaned %>%
  count(rating)

# Plot: Distribution of Content Ratings
ggplot(rating_distribution, aes(x = reorder(rating, -n), y = n, fill = rating)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Distribution of Content Ratings on Netflix", x = "Rating", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

# Analyze the Growth of Total Movies and TV Shows by Year
growth_by_year <- netflix_data_cleaned %>%
  group_by(release_year, type) %>%
  summarise(count = n(), .groups = 'drop')  # Add .groups argument to avoid warnings

# Plot: Growth of Movies and TV Shows by Year
ggplot(growth_by_year, aes(x = release_year, y = count, color = type)) +
  geom_line(size = 1.2) +
  labs(title = "Growth of Movies and TV Shows by Year on Netflix", x = "Release Year", y = "Count of Titles") +
  scale_color_manual(values = c("Movie" = "#FF3333", "TV Show" = "#3333FF")) +
  theme_minimal()

# Analyze the Most Common Genres Across All Countries
top_genres_all_countries <- netflix_data_cleaned %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE) %>%
  slice_max(n, n = 5) 

# Plot: Top 5 Most Common Genres Across All Countries
ggplot(top_genres_all_countries, aes(x = fct_reorder(listed_in, n), y = n, fill = listed_in)) +
  geom_bar(stat = "identity") +  # Create a bar plot
  labs(title = "Top 5 Most Common Genres Across All Countries", x = "Genre", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Show summary of the cleaned data
summary(netflix_data_cleaned)
