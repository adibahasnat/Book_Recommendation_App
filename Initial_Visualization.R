library(tidyverse)
library(dplyr)
library(ggplot2)


books_data <- read.csv("books_1.Best_Books_Ever.csv")


# Data Cleaning: Convert 'rating' to numeric and remove rows with invalid ratings
books_data$rating <- as.numeric(books_data$rating)
books_data_cleaned <- books_data %>%
  filter(!is.na(rating))

# Extract and count genres
genres <- books_data_cleaned %>%
  filter(!is.na(genres)) %>%
  mutate(genres = str_replace_all(genres, "\\[|\\]|'", "")) %>%
  separate_rows(genres, sep = ", ") %>%
  count(genres, sort = TRUE)

# Get the top 10 genres
top_genres <- genres %>%
  slice_max(n, n = 10)

# Plot the Top 10 Genres
ggplot(top_genres, aes(x = reorder(genres, n), y = n)) +
  geom_bar(stat = "identity", fill = "#52A49A") +
  coord_flip() +
  labs(
    title = "Top 10 Genres in the Dataset",
    x = "Genre",
    y = "Number of Books"
  ) +
  theme_minimal()



# Ensure firstPublishDate is clean and extract the year
books_data_cleaned <- books_data_cleaned %>%
  filter(!is.na(firstPublishDate)) %>%
  mutate(year = as.numeric(str_extract(firstPublishDate, "\\d{4}"))) %>%
  filter(!is.na(year))

# Calculate average rating per year
ratings_by_year <- books_data_cleaned %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>%
  filter(!is.na(avg_rating))

# Plot the trend of ratings over time
ggplot(ratings_by_year, aes(x = year, y = avg_rating)) +
  geom_line(color = "#52A49A", size = 1) +
  labs(
    title = "Average Book Ratings Over Time",
    x = "Year",
    y = "Average Rating"
  ) +
  theme_minimal()


# Calculate average rating for each genre
avg_rating_by_genre <- books_data_cleaned %>%
  filter(!is.na(genres)) %>%
  mutate(genres = str_replace_all(genres, "\\[|\\]|'", "")) %>%
  separate_rows(genres, sep = ", ") %>%
  group_by(genres) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE), count = n()) %>%
  filter(count >= 10) %>% # Filter genres with at least 10 books for better insights
  slice_max(avg_rating, n = 10)

# Plot the average rating by genre
ggplot(avg_rating_by_genre, aes(x = reorder(genres, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 Genres by Average Rating",
    x = "Genre",
    y = "Average Rating"
  ) +
  theme_minimal()


# Summary statistics for rating and price
summary(books_data_cleaned[, c("rating", "price")])



# Calculate correlation
correlation <- cor(books_data_cleaned$rating, books_data_cleaned$price, use = "complete.obs")

# Print correlation
print(paste("Correlation between rating and price:", correlation))

# Build a linear regression model
linear_model <- lm(rating ~ price + numRatings, data = books_data_cleaned)

# Print regression summary
summary(linear_model)

# Display structure of the dataset
str(books_data)

# Summary of the dataset
summary(books_data)


# Create a variable summary
variable_summary <- data.frame(
  Variable = colnames(books_data),
  Type = sapply(books_data, class),
  MissingValues = sapply(books_data, function(x) sum(is.na(x))),
  UniqueValues = sapply(books_data, function(x) length(unique(x)))
)

# View the summary
print(variable_summary)

# Get the dimensions of the dataset
dim(books_data)

# Number of rows
nrow(books_data)

# Number of columns
ncol(books_data)




