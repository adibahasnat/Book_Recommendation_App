book_data <- read.csv("books_1.Best_Books_Ever.csv")

# Select the first 1000 rows
first_1000_books <- head(book_data, 1000)

# Replace blank cells in 'setting' column with 'Non-applicable'
first_1000_books$setting[first_1000_books$setting == "" | is.na(first_1000_books$setting)] <- "Non-applicable"

# Fill empty 'firstPublishDate' cells with values from 'publishDate'
first_1000_books$firstPublishDate[is.na(first_1000_books$firstPublishDate) | first_1000_books$firstPublishDate == ""] <- 
  first_1000_books$publishDate[is.na(first_1000_books$firstPublishDate) | first_1000_books$firstPublishDate == ""]

# Replace empty or NA 'publishDate' with values from 'firstPublishDate'
first_1000_books$publishDate[is.na(first_1000_books$publishDate) | first_1000_books$publishDate == ""] <- 
  first_1000_books$firstPublishDate[is.na(first_1000_books$publishDate) | first_1000_books$publishDate == ""]

# Identify rows where 'publishDate' is still NA
missing_publish_dates <- first_1000_books[is.na(first_1000_books$publishDate), c("publishDate", "firstPublishDate")]
print(missing_publish_dates)

# Inspect rows where 'publishDate' is NA
missing_publish_dates <- first_1000_books[is.na(first_1000_books$publishDate), c("publishDate", "firstPublishDate")]
print(missing_publish_dates)

# Check how many rows have 'firstPublishDate' in the future
future_dates <- first_1000_books[as.Date(first_1000_books$firstPublishDate, format = "%Y-%m-%d") > Sys.Date(), ]
print(future_dates)


# Fix future dates by changing the year to the corresponding past year
first_1000_books$publishDate <- as.Date(first_1000_books$publishDate, format = "%Y-%m-%d")
first_1000_books$firstPublishDate <- as.Date(first_1000_books$firstPublishDate, format = "%Y-%m-%d")

# Replace future dates in 'publishDate'
future_dates <- which(first_1000_books$publishDate > Sys.Date())
first_1000_books$publishDate[future_dates] <- as.Date(sub("^20", "19", first_1000_books$publishDate[future_dates]))

# Replace future dates in 'firstPublishDate'
future_first_publish_dates <- which(first_1000_books$firstPublishDate > Sys.Date())
first_1000_books$firstPublishDate[future_first_publish_dates] <- as.Date(sub("^20", "19", first_1000_books$firstPublishDate[future_first_publish_dates]))


# Count NA values in 'publishDate'
na_publish_date <- sum(is.na(first_1000_books$firstPublishDate))
print(na_publish_date)

# Count NA values for each column
na_count <- sapply(first_1000_books, function(column) sum(is.na(column)))

# View the counts
print(na_count)


# Create a summary of NA counts
na_summary <- data.frame(
  Column = names(na_count),
  NA_Count = na_count
)

# View the summary
print(na_summary)


# Remove the 'price' and 'edition' column
first_1000_books <- first_1000_books[, !colnames(first_1000_books) %in% "price"]
first_1000_books <- first_1000_books[, !colnames(first_1000_books) %in% c("edition")]

# Fill missing values with appropriate defaults
first_1000_books$series[first_1000_books$series == "" | is.na(first_1000_books$series)] <- "Standalone"
first_1000_books$description[first_1000_books$description == "" | is.na(first_1000_books$description)] <- "Description not available"
first_1000_books$language[first_1000_books$language == "" | is.na(first_1000_books$language)] <- "Unknown"
first_1000_books$bookFormat[first_1000_books$bookFormat == "" | is.na(first_1000_books$bookFormat)] <- "Unknown"
first_1000_books$publisher[first_1000_books$publisher == "" | is.na(first_1000_books$publisher)] <- "Unknown"
first_1000_books$coverImg[first_1000_books$coverImg == "" | is.na(first_1000_books$coverImg)] <- "No Image Available"

# Update 'pages' value to 260 if 'title' is "Brainwalker"
first_1000_books$pages[first_1000_books$title == "Brainwalker"] <- 260

# Custom replacement values for each column
replacement_values <- list(
  setting = "Non-applicable",
  characters = "Unknown",
  description = "No description to show",
  awards = "No awards to show"
)

# Replace '[]' with specific values for each column
for (col in names(replacement_values)) {
  first_1000_books[[col]][first_1000_books[[col]] == "[]"] <- replacement_values[[col]]
}

# Verify the changes
sapply(names(replacement_values), function(col) {
  sum(first_1000_books[[col]] == "[]")
})


# Function to count empty cells (NA or blank strings) for each column
empty_cells_count <- sapply(first_1000_books, function(column) {
  sum(is.na(column) | column == "")
})

# Convert to a data frame for easier reading
empty_cells_summary <- data.frame(
  Column = names(empty_cells_count),
  EmptyCells = empty_cells_count
)

# View the summary
print(empty_cells_summary)



# Check the first few values of the date columns
head(first_1000_books$publishDate)
head(first_1000_books$firstPublishDate)

# Convert dates from MM/DD/YY format to Date objects
date_columns <- c("publishDate", "firstPublishDate")

for (col in date_columns) {
  first_1000_books[[col]] <- as.Date(first_1000_books[[col]], format = "%m/%d/%y")
}

# Verify the changes
str(first_1000_books[date_columns])



write.csv(first_1000_books, "updated_books_dataset.csv")

## manually accessed the excel file and changed some missing data

# Read the dataset
new <- read.csv("updated_books_dataset.csv", stringsAsFactors = FALSE)

# Convert 'publishDate' and 'firstPublishDate' to Date format
new$publishDate <- as.Date(new$publishDate, format = "%m/%d/%y")
new$firstPublishDate <- as.Date(new$firstPublishDate, format = "%m/%d/%y")

# Create the 'year' column based on the logic
new$year <- ifelse(
  !is.na(new$firstPublishDate) & new$publishDate != new$firstPublishDate,
  format(new$firstPublishDate, "%y"),  # Use 'firstPublishDate' if different
  format(new$publishDate, "%y")       # Otherwise, use 'publishDate'
)

# Convert 'year' to a full year
new$year <- ifelse(
  as.numeric(new$year) > 24, 
  paste0("19", new$year),  # Years > 25 are prefixed with '19'
  paste0("20", new$year)   # Years <= 25 are prefixed with '20'
)

# Verify the new column
head(new[, c("publishDate", "firstPublishDate", "year")])

# Manually update the year for specific titles
new$year[new$title == "Onyx"] <- "2012"
new$year[new$title == "A Passage to India"] <- "1924"
new$year[new$title == "Twenty Love Poems and a Song of Despair"] <- "1924"
new$year[new$title == "The Night Before Christmas"] <- "1823"
new$year[new$title == "The Prophet"] <- "1923"
new$year[new$title == "The Complete Works"] <- "1993"
new$year[new$title == "Ulysses"] <- "1922"
new$year[new$title == "Siddhartha"] <- "1922"
new$year[new$title == "The Velveteen Rabbit"] <- "1922"
new$year[new$title == "The Waste Land"] <- "1922"
new$year[new$title == "We"] <- "1924"
new$year[new$title == "The Divine Comedy"] <- "1320"
new$year[new$title == "The Age of Innocence"] <- "1920"
new$year[new$title == "The Aeneid"] <- "1469"
new$year[new$title == "Robinson Crusoe"] <- "1719"
new$year[new$title == "Demian: Die Geschichte von Emil Sinclairs Jugend"] <- "1919"
new$year[new$title == "Ivanhoe"] <- "1819"
new$year[new$title == "Frankenstein: The 1818 Text"] <- "1818"
new$year[new$title == "My Ántonia"] <- "1918"
new$year[new$title == "Persuasion"] <- "1817"
new$year[new$title == "Northanger Abbey"] <- "1817"
new$year[new$title == "The Metamorphosis"] <- "1915"
new$year[new$title == "The Arabian Nights"] <- "1815"
new$year[new$title == "Of Human Bondage"] <- "1915"
new$year[new$title == "Emma"] <- "1815"
new$year[new$title == "The Magic Mountain"] <- "1924"
new$year[new$title == "A Portrait of the Artist as a Young Man"] <- "1916"
new$year[new$title == "Mansfield Park"] <- "1814"
new$year[new$title == "Dubliners"] <- "1914"
new$year[new$title == "Pride and Prejudice"] <- "1813"
new$year[new$title == "The Complete Novels"] <- "1928"
new$year[new$title == "Swann's Way"] <- "1913"
new$year[new$title == "Peter Pan"] <- "1904"
new$year[new$title == "Sense and Sensibility"] <- "1811"
new$year[new$title == "Holy Bible: King James Version"] <- "1611"
new$year[new$title == "Ethan Frome"] <- "1911"
new$year[new$title == "The Tempest"] <- "1611"
new$year[new$title == "The Secret Garden"] <- "1911"
new$year[new$title == "Howards End"] <- "1910"
new$year[new$title == "The Phantom of the Opera"] <- "1909"
new$year[new$title == "Anne of Avonlea"] <- "1909"
new$year[new$title == "Grimm's Fairy Tales"] <- "1812"
new$year[new$title == "Shakespeare's Sonnets"] <- "1609"
new$year[new$title == "Anne of Green Gables"] <- "1908"
new$year[new$title == "The Wind in the Willows"] <- "1908"
new$year[new$title == "Faust, First Part"] <- "1808"
new$year[new$title == "A Room with a View"] <- "1908"
new$year[new$title == "White Fang"] <- "1906"
new$year[new$title == "Macbeth"] <- "1623"
new$year[new$title == "Don Quixote"] <- "1605"
new$year[new$title == "A Little Princess"] <- "1905"
new$year[new$title == "The Complete Calvin and Hobbes"] <- "1995"
new$year[new$title == "King Lear"] <- "1608"
new$year[new$title == "The Scarlet Pimpernel"] <- "1905"
new$year[new$title == "The Jungle"] <- "1894"
new$year[new$title == "The House of Mirth"] <- "1905"
new$year[new$title == "The Call of the Wild"] <- "1903"
new$year[new$title == "Othello"] <- "1622"
new$year[new$title == "Hamlet"] <- "1623"
new$year[new$title == "The Story of My Life"] <- "1903"
new$year[new$title == "The Hound of the Baskervilles"] <- "1902"
new$year[new$title == "Twelfth Night"] <- "1623"
new$year[new$title == "The Odyssey"] <- "1614"
new$year[new$title == "The Wonderful Wizard of Oz"] <- "1990"
new$year[new$title == "The Canterbury Tales"] <- "1400"
new$year[new$title == "The Iliad/The Odyssey"] <- "1616"
new$year[new$title == "Beowulf"] <- "1815"
new$year[new$title == "Tao Te Ching"] <- "-0400"
new$year[new$title == "The Art of War"] <- "-0500"
new$year[new$title == "The Bhagavad Gita"] <- "-0200"
new$year[new$title == "Heart of Darkness"] <- "1899"
new$year[new$title == "The Awakening"] <- "1897"
new$year[new$title == "Julius Caesar"] <- "1623"
new$year[new$title == "The War of the Worlds"] <- "1898"
new$year[new$title == "Much Ado About Nothing"] <- "1600"
new$year[new$title == "The Rime of the Ancient Mariner"] <- "1798"
new$year[new$title == "Dracula"] <- "1897"
new$year[new$title == "Cyrano de Bergerac"] <- "1897"
new$year[new$title == "The Ultimate Hitchhiker's Guide to the Galaxy"] <- "1979"
new$year[new$title == "The Merchant of Venice"] <- "1597"
new$year[new$title == "Romeo and Juliet"] <- "1597"
new$year[new$title == "A Midsummer Night's Dream"] <- "1600"
new$year[new$title == "The Time Machine"] <- "1895"
new$year[new$title == "Jude the Obscure"] <- "1895"
new$year[new$title == "The Jungle Books"] <- "1894"
new$year[new$title == "The Complete Works"] <- "1623"
new$year[new$title == "The Taming of the Shrew"] <- "1623"
new$year[new$title == "The Adventures of Sherlock Holmes"] <- "1892"
new$year[new$title == "Leaves of Grass"] <- "1855"
new$year[new$title == "The Yellow Wallpaper and Other Stories"] <- "1892"
new$year[new$title == "Tess of the D'Urbervilles"] <- "1891"
new$year[new$title == "The Picture of Dorian Gray"] <- "1890"
new$year[new$title == "The Complete Poems of Emily Dickinson"] <- "1976"
new$year[new$title == "A Connecticut Yankee in King Arthur's Court"] <- "1889"
new$year[new$title == "Songs of Innocence and of Experience"] <- "1794"
new$year[new$title == "A Study in Scarlet"] <- "1887"
new$year[new$title == "The Strange Case of Dr. Jekyll and Mr. Hyde"] <- "1886"
new$year[new$title == "The Adventures of Huckleberry Finn"] <- "1884"
new$year[new$title == "Thus Spoke Zarathustra"] <- "1883"
new$year[new$title == "The Portrait of a Lady"] <- "1881"
new$year[new$title == "Heidi"] <- "1881"
new$year[new$title == "The Republic"] <- "-0375"
new$year[new$title == "Meditations"] <- "0161"
new$year[new$title == "The Brothers Karamazov"] <- "1880"
new$year[new$title == "Treasure Island"] <- "1883"
new$year[new$title == "A Doll's House"] <- "1879"
new$year[new$title == "The Pilgrim's Progress"] <- "1678"
new$year[new$title == "Anna Karenina"] <- "1877"
new$year[new$title == "Black Beauty"] <- "1877"
new$year[new$title == "Far From the Madding Crowd"] <- "1874"
new$year[new$title == "Around the World in Eighty Days"] <- "1873"
new$year[new$title == "Alice's Adventures in Wonderland & Through the Looking-Glass"] <- "1871"
new$year[new$title == "Middlemarch"] <- "1872"
new$year[new$title == "Twenty Thousand Leagues Under the Sea"] <- "1870"
new$year[new$title == "The Idiot"] <- "1869"
new$year[new$title == "Little Women"] <- "1869"
new$year[new$title == "Paradise Lost"] <- "1667"
new$year[new$title == "War and Peace"] <- "1869"
new$year[new$title == "Crime and Punishment"] <- "1866"
new$year[new$title == "Alice in Wonderland"] <- "1865"
new$year[new$title == "Les Misérables"] <- "1862"
new$year[new$title == "Great Expectations"] <- "1861"
new$year[new$title == "Silas Marner"] <- "1861"
new$year[new$title == "A Tale of Two Cities"] <- "1859"
new$year[new$title == "Candide"] <- "1759"
new$year[new$title == "The Origin of Species"] <- "1859"
new$year[new$title == "The Woman in White"] <- "1859"
new$year[new$title == "Madame Bovary"] <- "1857"
new$year[new$title == "Walden"] <- "1854"
new$year[new$title == "Bleak House"] <- "1852"
new$year[new$title == "Uncle Tom's Cabin"] <- "1852"
new$year[new$title == "Moby-Dick or, the Whale"] <- "1851"
new$year[new$title == "The Scarlet Letter"] <- "1850"
new$year[new$title == "David Copperfield"] <- "1850"
new$year[new$title == "Vanity Fair"] <- "1848"
new$year[new$title == "Wuthering Heights"] <- "1847"
new$year[new$title == "Jane Eyre"] <- "1847"
new$year[new$title == "القرآن الكريم"] <- "0622"
new$year[new$title == "The Iliad"] <- "-0800"
new$year[new$title == "The Three Musketeers"] <- "1844"
new$year[new$title == "The Sorrows of Young Werther"] <- "1774"
new$year[new$title == "The Tell-Tale Heart and Other Writings"] <- "1843"
new$year[new$title == "A Christmas Carol"] <- "1843"
new$year[new$title == "Antigone"] <- "-0441"
new$year[new$title == "Oliver Twist"] <- "1839"
new$year[new$title == "Eugene Onegin"] <- "1833"
new$year[new$title == "The Prince"] <- "1532"
new$year[new$title == "The Hunchback of Notre-Dame"] <- "1831"
new$year[new$title == "The Book of Mormon: Another Testament of Jesus Christ"] <- "1830"
new$year[new$title == "Oedipus Rex"] <- "-0429"
new$year[new$title == "Gulliver's Travels"] <- "1726"
new$year[new$title == "The Last of the Mohicans"] <- "1826"




# Save the updated dataset
write.csv(new, "updated_books_dataset.csv", row.names = FALSE)

books_data <- read.csv("updated_books_dataset.csv")

# Extract the genres column
genres_column <- books_data$genres

# Split the genres into individual components
split_genres <- unlist(strsplit(genres_column, ", "))

# Remove unwanted characters like "[" and "]" and single quotes
clean_genres <- gsub("\\[|\\]|'", "", split_genres)

# Get unique genres
unique_genres <- unique(clean_genres)

# Sort the genres alphabetically
sorted_genres <- sort(unique_genres)

# View the cleaned list of unique genres
print(sorted_genres)

genre_counts <- table(clean_genres)
print(genre_counts)
