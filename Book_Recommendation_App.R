# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinyWidgets")

library(shinyWidgets)
library(rsconnect) 
library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(tidyr)
library(ggplot2)

# Load the dataset
books_data <- read.csv("combined_dfcorrectUPDATED.csv", stringsAsFactors = FALSE)

# Load the keywords file
# top_terms <- read.csv("top_terms.csv", stringsAsFactors = FALSE)

# Clean the genres and settings columns
books_data$genres <- gsub("\\[|\\]|'", "", books_data$genres)
books_data$setting <- gsub("\\[|\\]|'", "", books_data$setting)

# Ensure the year column has no commas
books_data$year <- as.numeric(gsub(",", "", books_data$year))

# Define a function to truncate text for "Show More" feature
truncate_text <- function(text, n = 50) {
  if (nchar(text) > n) {
    truncated <- paste0(substr(text, 1, n), "...")
    paste0(
      truncated, 
      " <button class='show-more' data-text='", text, 
      "' data-truncated='", truncated, "'>Show More</button>"
    )
  } else {
    text
  }
}

# building data table:
data_dictionary <- data.frame(
  Variable = c(
    "bookId", "title", "series", "author", "rating", "description", "language",
    "isbn", "genres", "characters", "bookFormat", "pages",
    "publisher", "publishDate", "firstPublishDate", "awards", "numRatings", "ratingsByStars",
    "likedPercent",
    "setting", "coverImg", "bbeScore", "bbeVotes", "year"),
  Description = c(
    "Book Identifier as in goodreads.com",
    "Book title",
    "Series name (if it is one, otherwise marked as standalone)",
    "Book’s author",
    "Global goodreads rating",
    "Book’s description",
    "Book’s language",
    "Book’s ISBN",
    "Book’s genres",
    "Main characters",
    "Type of binding",
    "Number of pages",
    "Editorial",
    "Chapter number where the interaction occurs.",
    "Publication date",
    "Publication date of the first edition",
    "List of awards",
    "Number of total ratings",
    "Number of ratings by stars",
    "Story setting",
    "URL to cover image",
    "Score in Best Books Ever List",
    "Number of votes in Best Books Ever List",
    "Year of the publication date of the first edition"),
  Type = c(
    "character", "character", "character", "character", "numeric", "character",
    "character", "character", "character", "character", "character", "integer", "character",
    "character", "character", "character", "integer", "numeric", "integer", "character",
    "character", "integer", "integer", "integer")
)


# Define UI
ui <- navbarPage(
  "Book Recommendation App",  # App Title
  # Use styles.css for custom theme
  tags$head(
    includeCSS("www/styles.css"),
    tags$script(HTML("
    $(document).on('click', '.show-more', function() {
      var fullText = $(this).attr('data-text');
      $(this).parent().html(fullText + \" <button class='show-less' data-text='\" + $(this).attr('data-truncated') + \"'>Show Less</button>\");
    });

    $(document).on('click', '.show-less', function() {
      var truncatedText = $(this).attr('data-text');
      $(this).parent().html(truncatedText + \" <button class='show-more' data-text='\" + $(this).attr('data-full') + \"' data-truncated='\" + truncatedText + \"'>Show More</button>\");
    });
  ")),
    tags$style(HTML("
      #random, #customize {
        margin-right: 15px;
      }
      .button-area {
        background-color: #f7f7f5; /* Matches overall theme */
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      table {
        table-layout: fixed; /* Ensures fixed column width */
        word-wrap: break-word; /* Handles long text */
      }
      .empty-message {
        text-align: center;
        font-size: 18px;
        color: #555;
        margin-top: 20px;
      }
    "))
  ),
  
  # Main Page Tab
  tabPanel(
    "Home",
    fluidPage(
      tags$div(
        class = "home-header",
        tags$img(src = "books.jpg", class = "home-logo"),
        h1("Welcome to the Book Recommendation App!")
      ),
      p("Explore books, analyze data, and get personalized recommendations.", align = "center"),
      h2("About the Dataset"),
      p("This dataset contains ~52,000 of the most popular books listed on ", 
        tags$a(href = "https://www.goodreads.com", "GoodReads", target = "_blank"), 
        ", which is a website where users can create profiles, add books to their accounts, and write or read reviews for books."
      ),
      p("The project we took the source code from on GitHub (", 
        tags$a(href = "https://github.com/scostap/goodreads_bbe_dataset", "GoodReads BBE Dataset", target = "_blank"), 
        ") was a data science project that sought to extract data from the GoodReads website."),
      p("The dataset is robust and complete, with only a few minor processing issues. The original work was done by students from the University of Catalonia, Barcelona, Spain."),
      
      h2("Similar Works"),
      tags$ul(
        tags$li(tags$a(href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3356349", "Machine Learning Techniques for Book Recommendations", target ="_blank"), ": Analyzed different BRS’ for librarians in the realm of recommendation technology, specifically for books, and compared supervised, unsupervised, semi-supervised, and reinforcement learning algorithms."),
        tags$li(tags$a(href = "https://ieeexplore.ieee.org/abstract/document/7919606?casa_token=lW-jh6YRZisAAAAA:PtVY27sBUnMTz4R5kMR9kLDe3okyLkpWtAGdpBzmGmvoaTNyWq2jsRTTS_KEZISXw5oR7CJF61k", "A Novel Approach for Book Recommendation Systems", target = "_blank"), ": This study found that the usual ML methods for book recommendations struggle with the sheer amount of data and social media opinions available. It proposed an FPIntersect Algorithm instead."),
        tags$li(tags$a(href = "https://ieeexplore.ieee.org/abstract/document/6637421?casa_token=_npqKriS63gAAAAA:1bpqLkiEUNuxB82capRhEELlAOP906llgmU8rRDcyt4awhkV0UqFxXfGN0Bola8B7KUyrzsAno4", "Book Recommendation System (BRS) Using Opinion Mining Techniques", target = "_blank"), ": This technique collects previous reviews from a user searching for a computer science book and generates a list of top 10 books for them based on feature importance.")
      ),
      
      h2("Project Objective"),
      p("Our goal is to generate a list of the top 10 books on GoodReads for a user based on various input prompts using ", 
        tags$a(href = "https://shiny.rstudio.com", "ShinyApps", target = "_blank"), 
        " and machine learning techniques for balanced recommendations that reflect the user’s prime interests."),
      p("For example: I want to read an older book. I want it to be at least 200 pages long and explore themes like death and destruction. I want it to be considered a good book or a classic, so I prefer if it had at least one award associated with it or was categorized as a classic by genre."),
      
      h2("Data Cleaning and Processing"),
      p("Here are the steps we took to clean and process our data:"),
      tags$ul(
        tags$li("We focused on the top 1,000 rows out of 52,000. These books are considered the most “popular” of all time and have a good distribution of rating scores."),
        tags$li("Identified and handled missing values."),
        tags$li("Cleaned and standardized text data (e.g., characters with umlauts)."),
        tags$li("Converted data types, imputed missing values, and extracted useful information (e.g., publication year and first publication date)."),
        tags$li("Populated empty 'Settings' values (e.g., 'Non-applicable' for dystopian universes)."),
        tags$li("Removed unnecessary columns such as 'edition' and 'price'."),
        tags$li("Converted 'publishDate' and 'firstPublishDate' from character to date type using R’s lubridate package."),
        tags$li("Used 'publishDate' and 'firstPublishDate' to create a 'year' column for easier filtering.")
      ),
      
      h3("Data Snapshot"),
      DT::dataTableOutput("data_dictionary_table"),
      
      
      h2("Explore Our App"),
      p("Using this app, you can:"),
      tags$ul(
        tags$li("Browse random recommendations."),
        tags$li("Customize your search by genre, series type, rating, page count, publication date, and more."),
        tags$li("Search for books using specific keywords to find titles that best match your interests."),
        tags$li("Discover books based on awards and other features that make them stand out.")
      ),
      
      h3("App Building Process"),
      p("The process of building this app involved several key steps to ensure that it provides accurate and meaningful book recommendations. Below is an overview of the steps taken during the development process:"),
      
      tags$ul(
        tags$li("Using R's Shiny framework, we designed a user-friendly interface with options for filtering books by various criteria such as genre, awards, and keywords."),
        tags$li("We utilized natural language processing techniques, such as TF-IDF, to identify keywords and understand the unique themes of each book."),
        tags$li("We tested the app with different inputs to ensure it works seamlessly and refined the user experience."),
        tags$li("To create a cohesive and visually appealing app, we customized the design using CSS to align with the thematic style of the application.")
      ),
      
      h4("Natural Language Processing (NLP) Techniques Used"),
      p("We used several key NLP techniques to extract meaningful insights from the book descriptions. Here is a detailed breakdown of the process:"),
      
      tags$ul(
        tags$li(tags$b("Packages used:"), " tidytext, tfidf"),
        tags$li(tags$b("TF-IDF:"), " Term Frequency-Inverse Document Frequency is a statistical measure used to evaluate the importance of a word in a document relative to a collection of documents (corpus). It helps identify words characteristic of specific documents and filters out common words such as 'the', 'and', or 'of'."),
        tags$li(tags$b("Preprocessing:"), 
                tags$ul(
                  tags$li("Cleaned the text by removing filler/stop words (e.g., 'the', 'and', 'of') to focus on meaningful words."),
                  tags$li("Tokenized the descriptions into individual words.")
                )),
        tags$li(tags$b("TF-IDF Calculation:"),
                tags$ul(
                  tags$li("Calculated the TF-IDF score for each word in each book description."),
                  tags$li("Identified terms that are specific to a book, rather than general terms appearing across many books.")
                )),
        tags$li(tags$b("Keyword Extraction:"),
                tags$ul(
                  tags$li("Sorted words by their TF-IDF scores for each book."),
                  tags$li("Selected the top words for each book from its description to capture its essence.")
                ))
      ),
      
      p("Utilizing tidytext and TF-IDF allowed us to extract the most meaningful words from each book description and ignore filler words. These extracted keywords helped us build a term importance matrix, which was essential for clustering books based on their descriptions. This analysis was a cornerstone of our recommendation feature, allowing us to recommend books with similar themes or keywords that match a user's search criteria."),
      
      p("Overall, these steps ensured that the app delivers robust and meaningful recommendations for users, while maintaining a user-friendly interface.")
    )
  ),
  
  
  # EDA Tab
  tabPanel(
    "EDA",
    fluidPage(
      h2("Exploratory Data Analysis"),
      
      # Visualization 1: Distribution of Ratings
      h3("Distribution of Ratings"),
      tags$div(
        class = "centered-plot plot-container",
        tags$div(class = "plot-output", plotOutput("rating_distribution"), width = "100%")
      ),
      tags$ul(
        tags$li("This visualization shows the distribution of ratings across all books."),
        tags$li("We observe that most books tend to have high ratings, clustering around 4 to 5 stars."),
        tags$li("This trend is typical for popular books as they are more likely to be rated positively by a broader audience.")
      ),
      
      # Visualization 2: Popular Genres
      h3("Top 10 Most Popular Genres"),
      tags$div(
        class = "centered-plot plot-container",
        tags$div(class = "plot-output", plotOutput("popular_genres"), width = "100%")
      ),
      tags$ul(
        tags$li("This bar chart displays the top 10 most popular genres based on the number of books."),
        tags$li("Genres such as Fantasy, Young Adult, and Fiction are highly represented."),
        tags$li("This trend reflects the preferences of GoodReads users, who are often fans of series and contemporary popular literature.")
      ),
      
      # Visualization 3: Pages vs. Ratings
      h3("Pages vs. Ratings"),
      tags$div(
        class = "centered-plot plot-container",
        tags$div(class = "plot-output", plotOutput("pages_vs_ratings"))
      ),
      tags$ul(
        tags$li("This scatter plot shows the relationship between the number of pages and the average rating."),
        tags$li("Books with fewer pages tend to have a wider spread of ratings, whereas longer books (e.g., epics) tend to maintain higher ratings."),
        tags$li("This could indicate that dedicated readers are more likely to give higher ratings to longer books.")
      ),
      
      # Visualization 4: Publish Year Trends
      h3("Trends in Book Publication Over the Years"),
      tags$div(
        class = "centered-plot plot-container",
        tags$div(class = "plot-output", plotOutput("publish_year_trends"))
      ),
      tags$ul(
        tags$li("This line chart tracks the number of books published per year in the dataset."),
        tags$li("A noticeable spike can be seen in the 2000s, likely due to the surge in Young Adult and Fantasy series popularity."),
        tags$li("Recent years show slightly fewer entries, possibly due to dataset limitations or the time it takes for new books to gain popularity.")
      ),
      
      # Visualization 5: Awards Distribution
      h3("Books by Awards"),
      tags$div(
        class = "centered-plot plot-container",
        tags$div(class = "plot-output", plotOutput("awards_distribution"))
      ),
      tags$ul(
        tags$li("This pie chart shows the proportion of books with 0, 1, or 2+ awards."),
        tags$li("Most books in the dataset have no awards, but a significant proportion have received at least one."),
        tags$li("This trend highlights that while awards are important, many popular books achieve success without them.")
      )
    )
  ),
  
  # Recommendation App Tab
  tabPanel(
    "Recommendation App",
    fluidPage(
      h2("Book Recommendation System"),
      sidebarLayout(
        sidebarPanel(
          div(
            class = "button-area",
            actionButton("random", "Random Recommendations"),
            actionButton("customize", "Customize Recommendations")
          ),
          conditionalPanel(
            condition = "input.customize > 0",
            selectInput(
              "genre",
              "Select Genre:",
              choices = sort(unique(unlist(strsplit(books_data$genres, ", ")))),
              multiple = TRUE
            ),
            selectInput(
              "series_type",
              "Series or Standalone:",
              choices = c("Any", "Series", "Standalone"),
              selected = "Any"
            ),
            sliderInput(
              "rating",
              "Minimum Rating:",
              min = 0,
              max = 5,
              value = 3.5,
              step = 0.1
            ),
            sliderInput(
              inputId = "pages",
              label = "Page Range:",
              min = 0,
              max = 4000,
              value = c(100, 500),
              step = 100,
              ticks = TRUE,
              post = " pages"
            ),
            sliderTextInput(
              inputId = "publish_date",
              label = "Publish Date Range:",
              choices = as.character(seq(min(books_data$year, na.rm = TRUE), max(books_data$year, na.rm = TRUE), by = 20)),
              selected = c("2000", "2020")
            ),
            selectInput(
              "awards",
              "Awards:",
              choices = c("Any", "No awards", "1 award", "2+ awards"),
              selected = "Any"
            ),
            textInput(
              "keyword",
              "Keyword Search (Optional):",
              placeholder = "Enter a keyword"
            ),
            actionButton("generate", "Generate Recommendations")
          )
        ),
        mainPanel(
          conditionalPanel(
            condition = "output.tableAvailable == false",
            p("Please click on 'Random Recommendations' or customize your input to generate recommendations.")
          ),
          uiOutput("book_table_ui")  # Use dynamic UI for table with Show More feature
        )
      )
    )
  ),
  # Limitations, Conclusions, and Takeaways
  tabPanel(
    "Limitations & Takeaways",
    fluidPage(
      h2("Limitations, Conclusions, and Takeaways"),
      h3("Limitations"),
      tags$ul(
        tags$li("Goodreads is based a lot on user opinion, not always specific critic/author opinion (though many of them are verified on Goodreads). So some of these could be skewed towards a specific audience of book readers, rather than a casual reader or someone wanting to start getting into reading."),
        tags$li("Because of this audience, the data could be skewed towards fantasy/YA series, which then may only work well as a recommendation feature for people who are also interested in those genres especially. However, in general, there exists a decent distribution of genres, standalones, classics, and newer publications.")
      ),
      h3("Conclusions & Takeaways"),
      tags$ul(
        tags$li("In conclusion, we were able to generate a list of recommendations for a user who wants to read a recommended popular, well-known book based on their input. Using tidytext and the tf-idf matrix allowed us to parse through the most important keywords and descriptions and deliver meaningful results."),
        tags$li("Since book recommendation software has always been a very robust topic with machine learning, this was made on a smaller scale to understand the skeleton model of finding keywords and the most important features in book descriptions. Having Goodreads also offers an opinion-based model, and the dataset was very helpful and insightful."),
        tags$li("We recommend using this dataset in the future. It could potentially combine with the Gutenberg dataset in R, which has entire texts downloaded for reference, with this dataset (for what matches) for an even more robust and intricate model. However, considering the broader implications of ensuring no echo chambers and allowing for diverse authors and ideas to make their spotlight in any genre is important. In this case, less may be truly more so as to connect readers to fulfilling and exciting stories in the genres they love.")
      )
    )
  ),
)

# Define Server
server <- function(input, output, session) {
  # Reactive value to control when the table is shown
  tableAvailable <- reactiveVal(FALSE)
  
  # Reactive value to store the table data
  displayed_table <- reactiveVal(NULL)
  
  # Generate random recommendations
  observeEvent(input$random, {
    tableAvailable(TRUE)
    random_books <- books_data %>%
      sample_n(10) %>%
      arrange(desc(likedPercent)) %>%
      mutate(
        title = tools::toTitleCase(title),
        description = sapply(description, truncate_text),
        setting = sapply(setting, truncate_text)
      )
    displayed_table(random_books)
  })
  
  # Generate customized recommendations
  observeEvent(input$generate, {
    req(input$customize > 0)  # Ensure 'Customize Recommendations' is active
    
    filtered_books <- books_data %>%
      filter(
        (is.null(input$genre) | sapply(1:nrow(books_data), function(i) {
          any(grepl(paste(input$genre, collapse = "|"), books_data$genres[i]))
        }))
      ) %>%
      filter(
        input$series_type == "Any" |
          (input$series_type == "Series" & grepl("#", series)) |
          (input$series_type == "Standalone" & !grepl("#", series))
      ) %>%
      filter(rating >= input$rating) %>%
      filter(pages >= input$pages[1] & pages <= input$pages[2]) %>%
      filter(year >= as.numeric(input$publish_date[1]) & year <= as.numeric(input$publish_date[2])) %>%
      filter(
        (input$awards == "Any") |
          (input$awards == "No awards" & awards == "No awards to show") |
          (input$awards == "1 award" & grepl(",", awards) == FALSE & awards != "No awards to show") |
          (input$awards == "2+ awards" & grepl(",", awards))
      ) %>%
      filter(if (nzchar(input$keyword)) grepl(input$keyword, Top_Terms, ignore.case = TRUE) else TRUE) %>%
      arrange(desc(likedPercent)) %>%
      head(10) %>%
      mutate(
        title = tools::toTitleCase(title),
        description = sapply(description, truncate_text),
        setting = sapply(setting, truncate_text)
      )
    
    if (nrow(filtered_books) == 0) {
      displayed_table(data.frame(
        Title = "No recommendations can be generated with your input.",
        Description = NA,
        Setting = NA
      ))
    } else {
      tableAvailable(TRUE)
      displayed_table(filtered_books)
    }
  })
  
  # Render table with "Show More" functionality
  output$book_table_ui <- renderUI({
    req(tableAvailable())  # Ensure the table is available
    
    table_data <- displayed_table()
    if (is.null(table_data) || nrow(table_data) == 1) {
      return(div(class = "empty-message", "No recommendations available. Please adjust your filters."))
    }
    
    tags$table(
      class = "table table-striped",
      tags$thead(
        tags$tr(
          tags$th(style = "width: 20%;", "Title"),
          tags$th(style = "width: 40%;", "Description"),
          tags$th(style = "width: 40%;", "Setting")
        )
      ),
      tags$tbody(
        lapply(1:nrow(table_data), function(i) {
          tags$tr(
            tags$td(table_data$title[i]),
            tags$td(HTML(table_data$description[i])),
            tags$td(HTML(table_data$setting[i]))
          )
        })
      )
    )
  })
  
  output$data_dictionary_table <- DT::renderDataTable({
    datatable(
      data_dictionary,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        "Data Dictionary: Variables and Descriptions"
      )
    )
  })
  # 1. Distribution of Ratings
  output$rating_distribution <- renderPlot({
    ggplot(books_data, aes(x = rating)) +
      geom_histogram(binwidth = 0.1, fill = "#114847", color = "white") +
      labs(
        title = "Distribution of Ratings",
        x = "Rating",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)
      )
  })
  
  # 2. Top 10 Most Popular Genres
  # 1. Distribution of Ratings
  output$rating_distribution <- renderPlot({
    ggplot(books_data, aes(x = rating)) +
      geom_histogram(binwidth = 0.1, fill = "#114847", color = "white", alpha = 0.8) +
      labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency") +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = "transparent", color = NA))
  }, height = 500, width = 700)
  
  # 2. Top 10 Most Popular Genres
  output$popular_genres <- renderPlot({
    genres_count <- books_data %>%
      separate_rows(genres, sep = ",") %>%
      group_by(genres) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      slice(1:10)
    
    ggplot(genres_count, aes(x = reorder(genres, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "#900C0F", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 10 Most Popular Genres", x = "Genres", y = "Number of Books") +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = "transparent", color = NA))
  }, height = 500, width = 700)
  
  # 3. Pages vs. Ratings
  output$pages_vs_ratings <- renderPlot({
    ggplot(books_data, aes(x = pages, y = rating)) +
      geom_point(color = "#AA594E", alpha = 0.6) +
      labs(title = "Pages vs. Ratings", x = "Number of Pages", y = "Rating") +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = "transparent", color = NA))
  }, height = 500, width = 700)
  
  # 4. Trends in Book Publication Over the Years
  output$publish_year_trends <- renderPlot({
    year_trends <- books_data %>%
      group_by(year) %>%
      summarise(Count = n())
    
    ggplot(year_trends, aes(x = year, y = Count)) +
      geom_line(color = "#114847", size = 1) +
      labs(title = "Trends in Book Publication Over the Years", x = "Year of Publication", y = "Number of Books") +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = "transparent", color = NA))
  }, height = 500, width = 700)
  
  # 5. Books by Awards
  output$awards_distribution <- renderPlot({
    awards_count <- books_data %>%
      mutate(AwardsCategory = case_when(
        awards == "No awards to show" ~ "No Awards",
        grepl(",", awards) ~ "2+ Awards",
        TRUE ~ "1 Award"
      )) %>%
      group_by(AwardsCategory) %>%
      summarise(Count = n())
    
    ggplot(awards_count, aes(x = "", y = Count, fill = AwardsCategory)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(
        title = "Books by Awards",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = c("#114847", "#900C0F", "#AA594E"))
  }, height = 500, width = 700)
  
  
}

# Run the App
shinyApp(ui = ui, server = server)

