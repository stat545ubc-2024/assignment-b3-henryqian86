#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(ggplot2)
library(colourpicker) 
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(dplyr)

# Dataset
books <- data.frame(
  Title = c("The Great Gatsby", "To Kill a Mockingbird", "1984", 
            "Pride and Prejudice", "Moby Dick", "The Catcher in the Rye",
            "The Hobbit", "Harry Potter and the Sorcerer's Stone", "The Lord of the Rings", 
            "The Chronicles of Narnia", "War and Peace", "Anna Karenina",
            "The Alchemist", "The Kite Runner", "Jane Eyre",
            "Wuthering Heights", "The Picture of Dorian Gray", "Brave New World",
            "Frankenstein", "Dracula"),
  Author = c("F. Scott Fitzgerald", "Harper Lee", "George Orwell", 
             "Jane Austen", "Herman Melville", "J.D. Salinger",
             "J.R.R. Tolkien", "J.K. Rowling", "J.R.R. Tolkien", 
             "C.S. Lewis", "Leo Tolstoy", "Leo Tolstoy",
             "Paulo Coelho", "Khaled Hosseini", "Charlotte Bronte",
             "Emily Bronte", "Oscar Wilde", "Aldous Huxley",
             "Mary Shelley", "Bram Stoker"),
  Genre = c("Classic", "Classic", "Dystopian", 
            "Romance", "Adventure", "Classic",
            "Fantasy", "Fantasy", "Fantasy", 
            "Fantasy", "Classic", "Classic",
            "Philosophical", "Drama", "Classic",
            "Classic", "Philosophical", "Dystopian",
            "Horror", "Horror"),
  Year = c(1925, 1960, 1949, 1813, 1851, 1951, 1937, 1997, 1954, 
           1950, 1869, 1877, 1988, 2003, 1847, 1847, 1890, 1932, 1818, 1897),
  Rating = c(4.4, 4.8, 4.7, 4.5, 4.2, 4.3, 4.8, 4.9, 4.9, 4.7, 4.3, 4.4, 
             4.6, 4.7, 4.6, 4.4, 4.5, 4.4, 4.3, 4.2),
  Reviews = c(45000, 35000, 50000, 22000, 18000, 27000, 48000, 80000, 
              75000, 32000, 15000, 18000, 23000, 45000, 25000, 28000, 
              20000, 34000, 21000, 19000)
)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),  # Default theme
  includeCSS("www/styles.css"),  # Custom CSS
  
  # Google Fonts for better typography
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&family=Merriweather:wght@300;700&display=swap",
      rel = "stylesheet"
    )
  ),
  
  # App Title and Banner
  titlePanel(
    h1("Book Explorer App", style = "font-family: 'Merriweather', serif; font-weight: bold; color: #2c3e50; text-align: center;")
  ),
  img(
    src = "books.jpg",
    alt = "Banner",
    style = "margin: 15px auto; display: block; width: 60%; border-radius: 10px;"  # Enhanced banner styling
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters", style = "font-family: 'Roboto', sans-serif; color: #2c3e50; text-align: center;"),
      
      # Feature 7: Collapsible Panels for Filters and Customizations
      # Purpose: Organize inputs into logical sections for a cleaner UI
      bsCollapse(
        bsCollapsePanel(
          "Filters",  # Collapsible Panel for Filters
          # Feature 1: Checkbox filters for Genre and Author
          # Purpose: Users can select multiple genres and authors to narrow down the books displayed in the table and plots.
          checkboxGroupInput("genre_filter", "Filter by Genre", 
                             choices = unique(books$Genre), 
                             selected = unique(books$Genre)),  # Default: Select all genres
          
          checkboxGroupInput("author_filter", "Filter by Author", 
                             choices = unique(books$Author), 
                             selected = unique(books$Author)),
          
          # Feature 2: Year slider for filtering books
          # Purpose: Allows users to select a range of publication years, helping them focus on books from a specific time period.
          sliderInput("year_filter", "Select Publication Year Range:",
                      min = min(books$Year), max = max(books$Year),
                      value = c(min(books$Year), max(books$Year))),
          style = "primary"
        ),
        bsCollapsePanel(
          "Customizations",  # Collapsible Panel for Customizations
          # Facet selection for graphs (Genre or Author)
          # Purpose: Dynamically facet the plots based on user selection of Genre or Author.
          selectInput("facet_by", "Facet Graphs By:", 
                      choices = c("Genre", "Author"), 
                      selected = "Genre"),
          
          # Feature 3: Colour picker for Ratings Distribution plot
          # Purpose: Lets users customize the color of the bars in the Ratings Distribution plot for a personalized experience.
          colourpicker::colourInput("bar_color", "Select Bar Color for Ratings Plot", 
                                    value = "skyblue"),
          style = "info"
        )
      ),
      
      # Feature 4: Display Filtered Count
      # Purpose: Shows the number of books that match the filters applied by the user.
      h4(textOutput("result_count"), style = "color: #27ae60; text-align: center; margin-top: 15px;"),
      
      # Feature 5: Download Button
      # Purpose: Allows users to download the filtered data as a CSV file.
      downloadButton("download_data", "Download Filtered Data", style = "width: 100%; margin-top: 10px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Book Table", 
          DTOutput("book_table"),
          style = "padding: 10px;"
        ),
        tabPanel(
          "Ratings Distribution", 
          plotOutput("rating_plot", height = "400px"),  # Set plot height
          style = "padding: 10px;"
        ),
        tabPanel(
          "Reviews vs. Ratings", 
          plotOutput("scatter_plot", height = "400px"),  # Set plot height
          style = "padding: 10px;"
        ),
        tabPanel(
          "About",  # About Tab
          # Feature 6: Enhanced "About" Section
          # Purpose: Provide clear instructions on how to use the app and explain its functionality.
          h4("About the Book Explorer App", style = "font-family: 'Merriweather', serif; text-align: center;"),
          p("This app allows you to explore a dataset of popular books by filtering them based on genre, author, and publication year.",
            style = "font-family: 'Roboto', sans-serif; text-align: justify;"),
          h4("How to Use This App", style = "font-family: 'Merriweather', serif; text-align: center;"),
          p("- Use the filters in the sidebar to narrow down your selection of books.", 
            style = "font-family: 'Roboto', sans-serif; text-align: justify;"),
          p("- View the filtered results in the 'Book Table' tab.", 
            style = "font-family: 'Roboto', sans-serif; text-align: justify;"),
          p("- Visualize the distribution of ratings in the 'Ratings Distribution' tab.", 
            style = "font-family: 'Roboto', sans-serif; text-align: justify;"),
          p("- Explore the relationship between reviews and ratings in the 'Reviews vs. Ratings' tab.",
            style = "font-family: 'Roboto', sans-serif; text-align: justify;"),
          p("- Download the filtered dataset using the 'Download Filtered Data' button.",
            style = "font-family: 'Roboto', sans-serif; text-align: justify;")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Reactive Filtered Data
  filtered_data <- reactive({
    req(input$genre_filter, input$author_filter, input$year_filter)  # Ensure inputs exist
    
    # Feature 8: Filter Data Using tidyverse (Modern Approach)
    # Purpose: Dynamically filter the dataset based on user inputs.
    books %>%
      filter(
        Genre %in% input$genre_filter,
        Author %in% input$author_filter,
        Year >= input$year_filter[1] & Year <= input$year_filter[2]
      )
  })
  
  # Render Book Table
  output$book_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        autoWidth = TRUE
      ),
      class = "table table-striped table-hover"
    )
  })
  
  # Render Rating Distribution Plot
  output$rating_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Rating, weight = Reviews)) +
      geom_bar(fill = input$bar_color, color = "black", alpha = 0.8) +
      facet_wrap(as.formula(paste("~", input$facet_by))) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Ratings Distribution by", input$facet_by),
        x = "Rating (Weighted by Reviews)",
        y = "Weighted Count"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  # Render Reviews vs Ratings Scatterplot
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Rating, y = Reviews, size = Reviews)) +
      geom_point(color = "#3498db", alpha = 0.7) +
      facet_wrap(as.formula(paste("~", input$facet_by))) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Reviews vs. Ratings by", input$facet_by),
        x = "Rating",
        y = "Number of Reviews",
        size = "Number of Reviews"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  # Display number of results
  output$result_count <- renderText({
    n <- nrow(filtered_data())
    if (n == 1) {
      paste("We found", n, "book for you.")
    } else {
      paste("We found", n, "books for you.")
    }
  })
  
  # Download Filtered Data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_books.csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)
