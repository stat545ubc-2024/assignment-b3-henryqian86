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
  theme = shinytheme("flatly"),
  includeCSS("www/styles.css"),
  titlePanel("Book Explorer App"),
  img(src = "books.jpg", alt = "Banner", style = "margin-bottom: 15px;"),
  
  
  sidebarLayout(
    sidebarPanel(
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
      
      # Feature 3: Colour picker for Ratings Distribution plot
      # Purpose: Lets users customize the color of the bars in the Ratings Distribution plot for a personalized experience.
      
      colourpicker::colourInput("bar_color", "Select Bar Color for Ratings Plot", 
                  value = "skyblue"),  # Default color
      
      # Feature 4: Display Filtered Count
      # Purpose: Shows the number of books that match the filters applied by the user.
      textOutput("result_count"),  # Display the number of results
      
      # Feature 5: Download Button
      # Purpose: Allows users to download the filtered data as a CSV file.
      downloadButton("download_data", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Book Table", DTOutput("book_table")),
        tabPanel("Ratings Distribution", plotOutput("rating_plot")),
        tabPanel("Reviews vs. Ratings", plotOutput("scatter_plot"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Reactive Filtered Data
  filtered_data <- reactive({
    df <- books
    
    # Filter by multiple Genres
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      df <- subset(df, Genre %in% input$genre_filter)
    }
    
    # Filter by multiple Authors
    if (!is.null(input$author_filter) && length(input$author_filter) > 0) {
      df <- subset(df, Author %in% input$author_filter)
    }
    
    # Filter by Year
    df <- subset(df, Year >= input$year_filter[1] & Year <= input$year_filter[2])
    
    return(df)
  })
  
  # Render Book Table
  output$book_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 5,  
        lengthMenu = c(5, 10, 15, 20)  
      )
    )
  })
  
  # Render Rating Distribution Plot with custom bar color
  output$rating_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Rating)) +
      geom_histogram(binwidth = 0.1, fill = input$bar_color, color = "black") +  # Dynamic color
      theme_minimal() +
      labs(title = "Distribution of Book Ratings", x = "Rating", y = "Count")
  })
  
  # Render Reviews vs Ratings Scatterplot
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Rating, y = Reviews)) +
      geom_point(color = "blue", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Reviews vs. Ratings", x = "Rating", y = "Number of Reviews")
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_books.csv")  # Name of the downloaded file
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)  # Write the filtered data to a CSV file
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)
