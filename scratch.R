# Scratch file for working with the files in data. I load (also known as
# "attach") the three libraries that are most useful, I think, for this sort of
# work. One of my roles is to point out which packages are useful. Including the
# package name (or "tidyverse" or "dplyr") when googling will often lead to
# better results than a straight google.

library(tidyverse)
library(stringr)
library(fs)
library(dplyr)
library(stringr)

# 1. Read data/ex_926_I.csv into a tibble and provide a summary.
x_926 <- read_csv("data/ex_926_I.csv")
summary(x_926)

# 2. Create a vector with all the file names in data/.
files_names <- dir_ls(path = "data/")

# 3. Create a vector with just the file names that have an "A" in them.
files_names_A <- str_subset(files_names, pattern = "A")

# 4. Read in all the files into one big tibble. Check out ?map_dfr . . .
# Background reading here:
# https://r4ds.had.co.nz/iteration.html#the-map-functions
x <- map_dfr(files_names, read_csv)

# 5. Read in everything and also add a new variable, source, which records the
# file name from which the data came.
x <- map_dfr(files_names, read_csv, .id = "source")

# 6. Find the 4 files with the largest number of observations.
x %>% 
  count(source, sort = TRUE) %>% 
  slice(1:4)

# 7. Write a function which takes a character string like "A" and then reads in
# all the files which have "A" in the name.
file_list <- function(x){
  str_subset(files_names, pattern = x)
}

# 8. Create a Shiny App which displays the histogram of b, allowing the user to
# subset the display for specific values of c.
library(shiny)
ui <- fluidPage(
  
  # Application title
  titlePanel("Histogram of b"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("c",
                  "Select Input:",
                  min = -5,
                  max = 5,
                  value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- files_names[,2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)