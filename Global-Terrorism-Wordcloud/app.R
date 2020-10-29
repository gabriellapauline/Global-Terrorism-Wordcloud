#This code is adapted from https://github.com/AntoineSoetewey/word-cloud/blob/master/app.R

library(readxl)
library(shiny)
# install.packages("wordcloud2")
library(wordcloud2)
# install.packages("tm")
library(tm)
# install.packages("colourpicker")
library(colourpicker)

ui <- fluidPage(
    h1("Word Cloud"),
    # Create a container for tab panels
    tabsetPanel(
        # Create a "Word cloud" tab
        tabPanel(
            title = "Word cloud",
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        inputId = "source",
                        label = "Word source",
                        choices = c(
                            "Summary" = "summary",
                            "Motives" = "motives"
                        )
                    ),
                    hr(),
                    selectInput(inputId="country","Country",country,selected="All"),
                    hr(),
                    sliderInput(inputId="year","Year",min=1998,max=2018,value=2018,sep="",animate=FALSE),
                    hr(),
                    checkboxInput("remove_words", "Remove specific words?", FALSE),
                    conditionalPanel(
                        condition = "input.remove_words == 1",
                        textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
                        textAreaInput("words_to_remove2", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
                        textAreaInput("words_to_remove3", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
                        textAreaInput("words_to_remove4", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
                        textAreaInput("words_to_remove5", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
                        textAreaInput("words_to_remove6", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
                        textAreaInput("words_to_remove7", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
                        textAreaInput("words_to_remove8", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
                        textAreaInput("words_to_remove9", "", rows = 1)
                    ),
                    conditionalPanel(
                        condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
                        textAreaInput("words_to_remove10", "", rows = 1)
                    ),
                    hr(),
                    numericInput("num", "Maximum number of words",
                                 value = 100, min = 5
                    ),
                    hr(),
                    colourInput("col", "Background color", value = "white")
                ),
                mainPanel(
                    textOutput("alt"),
                    wordcloud2Output("cloud"),
                    br(),
                    br()
                )
            )
        )
    )
)

server <- function(input, output) {
    #Read data from Excel
    gtd <- read_xlsx("globalterrorismdb_cleaned_1998onward_copy1.xlsx")
    
    #Get list of countries
    country <- unique(gtd$country_txt)
    country[length(country)+1] <- "All"
    country <- sort(country)
    str(country)
    
    #Get data depending on input (country, year, motive/summary)
    data_source <- reactive({
        if(input$country=="All"){
            subdata <- subset(gtd,gtd$iyear==input$year)
        }else{
            subdata <- subset(gtd,gtd$iyear==input$year&gtd$country_txt==input$country)
        }
        
        if (input$source == "summary") {
            data <- paste(subdata$summary[subdata$summary!=""],collapse=" ")
        }
        else if (input$source == "motives") {
            data <- paste(subdata$motive[subdata$motive!=""],collapse=" ")
        }
        
        return(data)
    })
    
    create_wordcloud <- function(data, num_words = 100, background = "white") {
        
        # If text is provided, convert it to a dataframe of word frequencies
        if (is.character(data)) {
            corpus <- Corpus(VectorSource(data))
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            corpus <- tm_map(corpus, removeWords, stopwords(tolower("English")))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
            tdm <- as.matrix(TermDocumentMatrix(corpus))
            data <- sort(rowSums(tdm), decreasing = TRUE)
            data <- data.frame(word = names(data), freq = as.numeric(data))
        }
        
        # Make sure a proper num_words is provided
        if (!is.numeric(num_words) || num_words < 3) {
            num_words <- 3
        }
        
        # Grab the top n most common words
        data <- head(data, n = num_words)
        
        #Display 'no data available' if there is only 1 word or less in the word cloud
        if(length(data$freq)<2) {
            output$alt <- renderText("No data available")
        }else{
            output$alt <- renderText("")
        }
        if (nrow(data) == 0) {
            return(NULL)
        }
        
        wordcloud2(data, backgroundColor = background)
    }
    
    output$cloud <- renderWordcloud2({
        create_wordcloud(data_source(),
                         num_words = input$num,
                         background = input$col
        )
    })
}

shinyApp(ui = ui, server = server)