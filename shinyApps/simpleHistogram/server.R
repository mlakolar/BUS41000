library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, clientData, session) {
    
    # read the data from the csv file
    df = reactive( read.csv( input$dfName ) )
    
    colNames = reactive( colnames(df()) )
    numColumns = reactive( length(colNames()) )
    numUnique = reactive( sapply(1:numColumns(), function(x) length(unique(df()[,x]))) )
    
    # Change values for input$colName
    observe({
        colNamesOptions = list()
        for (i in 1:numColumns()) {
            if  (numUnique()[i] > 25) {
                colNamesOptions[[ colNames()[i] ]] = as.character(i)
            }
        }
        updateSelectInput(session, "colName",
                          choices = colNamesOptions)
    })
        
    # Change values for input$bins
    observe({
        updateSliderInput(session, "bins",
                          min = 1,
                          max = min(200, numUnique()[ as.numeric(input$colName) ]),
                          value = 20)
    })
    
    
    # generate histogram
    # generate bins based on input$bins from ui.R
    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        indCol = as.numeric(input$colName)
        x = df()[,indCol]
        colName = colnames(df())[indCol]
        bins = seq(min(x), max(x), length.out = as.numeric(input$bins) + 1)
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white',
             main = "",
             xlab = colName
             )
        
        if (input$individual_obs) {
            rug(x)
        }
    })
})
