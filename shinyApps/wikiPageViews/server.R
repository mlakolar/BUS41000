 # libraries used. install as necessary

  library(shiny)
  library(RJSONIO) # acquiring and parsing data
  library(ggplot2) # graphs
  library(plyr)  # manipulating data
  library(lubridate) #dates
  library(stringr)

trim.leading <- function (x)  sub("^\\s+", "", x)

shinyServer(function(input, output) {
  
  
  data <- reactive({  
   
      # create a set of months to be analyzed
      dates <-seq(Sys.time()-months(input$obs[2]),Sys.time()-months(input$obs[1]), by = "month")
      
      # create blank dataframe to hold three fields
      allData <- data.frame(count=numeric(),date=character(),name=character())
      # seperate each variable of the subject vector
      
      subject <- str_split(input$subjects, ",")[[1]]
      
      # loop through subjects and months
      for(k in 1:length(subject)) {
          # handle remote problems related to strings
          target <- trim.leading(subject[k])
          target <- str_replace(target," ","_")
          
          # create dataframe for individual records
          df <- data.frame(count=numeric()) 
          
          for (i in 1:length(dates)) {
              yr <- year(dates[i])
              
              mth <- month(dates[i]) 
              if (str_length(mth)==1) {
                  mth<-paste0("0",as.character(mth))
              }
              
              # obtain and process daily count data by month by target
              url <- paste0("http://stats.grok.se/json/en/",yr,mth,"/",target)
              raw.data <- readLines(url, warn="F") 
              rd  <- fromJSON(raw.data)
              rd.views <- rd$daily_views 
              
              df <- rbind(df,as.data.frame(rd.views))
          }
          
          
          #create the dataframe with all targets search counts by day
          df$date <-  as.Date(rownames(df))
          df$name <- subject[k]
          colnames(df) <- c("count","date","name")
          df <- arrange(df,date)
          allData <- rbind(allData,df)
      }
      
      return(allData)
  })
  
  
  # Create a heading based on range of dates selected for printing as a caption
  output$caption <- renderText({
      endDate <- Sys.time()-months(input$obs[1])
      startDate <- Sys.time()-months(input$obs[2])
      if (input$obs[2]==0){
          paste("Daily rates for",month(Sys.time(), label = TRUE, abbr = TRUE),year(Sys.time()),sep=" ")
      } else if ((input$obs[2]!=0)&(year(endDate)==year(startDate))) {
          paste("Daily rates from",month(startDate, label = TRUE, abbr = TRUE),"to",month(endDate, label = TRUE, abbr = TRUE), year(endDate),sep=" ")   
      } else  {
          paste("Daily rates from",month(startDate, label = TRUE, abbr = TRUE),year(startDate),"to",month(endDate, label = TRUE, abbr = TRUE), year(endDate),sep=" ")   
      }
  })
  
  
  # create plot for linear and log scales
  output$plot <- renderPlot({
      if (input$log) {
          print(ggplot(data(), aes(x=date,y=log10(count),group=name,colour=name))+
                    geom_line()+ylab("log10")+xlab("")+theme_bw() + 
                    theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(colour="blue", size = 14, face = "bold")))
      } else {
          print(ggplot(data(), aes(x=date,y=count,group=name,colour=name))+
                    geom_line()+ylab("")+xlab("") +theme_bw() + 
                    theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(colour="blue", size = 14, face = "bold")))
      }
  })
  
  # create summary data for each subject 
  output$view <- renderTable({
      myTable <- data()
      myTable$count <- as.integer(myTable$count)
      
      myTable$date <- as.character(myTable$date) 
      mySummary <- ddply(subset(myTable,count>0),.(name), summarize, mean=mean(count),median=median(count),min=min(count),max=max(count),maxdate=date[which.max(count)])
      mySummary$showMax <- paste0(day(mySummary$maxdate)," ",month(mySummary$maxdate, label = TRUE, abbr = TRUE),", ",year(mySummary$maxdate))
      mySummary$maxdate <- NULL
      mySummary <- arrange(mySummary,desc(mean))
      colnames(mySummary) <- c("","Mean","Median","Min","Max","Max Date")
      mySummary                 
  })
  
  # make data downloadable
  output$downloadData <- downloadHandler(
      filename ='results.csv',
      content = function(file) {
          write.csv(data(), file)
      }
  )
  
})