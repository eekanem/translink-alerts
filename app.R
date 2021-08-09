library(shiny)
library(tidyverse)
library(RSQLite)
library(plotly)
library(shinythemes)

source("detour_func.R")

conn = dbConnect(SQLite(), "translink_tweets.db")
clean_timeline_df = tbl(conn, "alerts")%>%collect()

dbDisconnect(conn)

convert_encoding <- function(i){
    if(!validUTF8(clean_timeline_df$text[i])){
        return(iconv(enc2utf8(clean_timeline_df$text[i]),sub="byte"))
    }
    else
        return(clean_timeline_df$text[i])
}

clean_timeline_df$text = sapply(1:nrow(clean_timeline_df), convert_encoding)

    
ui <- fluidPage(theme = shinytheme("simplex"),
    titlePanel("TransLink Twitter Bus Alerts"),
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            
        
        dateInput("dates", 
                  label = "Select date",
                  min = min(as.Date(clean_timeline_df$created)),
                  max = max(as.Date(clean_timeline_df$created)),
                  value = max(as.Date(clean_timeline_df$created))
        )),
    
    mainPanel(tabsetPanel(type = "pills",
                          tabPanel("Tweets",dataTableOutput("show_tweets")),
                          tabPanel("Buses Affected", dataTableOutput("data")),
                          tabPanel("Tweet Volume", plotlyOutput("time_series",  width = 800, height=600))
              )
    )
))

server <-function(input, output, session){
    
    output$data <-renderDataTable({
        alerts = translink(input$dates, clean_timeline_df)
        to_df(alerts)
    })
    
    output$show_tweets <- renderDataTable({
        s = subset(clean_timeline_df, as.Date(clean_timeline_df$created)==input$dates)[,-1]
        s$created = format(as.POSIXct(s$created), format = "%H:%M:%S", tz="America/Los_Angeles", usetz = TRUE)
        colnames(s) = c("time", "tweet")
        s
    })
    
    finalInput<-reactive({
        summary = clean_timeline_df %>% group_by(created = as.Date(created)) %>% summarise(tweets=n())
    })
    
    output$time_series <- renderPlotly({
        ggplotly(finalInput()%>%ggplot(aes(x=created, y=tweets)) + geom_point()+
                     geom_line() + theme(
                         legend.title = element_blank(),
                         legend.position = "bottom",
                         plot.title = element_text(face = "bold")) + labs(
                             x = NULL, y = NULL,
                             title = "Frequency of TransLink Rider Alerts",
                             subtitle = "Tweet counts aggregated by day from June - August 2021",
                             caption = "\nSource: Data collected from Twitter's API"))})
}

# Run the application 
shinyApp(ui = ui, server = server)

