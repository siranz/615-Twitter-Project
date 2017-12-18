#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rsconnect::setAccountInfo(name='siranz', token='204BC383C4D26755D05635434AE141E3', secret='NNwB7xTxVWg0RY7N/NkctWcpcMMwmRdJFw0yEs8q')

library(dplyr)
library(tidyr)
library(tidyverse)
library(plotly)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(plyr)
library(grid)
library(gridExtra)
library(devtools)
library(twitteR)
library(streamR)
library(leaflet)
library(XML)
# Load Data

iph <- readRDS("Iphonex.rds")
iph <- separate(iph,created,c("Date","Time")," ")
iphx.geo <-dplyr::filter(iph,!is.na(iph$longitude) & !is.na(iph$latitude))
iphx.geo$longitude<- as.numeric(iphx.geo$longitude)
iphx.geo$latitude <- as.numeric(iphx.geo$latitude)
iph8 <- readRDS("Iphone8.rds")
iph8.geo <- dplyr::filter(iph8, !is.na(iph8$longitude) & !is.na(iph8$latitude))
iph8.geo <- tidyr::separate(iph8.geo,"created",c("Date","Time")," ")
iph8.geo$longitude<- as.numeric(iph8.geo$longitude)
iph8.geo$latitude <- as.numeric(iph8.geo$latitude)
iph8x <- readRDS("iphone8_vs_iphonex.rds")
iph8 <- separate(iph8,created,c("Date","Time")," ")
iph8x <- separate(iph8x,created,c("Date","Time")," ")

#Sentiment analysis

#iphone x
result.iphx.1 <- readRDS("iphx_result1.rds")
plot1.1 <- ggplot(result.iphx.1,aes(result.iphx.1$score))+
  geom_bar()+
  labs(x = "Sentiment Score")
result.iphx.2 <- filter(result.iphx.1,score != "0")
plot1.2 <- ggplot(result.iphx.2,aes(result.iphx.2$score))+
  geom_bar()+
  labs(x = "Sentiment Score")
grid.arrange(plot1.1,plot1.2,ncol = 2)

#iphone 8
result.iph8.1 <- readRDS("iph8_result1.rds")
plot2.1 <- ggplot(result.iph8.1,aes(result.iph8.1$score))+
  geom_bar()+
  labs(x = "Sentiment Score")
result.iph8.2 <- filter(result.iph8.1,score != "0")
plot2.2 <- ggplot(result.iph8.2,aes(result.iph8.2$score))+
  geom_bar()+
  labs(x = "Sentiment Score")
grid.arrange(plot2.1,plot2.2,ncol = 2)

#Comparision Dataset
result.iphx.1$pos <- 
  ifelse(result.iphx.1$score < 0,"Negative",
         ifelse(result.iphx.1$score == 0,"Neutral","Positive"))
result.iphx.1$belong <- rep("iphonex",170000)

result.iph8.1$pos <- 
  ifelse(result.iph8.1$score < 0,"Negative",
         ifelse(result.iph8.1$score == 0,"Neutral","Positive"))
result.iph8.1$belong <- rep("iphone8",98608)

result.compare <- rbind(result.iphx.1,result.iph8.1)
result.compare <- filter(result.compare,pos != "Neutral")

ggplot(result.compare,aes(x= belong,fill=pos))+geom_bar(position = "fill")

#Word cloud
BrandSelection <- c("Iphone X's Twitter","Iphone 8's Twitter", "Iphone X & Iphone 8")

iph$belong <- rep("iphx",10000)
iph8$belong <- rep("iph8",6163)
iph8x$belong <- rep("iph8x",1700)
iphall <- rbind(iph,iph8,iph8x)

iph_x <- filter(iphall, belong == "iphx")
iph_8 <- filter(iphall, belong == "iph8")
iph_8x <- filter(iphall, belong == "iph8x")

#iphx wordcloud


#UI Design
ui <- fluidPage(
  tabsetPanel(
  
  tabPanel("Introduction",
           shiny::h1("Introduction"),
           p("This shiny app is used to present the results from the analysis of Twitter data about Apple's new products.
             In the shiny app, I drew two word clouds for Iphone X and Iphone 8, and I made a sentimental analysis for these two 
             products to see wether there are some difference between people's attitude towards these two products. At last, I map the
              location of these comments on twitter.")
           ),
  
  tabPanel("Iphone X",
  shiny::h1("Iphone X"), 
  
  mainPanel(
    # Iphone X Map
    leafletOutput("iphx", height = "450px")
    
  ), 
  
  
  sidebarPanel(
   
    # Table of Comparision
    plotOutput("iphx.1")
  )
  
  
  
  
),
tabPanel("Iphone 8",
         shiny::h1("Iphone 8"),
        mainPanel(
         leafletOutput("iph8",height = "450px")
         ),
         sidebarPanel(
           
           # Table of Comparision
           plotOutput("iph8.1")
         )
         
  
),

tabPanel("Comparision",
         shiny::h1("Comparision Bar Plot"),
         mainPanel(
           plotOutput("result")
         )
  
),

tabPanel("Word Cloud",
         shiny::h1("Word Clouds"),
         sidebarPanel(
           selectInput(
             inputId = "brands",
             label = "Brands",
             choices = BrandSelection,
             selected = 1)
         ),
         mainPanel(
           plotOutput(
             outputId = "wordclouds"
           )
         )
         
  
)

))

# Define server logic required
server <- function(input, output) {
  
  
  
  # Leaflet Map
  output$iphx <- renderLeaflet({
    
   
    leaflet(data = iphx.geo)%>%
      addTiles()%>%
      addCircleMarkers(~iphx.geo$longitude,~iphx.geo$latitude)
    
    
  })
  
  output$iph8 <- renderLeaflet({
    
    
    leaflet(data = iph8.geo)%>%
      addTiles()%>%
      addCircleMarkers(~iph8.geo$longitude,~iph8.geo$latitude)
    
    
  })
  
  #Brand selected
  
  brandselected <- reactive({
    ifelse(input$brands == "Iphone X's Twitter","iphx",
           ifelse(input$brands == "Iphone 8's Twitter","iph8","iph8x"))
    #"Iphone X's Twitter","Iphone 8's Twitter", "Iphone X & Iphone 8"
  })
  
  #wordplot selection
  
  wordplot <- reactive({
    iphall %>%
    filter(belong == brandselected())
  })
  
  # plots
  output$iphx.1 = renderPlot({
    
    grid.arrange(plot1.1,plot1.2,ncol = 1)
    
  })
  
  output$iph8.1 = renderPlot({
    
    grid.arrange(plot2.1,plot2.2,ncol = 1)
    
  })
  
  output$result = renderPlot({
    
    ggplot(result.compare,aes(x= belong,fill=pos))+geom_bar(position = "fill")
    
  })
  
  output$wordclouds = renderPlot({
    
  })
  
  output$wordclouds = renderPlot({
    words <- wordplot()
    iph_corpus <- Corpus(VectorSource(str_replace_all(words$text, "@", "")))
    iph_corpus <- tm_map(iph_corpus,removePunctuation)
    iph_corpus <- tm_map(iph_corpus, content_transformer(tolower))
    iph_corpus <- tm_map(iph_corpus, removeWords, stopwords("english"))
    iph_corpus <- tm_map(iph_corpus,removeWords,c("apple","iphonex"))
    iph_corpus <- tm_map(iph_corpus,removeWords,c("will","now","happening"))
    iph_corpus <- tm_map(iph_corpus,removeWords,c("iphone8","iphone7","iphone8andx","iphone","iphone8plus","iphone8officiai"))
    iph_corpus <- tm_map(iph_corpus,removeWords,c("giveaway","ever","holiday","1000","available","message","reverskinegh","smartphone","ampamp","day","1000","gssrinivasrao"))
    iph_corpus <- tm_map(iph_corpus, stripWhitespace)
    set.seed(1000)
    wordcloud(iph_corpus,max.words = 100,random.order = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

