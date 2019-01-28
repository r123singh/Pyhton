#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Author- Ramandeep Singh

source("global.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(windowTitle = "OneCommerce","One-Commerce : Online Seller Info"),
   
   tabsetPanel(tabPanel(title = "All-Sellers", fluidRow(column(dataTableOutput('sellerTable'),width = 10,icon("table"))
   )),tabPanel("Ratings",   # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("bins",
                                        "Number of bins:",
                                        min = 1,
                                        max = 50,
                                        value = 30),
                            sliderInput("n",
                                        "Select Top n Sellers:",
                                        min = 1,
                                        max = 10,
                                        value = 5),
                            actionButton("sync_sellers","Sync Sellers")
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            plotOutput("distPlot"),
                            plotOutput("topRatingsPlot")
                          )
                        )),tabPanel("GlobalCloud",verticalLayout(plotOutput("globalWc"),plotOutput("categoryPlot"))),
                                      tabPanel("1-Star",verticalLayout(plotOutput("star1RatedWc"),plotOutput("star1RatedWc_pos"))),
                                      tabPanel("2-Star",verticalLayout(plotOutput("star2RatedWc"),plotOutput("star2RatedWc_pos"))),
   tabPanel("3-Star",verticalLayout(plotOutput("star3RatedWc"),plotOutput("star3RatedWc_pos"))),
   tabPanel("4-Star",verticalLayout(plotOutput("star4RatedWc"),plotOutput("star4RatedWc_pos"))),
   tabPanel("5-Star",verticalLayout(plotOutput("star5RatedWc"),plotOutput("star5RatedWc_pos")))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$globalWc<-renderPlot({
     wordcloud(words = global_frame$word,freq = global_frame$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))
   })
   output$distPlot <- renderPlot({    
      Top_Sellers <- ebay_df$predicted_rating
      bins <- seq(min(Top_Sellers), max(Top_Sellers), length.out = input$bins + 1)
      hist(Top_Sellers, breaks = bins, col = 'darkgray', border = 'white')
   })
   output$topRatingsPlot<-renderPlot({
     ggplot(data=ebay_df[c(1:input$n),], aes(seller_name, predicted_rating)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(predicted_rating, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(x="Seller Name", y="Average Weighted Score", title="Top Rated Sellers By Online Seller Info")
   })
   output$categoryPlot<-renderPlot({cat_plot})
   
   output$sellerTable <- renderDataTable(ebay_df[,c("seller_code","seller_name","channel","category_cnt","predicted_rating","product_cnt")],
                                   options = list(
                                     pageLength = 10,
                                     initComplete = I("function(settings, json) {}")
                                   )
   )
   output$star1RatedWc<-renderPlot({
     wordcloud(words = oneRated_dframe$word,freq = oneRated_dframe$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "1-Star Rated Negative Feeback")
   })
   output$star2RatedWc<-renderPlot({
     wordcloud(words = twoRated_dframe$word,freq = twoRated_dframe$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "2-Star Rated Negative Feedback")
   })
   output$star3RatedWc<-renderPlot({
     wordcloud(words = threeRated_dframe$word,freq = threeRated_dframe$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "3-Star Rated Negative Feedback")
   })
   output$star4RatedWc<-renderPlot({
     wordcloud(words = fourRated_dframe$word,freq = fourRated_dframe$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "4-Star Rated Negative Feedback")
   })
   output$star5RatedWc<-renderPlot({
     wordcloud(words = fiveRated_dframe$word,freq = fiveRated_dframe$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "5-Star Rated Negative Feedback")
   })
   output$star1RatedWc_pos<-renderPlot({
     wordcloud(words = oneRated_dframe_pos$word,freq = oneRated_dframe_pos$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "1-Star Rated Positive Feedback")
   })
   output$star2RatedWc_pos<-renderPlot({
     wordcloud(words = twoRated_dframe_pos$word,freq = twoRated_dframe_pos$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "2-Star Rated Positive Feeback")
   })
   output$star3RatedWc_pos<-renderPlot({
     wordcloud(words = threeRated_dframe_pos$word,freq = threeRated_dframe_pos$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "3-Star Rated Positive Feedback")
   })
   output$star4RatedWc_pos<-renderPlot({
     wordcloud(words = fourRated_dframe_pos$word,freq = fourRated_dframe_pos$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "4-Star Rated Positive Feedback")
   })
   output$star5RatedWc_pos<-renderPlot({
     wordcloud(words = fiveRated_dframe_pos$word,freq = fiveRated_dframe_pos$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))+title(main = "5-Star Rated Positive Feedback")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
