library(shiny)
library(ggplot2)
library(pdftools)
library(XML)
library(stringr)

shinyServer(function(input,output){

  #create availability column based on user input
  space_avail <- reactive({factor(camp_df$sitesAvail >= input$sites_needed,labels = c("Unavailable","Available"))})
  
  #create a data.frame that includes the new column then subset it based on the other user inputs
  camp <- reactive({cbind(camp_df,space_avail())})
  camp_sub <- reactive({subset(camp(),campground%in%(if(!is.null(input$campground)) {input$campground} else {campground}) 
                         & date >= input$date_range[1] 
                         & date <= input$date_range[2])})
  
  #make sure the colors are correct in the case that everything is either available or unavailable 
  color_chart <- reactive({
    if(length(unique(camp_sub()$space_avail)) == 2) {c("#F8766D","#00BFC4")}
    else if (unique(camp_sub()$space_avail) == "Available") {"#00BFC4"}
    else {"#F8766D"}
  })
  
  #update the plot size based on how many campsites are selected
  plot_size <- reactive({
    if(is.null(input$campground)){800} 
    else if (length(input$campground)==1) {200} 
    else if (length(input$campground)<4) {450} 
    else if (length(input$campground)<=5) {600}
    else {800}
  })

  #create the plot ouput
  output$plot1 <- renderPlot({
    gg <- ggplot(camp_sub(),aes(x=date,y=campground)) +
      geom_tile(aes(fill=camp_sub()$space_avail),colour = "white") +  
      #scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") + 
      scale_fill_manual(values=color_chart()) +
      labs(x='Date', y='Campground',fill='Available?')
    plot(gg)
  })
  
  #using renderUI allows the server to update plot size
  output$plot_ui <- renderUI({
    plotOutput("plot1", height = plot_size())
  })
            
  
})