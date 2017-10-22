pageWithSidebar(
  headerPanel('Banff National Park 90 Day Campsite Availability'),
  
  sidebarPanel(
    numericInput('sites_needed', 'Sites Needed', min=1,value=1),
    dateRangeInput('date_range', 'Dates', 
                   start = min(camp_df$date), end = max(camp_df$date),
                   min = min(camp_df$date), max = max(camp_df$date),
                   startview = 'month'),
    selectInput('campground', 'Campground', unique(camp_df$campground), selected=NULL,multiple = TRUE),
    h6(paste0("PDF last modified ", modified))
  ),
 
 
  mainPanel(
    uiOutput("plot_ui")
  )
)