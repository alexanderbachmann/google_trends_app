
# LIBRARIES ----

# Shiny
library(shiny)
library(bs4Dash)
library(gtrendsR)
library(bslib)
library(shinyjs)

# Tables
library(gt)
library(gtExtras)
library(DT)

# Core
library(tidyquant)
library(tidyverse)
library(tidygeocoder)
library(lubridate)

# Others
library(fs)
library(plotly)
library(shinycssloaders)
library(waiter)
library(callr)


# UI ----

ui <- dashboardPage(
  preloader = list(html = tagList(spin_5(), "Loading App ..."),
                   color = "#2C3E50"),
  dashboardHeader(title = dashboardBrand(
    title = "Google Trends ",
    color = "gray-dark",
    image = "google_trends_logo.png",
  ),
  titleWidth = 500),
  sidebar = dashboardSidebar(
    minified = F,
    width = "350px",
    skin = "light",
    uiOutput("search_terms"),
    uiOutput("time_horizon"),
    actionButton(inputId = "submit", label = "Search", status = "primary")
  ),
  
  dashboardBody(
    fluidPage(
      tags$head(
        tags$style(".butt{background:#dc3545;} .butt{color: white;}"),
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
          )
        )
      ),
    fluidRow(
      column(
        3,
        box(title = "Information", status = "danger",
            height = 400, width = 12,
            icon = icon("search"),
            solidHeader = TRUE,
            p(strong("Goal of the App:"), "  determine whether there has been less interest in topics such as 
              remote work and more interest in economic key terms as we are getting into a recession.
              The results will be based on the inputs added to the sider such as search terms and time horizon."),
            br(),
            p(strong("Note on Loading Time:"), "The app might take sometime to display the results, please wait
              approximately 20 seconds until the final plots and tables display on each box."),
            p(strong("Wait a few seconds for the results to dislay!"))
            ),
       
      ),
      column(
        9,
        box(
          title = "Interest OverTime",
          status = "secondary",
          width = 12,
          height = 400,
          icon = icon("chart-line"),
          solidHeader = TRUE,
          maximizable = FALSE,
          # Line chart plotlyOutput
          withSpinner(type = getOption("spinner.type", default = 3),
                      color.background = "#2C3E50",
                      color = getOption("spinner.color", default = "#F2300F"),
                      plotlyOutput(outputId = "plot_1" ))
        )
      )),
    
    fluidRow(
      column(5,
             box(title = "Interest by State", status = "secondary", height = 550,
                 width = 12, icon = icon("fa-solid fa-table"), solidHeader = TRUE,
                 maximizable = FALSE,
                 # Table Output
                 div(style='height:525px; overflow-y: scroll',withSpinner(type = getOption("spinner.type", default = 3),
                                                                                 color.background = "#2C3E50",
                                                                                 color = getOption("spinner.color", default = "#F2300F"),
                                                                                 gt_output(outputId = "tbl_1")))
                 )),
      column(7,
             box(title = "US States", status = "secondary", height = , 
                 width = 12, icon = icon("flag-usa"), solidHeader = TRUE,
                 maximizable = FALSE,
                 # Plotly Output for US-States
                 withSpinner(type = getOption("spinner.type", default = 3),
                             color.background = "#2C3E50",
                             color = getOption("spinner.color", default = "#F2300F"),
                             plotlyOutput("plot_2", width = "auto", height = 350))
                 ))
    )

)
)
)

server <- function(input, output) {
  
  
  # Key terms input
  output$search_terms <- renderUI({
    selectizeInput(
      inputId = "search_terms",
      label = "Search Term(s)",
      choices = c("remote work",
                  "recession",
                  "economy",
                  "economic crisis",
                  "resignation",
                  "quit",
                  "job",
                  "find jobs",
                  "unemployment",
                  "high salary",
                  "work benefits",
                  "sick",
                  "sick leave",
                  "pandemic",
                  "layoff",
                  "hybrid",
                  "covid"
                  ),
      multiple = TRUE,
      options = list(maxItems = 3),
      selected = c("remote work", "recession", "economy"))
  })
  
  # Time Horizon
  
  output$time_horizon <- renderUI({
    selectInput(
      inputId = "search_horizon",
      label = "Search Period",
      choices = c(
                  "Last hour" = "now 1-H",
                  "Last four hours" = "now 4-H",
                  "Last day" = "now 1-d",
                  "Last seven days" = "now 7-d",
                  "Past 30 days" = "today 1-m",
                  "Past 90 days" = "today 3-m",
                  "Past 12 months" = "today 12-m",
                  "Last five years" = "today+5-y",
                  "Since the beginning of Google Trends (2004)" = "all"),
      selected = "now 7-d"
    )
  })
  

  # QUERY GTRENDS LIST ----
  
  reactive_val <- reactiveValues()
  
  
  observeEvent(input$submit, handlerExpr = {
    
    # Ensure the values are there
    req(input$search_terms)
    req(input$search_horizon)
    
    
    id <- showNotification("Pulling Gtrends Data ...", 
                           duration = NULL,
                           closeButton = FALSE,
                           type = "error")
    
    # Remove Search query notification once finished
    on.exit(removeNotification(id), add = TRUE)
    
    # Output
    reactive_val$ggtrend <- gtrends(input$search_terms,
                                    geo  = "US",
                                    time = input$search_horizon)
    
    
    
    
  }, ignoreNULL = FALSE)
  
  
  # PLOT 1 - TIME SERIES
  
  output$plot_1 <- renderPlotly({
    
    req(reactive_val$ggtrend)
    
     ggplotly(reactive_val$ggtrend %>%
      pluck("interest_over_time") %>% 
      select(date, hits, keyword) %>% 
      mutate(
        hits = hits %>% as.numeric()
      ) %>% 
      as_tibble() %>% 
      ggplot(aes(x = date, y = hits, color = keyword)) +
      geom_line() +
      theme_tq() +
      scale_color_manual(values = c("#555555", "#2C3E50", "#F2300F")) + 
      geom_smooth(aes(color = keyword), method = "loess", se = F, lty = "dashed") + 
      labs(
        x = ""
      )
)
  })
     
# Table 1 - Render Table
output$tbl_1 <- render_gt(expr = {

       req(reactive_val$ggtrend)

       geo_tbl <- reactive_val$ggtrend %>%
         pluck("interest_by_region") %>%
         select(location, hits, keyword)

       
       geo_tbl %>%
       dplyr::group_by(location) %>%
         dplyr::summarize(trend = list(hits),
                          hits      = round(mean(hits, na.rm = TRUE),2)) %>%
         ungroup() %>% 
         arrange(desc(hits)) %>% 
         mutate(
           rank = row_number()
         ) %>% 
         gt() %>%
         gt_plt_sparkline(trend,
                          type = "shaded",
                          palette = c("#2C3E50", "#555555", "#E31A1C", "#2C3E50", "#A6CEE3")
                          ) %>% 
         gt_theme_pff() %>% 
           tab_header( title = md("Summary of Interest by **US States**"),
           subtitle = md("The dots in the trendline represent the **order of the Search Term(s)**,
                         allowing you to compare trends between keywords by **US state**.")) %>% 
         cols_width(
           `location` ~ px(130),
           `trend` ~ px(130),
           `hits` ~ px(130),
           `rank` ~ px(130)
         ) %>% 
         tab_footnote(
           footnote = md("Hits is a representation of the **average** hits.")
         )
         
         
       })


      # US Map Plotly
      output$plot_2 <- renderPlotly(expr = {
  
      req(reactive_val$ggtrend)
  
      # Main Map Tbl
      map_tbl <- reactive_val$ggtrend %>%
        pluck("interest_by_region") %>%
        select(location, hits, keyword)
  
      
      # Map plot
      map_tbl %>% 
        group_by(location) %>% 
        summarize(
          hits = round(mean(hits, na.rm = TRUE),2),
          state = state.abb[match(location, state.name)]
        ) %>% 
        ungroup() %>% 
        arrange(desc(hits)) %>% 
        mutate(
          rank = row_number(),
          label_txt = str_glue("State: {state}")
        ) %>% 
        plot_geo(locationmode = "USA-states") %>% 
        add_trace(
          z         = ~ hits,
          locations = ~ state,
          color     = ~ hits,
          text      = ~ label_txt,
          colors    = "Blues"
        ) %>% 
        layout(
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa"),
            showlakes = TRUE,
            lakecolor = toRGB("white")
          )
        )
        
     })
    
    
    

  
  
}

shinyApp(ui, server)
