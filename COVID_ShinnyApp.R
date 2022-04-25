#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




if(!require(covid19.analytics)) install.packages("covid19.analytics", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stats)) install.packages("stats", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sqldf)) install.packages("sqldf", repos = "http://cran.us.r-project.org")


options(scipen = 999)
#Loading In Data
covid19DF = covid19.data()


#Country Summary prep

countryDeaths = sqldf(
    "SELECT Country_Region Country, sum(Deaths) Deaths, sum(Confirmed) Cases, sum(Active) Active  FROM covid19DF GROUP BY Country_Region "
)


avgActiveCases = median(countryDeaths$Active)
avgTotalCases = median(countryDeaths$Cases)
avgDeaths = median(countryDeaths$Deaths)


countryDeaths$WorldActiveAvg = avgActiveCases
countryDeaths$WorldTotalCasesAvg = avgTotalCases
countryDeaths$WorldDeathsAvg = avgDeaths

countryDeaths$ActIndicator = if_else(
    countryDeaths$Active > countryDeaths$WorldActiveAvg,
    "Above Average",
    "Below Average"
)
countryDeaths$CaseIndicator = if_else(
    countryDeaths$Cases > countryDeaths$WorldTotalCasesAvg,
    "Above Average",
    "Below Average"
)
countryDeaths$DeathIndicator = if_else(
    countryDeaths$Deaths > countryDeaths$WorldDeathsAvg,
    "Above Average",
    "Below Average"
)


#Region Summary Prep


RegionavgActiveCases = median(covid19DF$Active)
RegionavgTotalCases = median(covid19DF$Confirmed)
RegionavgDeaths = median(covid19DF$Deaths)


covid19DF$WorldActiveAvg = avgActiveCases
covid19DF$WorldTotalCasesAvg = avgTotalCases
covid19DF$WorldDeathsAvg = avgDeaths

covid19DF$ActIndicator = if_else(covid19DF$Active > covid19DF$WorldActiveAvg,
                                 "Above Average",
                                 "Below Average")
covid19DF$CaseIndicator = if_else(covid19DF$Confirmed > covid19DF$WorldTotalCasesAvg,
                                  "Above Average",
                                  "Below Average")
covid19DF$DeathIndicator = if_else(covid19DF$Deaths > covid19DF$WorldDeathsAvg,
                                   "Above Average",
                                   "Below Average")





#Functions


#Country summary function

CounntryBarDeaths = function(df) {
    a =   ggplot(df,
                 aes(
                     reorder(Country, -Deaths),
                     Deaths,
                     fill = DeathIndicator,
                     group = 1,
                     text =   paste0(
                         "<b>",
                         Country,
                         " </b>",
                         "<br>",
                         "<b>",
                         df$DeathIndicator,
                         "<br>",
                         'Deaths: ',
                         scales::comma(Deaths, 1),
                         "<br>"
                     )
                 )) +
        geom_bar(stat = "identity") +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = median(df$WorldDeathsAvg),
                   color = "yellow") +
        annotate(
            "text",
            x = .7,
            y = median(df$WorldDeathsAvg) + 1000,
            size = 2,
            label = c("World Average")
        ) +
        labs(
            title = "COVID Deaths by Country",
            x = "Country",
            y = "Deaths",
            fill =  "World Average"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)+
        scale_x_discrete(guide = guide_axis(n.dodge=3))
    
    b = ggplotly(a, tooltip = c("text")) %>%
        layout(title = list(text = paste0(
            'COVID Deaths by Country',
            '<br>',
            '<sup>',
            '</sup>'
        )))
    
    
    
    
    
    
}

CounntryBarCases = function(df) {
    a =   ggplot(df,
                 aes(
                     reorder(Country, -Cases),
                     Cases,
                     fill = CaseIndicator,
                     group = 1,
                     text =   paste0(
                         "<b>",
                         Country,
                         " </b>",
                         "<br>",
                         "<b>",
                         CaseIndicator,
                         "<br>",
                         'Cases: ',
                         scales::comma(Cases, 1),
                         "<br>"
                     )
                 )) + #Source: https://community.rstudio.com/t/how-to-display-plot-values-with-comma-separated-on-ggplotly/48498
        geom_bar(stat = "identity") +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = median(df$WorldTotalCasesAvg),
                   color = "yellow") +
        annotate(
            "text",
            x = .7,
            y = median(df$WorldTotalCasesAvg) + 1000,
            size = 2,
            label = c("World Average")
        ) +
        labs(
            title = "COVID Deaths by Country",
            x = "Country",
            y = "Total Cases",
            fill =  "World Average"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    
    b = ggplotly(a, tooltip = c("text")) %>%
        layout(title = list(text = paste0(
            'COVID Deaths by Country',
            '<br>',
            '<sup>',
            '</sup>'
        )))
    
    
    
}


CounntryBarActive = function(df) {
    a =    ggplot(df ,
                  aes(
                      reorder(Country, -Active),
                      Active,
                      fill = ActIndicator,
                      text =   paste0(
                          "<b>",
                          Country,
                          " </b>",
                          "<br>",
                          "<b>",
                          df$ActIndicator,
                          "<br>",
                          'Active: ',
                          scales::comma(Active, 1),
                          "<br>"
                      )
                  )) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = median(df$WorldActiveAvg),
                   color = "yellow") +
        annotate(
            "text",
            x = .7,
            y = median(df$WorldActiveAvg) + 1000,
            size = 2,
            label = c("World Average")
        ) +
        labs(
            title = "COVID Active Cases by Country",
            x = "Country",
            y = "Active Cases",
            subtitle =  " world average",
            fill =  "World Average"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    
    b = ggplotly(a, tooltip = c("text")) %>%
        layout(title = list(text = paste0(
            'COVID Deaths by Country',
            '<br>',
            '<sup>',
            '</sup>'
        )))
    
    
    
    
}



#Sub region functions


RegionBarDeaths = function(df) {
    a =   ggplot(df,
                 aes(
                     reorder(Admin2, -Deaths),
                     Deaths,
                     fill = DeathIndicator,
                     text =   paste0(
                         "<b>",
                         Admin2,
                         " </b>",
                         "<br>",
                         "<b>",
                         df$DeathIndicator,
                         "<br>",
                         'Deaths: ',
                         scales::comma(Deaths, 1),
                         "<br>"
                     )
                 )) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = mean(df$Deaths), color = "blue") +
        annotate(
            "text",
            x = .7,
            y = mean(df$Deaths) + 300,
            size = 2,
            label = c("Selected Regions Average")
        ) +
        geom_hline(yintercept = median(df$WorldDeathsAvg),
                   color = "yellow") +
        annotate(
            "text",
            x = .7,
            y = median(df$WorldDeathsAvg) + 100,
            size = 2,
            label = c("World Average")
        ) +
        labs(
            title = "COVID Deaths by Region",
            x = "Region",
            y = "Deaths",
            fill =  "World Average",
            color = NULL
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    
    
    
    b = ggplotly(a, tooltip = c("text")) %>%
        layout(title = list(text = paste0(
            'COVID Deaths by Country',
            '<br>',
            '<sup>',
            '</sup>'
        )))
    
    
    
    
    
    
}

RegionBarCases = function(df) {
    a =   ggplot(df,
                 aes(
                     reorder(Admin2, -Confirmed),
                     Confirmed,
                     fill = CaseIndicator,
                     group = 1,
                     text =   paste0(
                         "<b>",
                         Admin2,
                         " </b>",
                         "<br>",
                         "<b>",
                         CaseIndicator,
                         "<br>",
                         'Cases: ',
                         scales::comma(Confirmed, 1),
                         "<br>"
                     )
                 )) + #Source: https://community.rstudio.com/t/how-to-display-plot-values-with-comma-separated-on-ggplotly/48498
        geom_bar(stat = "identity") +
        geom_hline(yintercept = mean(df$Confirmed), color = "blue") +
        annotate(
            "text",
            x = .7,
            y = mean(df$Confirmed) + 3000,
            size = 2,
            label = c("Selected Regions Average")
        ) +
        geom_hline(yintercept = median(df$WorldTotalCasesAvg),
                   color = "yellow") +
        annotate(
            "text",
            x = .7,
            y = median(df$WorldTotalCasesAvg) + 1000,
            size = 2,
            label = c("World Average")
        ) +
        labs(
            title = "COVID Deaths by Country",
            x = "Region",
            y = "Total Cases",
            fill =  "World Average"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    
    b = ggplotly(a, tooltip = c("text")) %>%
        layout(title = list(text = paste0(
            'COVID Deaths by Country',
            '<br>',
            '<sup>',
            '</sup>'
        )))
    
    
    
}


RegionBarActive = function(df) {
    a =    ggplot(df ,
                  aes(
                      reorder(Admin2, -Active),
                      Active,
                      fill = ActIndicator,
                      group = 1,
                      text =   paste0(
                          "<b>",
                          Admin2,
                          " </b>",
                          "<br>",
                          "<b>",
                          df$ActIndicator,
                          "<br>",
                          'Active: ',
                          scales::comma(Active, 1),
                          "<br>"
                      )
                  )) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = mean(df$Active), color = "blue") +
        annotate(
            "text",
            x = .7,
            y = mean(df$Active) + 3000,
            size = 2,
            label = c("Selected Regions Average")
        ) +
        geom_hline(yintercept = median(df$WorldActiveAvg),
                   color = "yellow") +
        annotate(
            "text",
            x = .7,
            y = median(df$WorldActiveAvg) + 1500,
            size = 2,
            label = c("World Average")
        ) +
        labs(
            title = "COVID Active Cases by Country",
            x = "Region",
            y = "Active Cases",
            subtitle =  " world average",
            fill =  "World Average"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
    
    b = ggplotly(a, tooltip = c("text")) %>%
        layout(title = list(text = paste0(
            'COVID Deaths by Country',
            '<br>',
            '<sup>',
            '</sup>'
        )))
    
    
    
    
}

















ui <- bootstrapPage(
    navbarPage(
        theme = shinytheme("yeti"),
        collapsible = TRUE,
        HTML(
            '<a style="text-decoration:none;cursor:default;color:#FF0000;" class="active" href="#">COVID-19 Daily Update</a>'
        ),
        id = "nav",
        windowTitle = "COVID-19 Daily Update",
        
        
        tabPanel("Country Summary",
                 sidebarLayout(
                     sidebarPanel(
                         pickerInput(
                             "region_select",
                             "Country:",
                             choices = as.character(countryDeaths[order(-countryDeaths$Deaths),]$Country),
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = as.character(countryDeaths[order(-countryDeaths$Deaths),]$Country)[1:10],
                             multiple = TRUE
                         )
                     ),
                     
                     
                     
                     
                     mainPanel(tabsetPanel(
                         tabPanel(
                             "COVID Deaths",
                             plotlyOutput("BarGraphDeath"),
                             br(),
                             br(),
                             br(),
                             dataTableOutput("DataTableDeaths")
                         ),
                         
                         tabPanel(
                             "COVID Total Cases",
                             plotlyOutput("BarGraphCases"),
                             br(),
                             br(),
                             br(),
                             dataTableOutput("DataTableCases")
                         ),
                         
                         tabPanel(
                             "COVID Cases Active",
                             plotlyOutput("BarGraphActiveCases"),
                             br(),
                             br(),
                             br(),
                             dataTableOutput("DataTableActiveCases")
                         )
                         
                         
                         
                         
                         
                     ))
                     
                 )),
        
        tabPanel("Region Summary",
                 sidebarLayout(
                     sidebarPanel(
                         pickerInput(
                             "country_select",
                             "Country:",
                             choices = unique(as.character(covid19DF$Country_Region)),
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = 'US',
                             multiple = FALSE
                         ),
                         
                         pickerInput(
                             "sub_country_select",
                             "Region:",
                             choices = unique(as.character(covid19DF$Province_State)),
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = 'New York',
                             multiple = FALSE
                         ),
                         
                         pickerInput(
                             "sub_region_select",
                             "Sub-Region:",
                             choices = unique(as.character(covid19DF$Admin2)),
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = c(
                                 "Albany",
                                 "Queens",
                                 "Kings",
                                 "Orange",
                                 "Richmond",
                                 "New York",
                                 "Bronx"
                             ),
                             multiple = TRUE
                         )
                     ),
                     
                     mainPanel(tabsetPanel(
                         tabPanel(
                             "COVID Deaths",
                             plotlyOutput("BarGraphDeathRegion"),
                             br(),
                             br(),
                             br(),
                             dataTableOutput("DataTableDeathsRegion")
                             
                         ),
                         
                         tabPanel(
                             "COVID Total Cases",
                             plotlyOutput("BarGraphCasesRegion"),
                             br(),
                             br(),
                             br(),
                             dataTableOutput("DataTableCasesRegion")
                             
                         ),
                         
                         tabPanel(
                             "COVID Cases Active",
                             plotlyOutput("BarGraphActiveCasesRegion"),
                             br(),
                             br(),
                             br(),
                             dataTableOutput("DataTableActiveCasesRegion")
                             
                         )
                         
                         
                         
                         
                         
                     ))
                     
                 ))
        
        
        
    )
    
    
    
    
    
)





server <- function(input, output) {
    countryDF <- reactive({
        countryDeaths %>% filter(Country %in% input$region_select)
        
        
    })
    
    
    
    #First Tab summary code
    
    output$BarGraphDeath <- renderPlotly({
        CounntryBarDeaths(countryDF())
    })
    
    output$BarGraphCases <- renderPlotly({
        CounntryBarCases(countryDF())
    })
    
    
    output$BarGraphActiveCases <- renderPlotly({
        CounntryBarActive(countryDF())
    })
    
    output$DataTableDeaths <- renderDataTable({
        countryDF() %>%
            select("Country", "Deaths") %>%
            arrange(desc(Deaths)) %>%
            mutate(Deaths = formatC(
                round(Deaths),
                format = "f",
                big.mark = ",",
                drop0trailing = TRUE
            )) %>%
            datatable()
        
        
        
        
    })
    
    output$DataTableCases <- renderDataTable({
        countryDF() %>%
            select("Country", "Cases") %>%
            arrange(desc(Cases)) %>%
            mutate(Cases = formatC(
                round(Cases),
                format = "f",
                big.mark = ",",
                drop0trailing = TRUE
            )) %>%
            datatable()
        
        
        
    })
    
    
    output$DataTableActiveCases <- renderDataTable({
        countryDF() %>%
            select("Country", "Active") %>%
            arrange(desc(Active)) %>%
            mutate(Active = formatC(
                round(Active),
                format = "f",
                big.mark = ",",
                drop0trailing = TRUE
            )) %>%
            datatable()
        
        
    })
    
    
    
    
    
    #Second tab region code
    countryDF_2 <- reactive({
        covid19DF %>%
            filter(
                Country_Region == input$country_select,
                Province_State == input$sub_country_select,
                Admin2 %in% input$sub_region_select
            )
        
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$BarGraphDeathRegion <- renderPlotly({
        RegionBarDeaths(countryDF_2 ())
    })
    
    output$BarGraphCasesRegion <- renderPlotly({
        RegionBarCases(countryDF_2 ())
    })
    
    
    output$BarGraphActiveCasesRegion <- renderPlotly({
        RegionBarActive(countryDF_2 ())
    })
    
    output$DataTableDeathsRegion <- renderDataTable({
        countryDF_2 () %>%
            select("Admin2", "Deaths") %>%
            arrange(desc(Deaths)) %>%
            mutate(Deaths = formatC(
                round(Deaths),
                format = "f",
                big.mark = ",",
                drop0trailing = TRUE
            )) %>%
            datatable()
        
        
        
        
    })
    
    output$DataTableCasesRegion <- renderDataTable({
        countryDF_2 () %>%
            select("Admin2", "Confirmed") %>%
            arrange(desc(Confirmed)) %>%
            mutate(Confirmed = formatC(
                round(Confirmed),
                format = "f",
                big.mark = ",",
                drop0trailing = TRUE
            )) %>%
            datatable()
        
        
        
    })
    
    
    output$DataTableActiveCasesRegion <- renderDataTable({
        countryDF_2 () %>%
            select("Admin2", "Active") %>%
            arrange(desc(Active)) %>%
            mutate(Active = formatC(
                round(Active),
                format = "f",
                big.mark = ",",
                drop0trailing = TRUE
            )) %>%
            datatable()
        
        
    })
    
    
    
    
    
    
    
}
























# Run the application
shinyApp(ui = ui, server = server)

