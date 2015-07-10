library(stringr)

load("../data_processed/fao_meta.RData")
load("FAOmetaTable.RData")

groupTable <- FAOmetaTable[[1]]
domainTable <- FAOmetaTable[[2]]
itemTable <- FAOmetaTable[[3]]
itemAggTable <- FAOmetaTable[[4]]
elementTable <- FAOmetaTable[[5]]

server <- function(input, output) {
  
  output$group <- renderUI({
    groupNames <- groupTable[["groupName"]]
    opts <- selectInput("gc_name", "Which Group are you looking for:",choices = groupNames, selected=groupNames[2])
    list(opts)
  })
  
  output$domain <- renderUI({
    
    gc <- groupTable[groupTable$groupName == input$gc_name,]$groupCode
    #     x <- groupNames[1]
    #     gc <- groupTable[groupTable$groupName == x,]$groupCode
    subdomainTable <- domainTable[domainTable$groupCode == gc,]
    domainNames <- subdomainTable[["domainName"]]
    opts <- selectInput("domain_name", "Which Domain are you looking for:",
                        choices = domainNames, selected=domainNames[1])
    list(opts)
  })
  
  output$indOrAgg <- renderUI({
    ind_or_agg <- data.frame(name = c("(0) Individual item (e.g. Apples, Wheat)",
                                      "(1) Aggregated item (e.g. Total cereals, Total meat"),
                             code = c(0,1),
                             stringsAsFactors = FALSE
    )
    values <- ind_or_agg$name
    opts <- selectInput("ind_or_agg", "Are you looking for individual item or aggregated item:",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  output$item <- renderUI({
    
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    #    agg <- ind_or_agg[ind_or_agg$name == input$ind_or_agg,]$code
    #     x <- values[2]
    #     agg <- ind_or_agg[ind_or_agg$name == x,]$code
    #if (agg == 1) {
    #  subitemTable = itemAggTable[itemAggTable$domainCode == dc,]
    #} else {
    subitemTable = itemTable[itemTable$domainCode == dc,]
    #}
    values <- subitemTable$itemName
    opts <- selectInput("item_name", "Which Item are you looking for?",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  output$element <- renderUI({
    
    
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    subelementTable = elementTable[elementTable$domainCode == dc,]
    values <- as.character(subelementTable$elementName)
    opts <- selectInput("element_name", "Which Element are you looking for?",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  fao_data <- reactive({
    
    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    ec <- elementTable[elementTable$elementName == input$element_name & elementTable$domainCode == dc,]$elementCode
    ic <- itemTable[itemTable$itemName == input$item_name & itemTable$domainCode == dc,]$itemCode
    
    dat <- fao_meta[fao_meta$ElementCode == ec & fao_meta$ItemCode == ic, ]
    dat
  })

  output$mytable = renderDataTable({
    fao_data()
  },options = list(pageLength = 10))

  fao_data_raw <- reactive({
    
    row <- fao_data()
    load(paste0("../data_processed/",as.character(row$file_name)))
    dat
  })
  
  output$varnames <- renderUI({
    
    d <- fao_data_raw()
    
    values <- names(d)[-1:-3]
    opts <- selectInput("varnames", "Pick var?",
                        choices = values, selected=values[1])
    list(opts)
  })
  
  output$rawtable = renderDataTable({
    dd <- fao_data_raw()
    dd[c("CountryCode","Country","Year",input$varnames)]
  },options = list(pageLength = 10))
  
    
  
  }

ui <- shinyUI(navbarPage("FAOSTAT data explorer", id="nav",
                         
                         tabPanel("Single-variable explorer",
                                  div(class="inner",
                                      
                                      tags$head(
                                        # Include our custom CSS
                                        includeCSS("styles.css"),
                                        # Hide the red error messages!!!
                                        tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                                        )
                                        
                                      ), 
                                      #tags$style(type="text/css", "body {padding-left: 70px;}"),
                                      tags$h4("Select data"),
                                      tags$hr(),
                                      fluidRow(
                                        shiny::column(4, uiOutput("group")
                                        ),
                                        shiny::column(4, uiOutput("domain")
                                        )#,
                                        #shiny::column(4, uiOutput("indOrAgg")
                                        )
                                      ),
                                      fluidRow(
                                        shiny::column(4, uiOutput("item")
                                        ),
                                        shiny::column(4, uiOutput("element")
                                        )#,
                                        #shiny::column(4, uiOutput("yearRange")
                                        )
                                      ),
                                      tags$hr(),
                                      fluidRow(
                                        shiny::column(5, tags$h4("Preview data")),
                                        shiny::column(5, tags$h4("Display time-series")),
                                        shiny::column(2, tags$h4("Download data"))
                                      ),
                                      fluidRow(
                                        shiny::column(12, dataTableOutput("mytable"))#,
                                        #shiny::column(2, downloadButton('downloadData', 'Download'))
                                      ),
                                     fluidRow(
                                       shiny::column(4, uiOutput("varnames")
                                       )
                                     ),
                                     fluidRow(
                                       shiny::column(12, dataTableOutput("rawtable"))#,
                                       #shiny::column(2, downloadButton('downloadData', 'Download'))
                                     )
                                  )
                         )

shinyApp(ui = ui, server = server)