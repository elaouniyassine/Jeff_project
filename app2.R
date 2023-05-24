library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(lazyeval)
library(maps)
library(plotly)
library(reactable)
library(zoo)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(shinycssloaders)
library(shinydashboard)

load("DashboardData.RData")

highchart_theme <- hc_theme(
     colors = c("#ffba08", "#faa307", "#f48c06", "#e85d04","#dc2f02","#d00000",  "#9d0208",  "#6a040f", "#370617", "#000"),
     chart = list(
          backgroundColor = "#FFFFFF",
          plotBackgroundColor = "#FFFFFF",
          plotBorderColor = "#ff8c04",
          plotBorderWidth = 0,
          style = list(fontFamily = "Arial")
     ),
     legend = list(
          backgroundColor = "#FFFFFF",
          itemStyle = list(color = "#000000"),
          itemHoverStyle = list(color = "#000000"),
          itemHiddenStyle = list(color = "#BDBDBD"),
          borderColor = "#fff",
          borderWidth = 0
     ),
     plotOptions = list(
          series = list(
               marker = list(
                    symbol = "circle",
                    states = list(
                         hover = list(fillColor = "#ff8c04", radiusPlus = 2),
                         select = list(fillColor = "#FFA000", radiusPlus = 2)
                    )
               )
          ),
          column = list(
               borderColor = "#FFFFFF",
               borderWidth = 2,
               stacking = "normal"
          )
     ),
     yAxis = list(
          title = list(
               text = "Total",
               style = list(color = "#000000")
          ),
          labels = list(style = list(color = "#000000")),
          gridLineColor = "#BDBDBD"
     ),
     xAxis = list(
          title = list(
               text = "Financial Period",
               style = list(color = "#000000")
          ),
          labels = list(style = list(color = "#000000"))
     ),
     credits = list(enabled = FALSE)
)

# Define attribute options
atts <- c("ProductType")
productAtts <- c("ProductType")
factorAtts <- c() # will need to reorder for purpose of stacked bar

# define presence of COGS
cogsIncluded <- TRUE

attSummaryDf <- paste0("attSummary", atts[1])
ProdDetailDf <- paste0("ProdDetail", productAtts[1])

CapCamelCase <- function(string) {
     paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
}





ui <- fluidPage(
     
     includeCSS("www/mycss.css"),
     tags$head(
          tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png"),
          tags$title("Digital Fuel Capital"),
          tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/admin-lte/2.4.18/css/AdminLTE.min.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/admin-lte/2.4.18/css/skins/skin-blue.min.css"),
          tags$script(
               HTML(
                    '

const zoomLevels = [
  { width: 1100, zoom: 0.80 },   // Small screens
  { width: 1400, zoom: 0.85 },      // Medium screens
  { width: Infinity, zoom: 1} // Large screens
];

const screenWidth = window.innerWidth;
let zoomLevel = 1;

for (const level of zoomLevels) {
  if (screenWidth < level.width) {
    zoomLevel = level.zoom;
    break;
  }
}

$(document).ready(function() { document.body.style.zoom = zoomLevel;});

// window.alert(zoomLevel);
               
               window.addEventListener("click", function() {
               document.getElementById("btn-summary-metrics-yoy").style.color = "black";
               document.getElementById("btn-Repeat-Frequency").style.color = "black";
               document.getElementById("btn-repeat-revenue-trend").style.color = "black";
               document.getElementById("btn-cohort-financial-contribution").style.color = "black";
               document.getElementById("btn-category-economics-summary").style.color = "black";
               document.getElementById("btn-Geography").style.color = "black";
               const elementsWithClass = document.querySelectorAll("div.active");
               const firstElementId = elementsWithClass[0].id;
               document.getElementById("btn-".concat(firstElementId)).style.color = "orange";
               });

      '
               )
          )
     ),
     HTML('
          <div id="loader-wrapper">
        <div id="loader"></div>
        <div class="loader-section section-left"></div>
    </div>
    <!-- /Preloader -->
    <div class="body-white">'),
     tags$div(
          class = "header",
          tags$div(
               class = "container",
               tags$div(
                    class = "header-wrapper",
                    tags$a(href = "https://www.digitalfuelcapital.com/",
                           tags$img(src = "https://www.digitalfuelcapital.com/img/logo.png",
                                    height = "45px")
                    ),
                    tags$div(class = "aside-btn",
                             style = "display: flex; align-items: center;",
                             tags$svg(`enable-background` = "new 0 0 512 512",
                                      fill = "currentColor",
                                      version = "1.1",
                                      viewbox = "0 0 512 512",
                                      xmlns = "http://www.w3.org/2000/svg",
                                      tags$g(tags$rect(height = "64",
                                                       width = "256",
                                                       x = "128",
                                                       y = "320"),
                                             tags$rect(height = "64",
                                                       width = "256",
                                                       x = "128",
                                                       y = "224"),
                                             tags$path(d = "M480,0H32C14.312,0,0,14.312,0,32v448c0,17.688,14.312,32,32,32h448c17.688,0,32-14.312,32-32V32   C512,14.312,497.688,0,480,0z M448,448H64V64h384V448z"),
                                             tags$rect(height = "64",
                                                       width = "256",
                                                       x = "128",
                                                       y = "128")
                                      )
                             )
                             #,
                             #tags$h1("Your content")
                    )
               )
          )
     ),
     
     
     tags$div(class = "wrapper",
              
              tags$div(class = "container",
                       tags$div(
                            class = "page-content",
                            tags$div(
                                 class = "aside-container open",
                                 tags$aside(
                                      class = "aside open",
                                      tags$nav(
                                           class = "aside__nav",
                                           tags$ul(
                                                class = "aside__items",
                                                `data-spollers` = "", `data-one-spoller` = "",
                                                tags$li(class = "aside__item summary-metrics-item",
                                                        tags$button(
                                                             type = "button", class = "aside__nav-btn _spoller-active",
                                                             `data-spoller` = "", 
                                                             "Summary metrics",
                                                             href = "#summary-metrics",
                                                             `data-toggle` = "tab"
                                                        ),
                                                        tags$ul(class = "aside__submenu",
                                                                tags$li(
                                                                     tags$ul(
                                                                          tags$button(
                                                                               type = "button",
                                                                               "summary metrics yoy", tags$i("→"),
                                                                               `data-toggle` = "tab", href = "#summary-metrics-yoy",
                                                                               id = "btn-summary-metrics-yoy",
                                                                               style = "color: orange;"
                                                                          )
                                                                     )
                                                                )
                                                        )
                                                ),
                                                tags$li(
                                                     class = "aside__item",
                                                     tags$button(
                                                          type = "button", class = "aside__nav-btn",
                                                          `data-toggle` = "tab",
                                                          href = "#Cohort-Analysis",
                                                          `data-spoller` = "", "Cohort Analysis",
                                                          id = "btn-Cohort-Analysis"
                                                     ),
                                                     
                                                     tags$ul(
                                                          class = "aside__submenu", hidden = "",
                                                          tags$li(tags$h4("Cohort summary")),
                                                          tags$li( 
                                                               tags$ul(
                                                                    tags$button(
                                                                         type = "button",
                                                                         href = "#category-economics-summary",
                                                                         `data-toggle` = "tab",
                                                                         
                                                                         "Category Economics Summary", tags$i("→"),
                                                                         id = "btn-category-economics-summary"
                                                                    )
                                                               )
                                                          ),
                                                          tags$li( 
                                                               tags$ul(
                                                                    tags$button(
                                                                         type = "button",class = "cohort-financial-contribution", 
                                                                         href = "#cohort-financial-contribution",
                                                                         `data-toggle` = "tab",
                                                                         
                                                                         "Cohort Financial Contribution", tags$i("→"),
                                                                         id = "btn-cohort-financial-contribution"
                                                                    )
                                                               )
                                                          ),
                                                          tags$li(
                                                               tags$ul(
                                                                    tags$button(
                                                                         type = "button",
                                                                         href = "#repeat-revenue-trend",
                                                                         `data-toggle` = "tab",
                                                                         
                                                                         "Repeat Revenue trend", tags$i("→"),
                                                                         id = "btn-repeat-revenue-trend"
                                                                    )
                                                               )
                                                          ),
                                                          tags$li(tags$h4("Customer Behavior")),
                                                          tags$li( 
                                                               tags$ul(
                                                                    tags$button(
                                                                         type = "button", 
                                                                         href = "#Repeat-Frequency",
                                                                         `data-toggle` = "tab",
                                                                         "Repeat Frequency", tags$i("→"),
                                                                         id = "btn-Repeat-Frequency"
                                                                    )
                                                               )
                                                          )
                                                     )
                                                ),
                                                tags$li(
                                                     tags$ul(
                                                          class = "aside__item",
                                                          tags$button(
                                                               type = "button",class = "aside__nav-btn geografy-item",
                                                               `data-spoller` = "","Geography",
                                                               href = "#Geography",
                                                               `data-toggle` = "tab",
                                                               id = "btn-Geography"
                                                          ),
                                                          tags$ul(class = "aside__submenu", hidden = ""
                                                          )
                                                     )
                                                )
                                                
                                           ),
                                           br(),br(),
                                           tags$div(
                                                class = "aside-buttons",
                                                tags$div(
                                                     class = "button-load aside-load-button",
                                                     tags$a(
                                                          #id = "DnldTotalOutcomesTable",
                                                          #href = "session/eb47213fcd80c0960ff5f4af67c4f173/download/DnldTotalOutcomesTable?w=",
                                                          downloadButton('DnldTotalOutcomesTable', 'Download Table'),
                                                          target = "_blank",
                                                          download = "",
                                                          #"Download Table",
                                                          `aria-live` = "polite"
                                                     )
                                                ),
                                                tags$div(
                                                     class = "buttons-cohort-financial hidden",
                                                     tags$div(
                                                          class = "button-load",
                                                          tags$a(
                                                               downloadButton('DnldCohortFinancialTable', 'Download Cohort Table'),
                                                               #href = "#",
                                                               target = "_blank",
                                                               download = "",
                                                               #"DOWNLOAD COHORT TABLE",
                                                               `aria-live` = "polite"
                                                          )
                                                     ),
                                                     tags$div(
                                                          class = "button-load",
                                                          tags$a(
                                                               downloadButton('DnldNewCustFinancialTable', 'New/Repeat Table'),
                                                               #href = "#",
                                                               target = "_blank",
                                                               download = "",
                                                               #"DOWNLOAD",
                                                               #tags$br(), "NEW/REPEAT TABLE",
                                                               `aria-live` = "polite"
                                                          )
                                                     )
                                                )
                                           )
                                           
                                      )
                                 )
                            )
                            ,tags$div(
                                 class= "tab-content",
                                 tags$div(
                                      class = "tab-pane active",
                                      id = "summary-metrics-yoy",
                                      fluidRow(
                                           column(4, sidebarPanel(width = "100%",
                                                                  h2("Summary Metrics yoy", class = "title_sidebar"),br(),
                                                                  shinyWidgets::pickerInput(inputId = "SumMetricMeas", label = "Select Measure", choices = unique(totalOutcomes$Measure)[!unique(totalOutcomes$Measure) %in% c("NetRevenueNewCustomers")], selected = "NetRevenue"),
                                                                  shinyWidgets::pickerInput(inputId = "SumMetricYear", label = "Select Years", choices = as.vector(unique(totalOutcomes$Year)), selected = as.vector(unique(totalOutcomes$Year)), multiple = TRUE,
                                                                                            options = shinyWidgets::pickerOptions(actionsBox = T, dropdownAlignRight = T)),
                                                                  br(),
                                                                  
                                                                  p("• View YoY monthly performance by selected KPI", class = "title_sidebar", style = "font-size:14px; line-height:1.5;"),
                                                                  p("• Earlier years available in filter", class = "title_sidebar", style = "font-size:14px; margin-bottom:-30px;")
                                                                  
                                           ))),
                                      br(),
                                      fluidRow(
                                           column(11,
                                                  highchartOutput("totalOutcomes", height = "450px") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4),
                                                  br(),br(),br(),br(),
                                           ))
                                      
                                      
                                      
                                 ),
                                 tags$div(
                                      class = "tab-pane",
                                      id = "category-economics-summary",
                                      fluidRow(
                                           column(4, sidebarPanel(width = "100%", h2("Category Economics Summary", class = "title_sidebar"),
                                                                  br(),
                                                                  shinyWidgets::pickerInput(inputId = "categoryEconAtt", label = "Select Attribute", choices = atts, selected = atts[1]),
                                                                  shinyWidgets::pickerInput(inputId = "categoryEconYear", label = "Select Cohort", choices = (eval(parse(text = attSummaryDf)) %>% distinct(Year) %>% arrange(desc(Year)) %>% pull), selected = (eval(parse(text = attSummaryDf)) %>% distinct(Year) %>% arrange(desc(Year)) %>% pull)[1]),
                                                                  br(),
                                                                  p("Repeat Metrics Summary by Attribute Value",class = "title_sidebar", style = "font-size:14px; margin-bottom:-20px; line-height:1.5;")
                                                                  
                                           ))
                                      ),br(),br(),
                                      fluidRow( 
                                           column(12,reactableOutput("categoryEcon") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4)),
                                           
                                      ),
                                      br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                 ),
                                 tags$div(
                                      class = "tab-pane",
                                      id = "cohort-financial-contribution",
                                      fluidRow( 
                                           column(4,
                                                  sidebarPanel(width = "100%", h2("Cohort Financial Contribution", class = "title_sidebar"),
                                                               br(),
                                                               shinyWidgets::pickerInput(inputId = "CohortFinMetric", label = "Select Metric", choices = unique(CohortPeriodContribution$Metric), selected = "Revenue"),
                                                               shinyWidgets::radioGroupButtons(
                                                                    inputId = "CohortFinChartType",
                                                                    label = "Select Chart Type",
                                                                    choices = c("Total", "% of Total"),
                                                                    checkIcon = list(
                                                                         yes = tags$i(class = "fa fa-circle", 
                                                                                      style = "color: steelblue"),
                                                                         no = tags$i(class = "fa fa-circle-o", 
                                                                                     style = "color: steelblue")),
                                                                    individual = T
                                                               ),br(),br(),
                                                               p("Amount of in-period revenue or margin contributed by each cohort",class = "title_sidebar", style = "font-size:14px; margin-bottom:-20px; line-height:1.5;")
                                                               
                                                  )
                                           )
                                      ),
                                      br(),br(),
                                      fluidRow(
                                           column(11,highchartOutput("CohortFinancial") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4))
                                      ),br(),br(),
                                      fluidRow(
                                           column(11,highchartOutput("NewCustFinancial") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4))
                                      )    
                                 ),
                                 tags$div(
                                      class = "tab-pane",
                                      id = "repeat-revenue-trend",
                                      fluidRow(
                                           column(width = 4,
                                                  sidebarPanel(width = "100%",
                                                               h2("Repeat Revenue Trend", class = "title_sidebar"),
                                                               br(),
                                                               shinyWidgets::pickerInput(inputId = "unitRevenueYears", label = "Years to Include", choices = unique(year(d$Cohort)), selected = unique(year(d$Cohort)), multiple = TRUE,
                                                                                         options = shinyWidgets::pickerOptions(actionsBox = T, dropdownAlignRight = T)),
                                                               br(),
                                                               p("Revenue Per Customer Trend by Period",class = "title_sidebar", style = "font-size:14px; margin-bottom:-20px; line-height:1.5;")
                                                  ))
                                      ), br(),br(),
                                      
                                      fluidRow(
                                           column(width = 6, highchartOutput("unitRevenueTrend3") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4)),
                                           column(width = 6, highchartOutput("unitRevenueTrend6") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4))
                                      ),
                                      br(),
                                      fluidRow(
                                           column(width = 6,highchartOutput("unitRevenueTrend12") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4)),
                                           column(width = 6, highchartOutput("unitRevenueTrend18") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4))
                                      ),
                                      br(),
                                      fluidRow(
                                           column(width = 6, highchartOutput("unitRevenueTrend24") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4)),
                                           column(width = 6,highchartOutput("unitRevenueTrend36") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4))
                                      )
                                      
                                      
                                      
                                 ),
                                 tags$div(
                                      class = "tab-pane",
                                      id = "Repeat-Frequency",
                                      fluidRow(
                                           column(width = 4,
                                                  sidebarPanel(width = "100%",
                                                               h2("Repeat Frequency", class = "title_sidebar"),
                                                               br(),
                                                               shinyWidgets::pickerInput(inputId = "totOrdersCompyears",label = "Select Years ", choices = unique(year(repeatFrequency$Cohort)), selected = 2019:2023, multiple = T),
                                                               shinyWidgets::pickerInput(inputId = "periodwindow",label= "Select Window",choices= c("3","6","12","24","36"),selected = "12",multiple = F),
                                                               br(),
                                                               p("Distribution of Repeat Orders by Cohort",class = "title_sidebar", style = "font-size:14px; margin-bottom:-20px; line-height:1.5;")
                                                  ))
                                      ),br(),br(),
                                      fluidRow(
                                           column(11,highchartOutput("repeatFrequency")%>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4)
                                           )),
                                      fluidRow(
                                           column(11,highchartOutput("cumLTVDist") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4)
                                           )
                                      )
                                 ),
                                 tags$div(
                                      class = "tab-pane",
                                      id = "repeat-revenue-trend",
                                      "repeat revenue trend"
                                 ),
                                 tags$div(
                                      class = "tab-pane",
                                      id = "Geography",
                                      fluidRow(
                                           column(3,
                                                  sidebarPanel(width = "100%",
                                                       uiOutput("geoMapYears"),
                                                       shinyWidgets::radioGroupButtons(
                                                            inputId = "geoMapOutcome",
                                                            label = "Select Metric",
                                                            choices = c("PerCapita","Total"),
                                                            selected = "PerCapita",
                                                            checkIcon = list(
                                                                 yes = tags$i(class = "fa fa-circle", 
                                                                              style = "color: steelblue"),
                                                                 no = tags$i(class = "fa fa-circle-o", 
                                                                             style = "color: steelblue")),
                                                            individual = T
                                                       ),br(),
                                                       p("Revenue per capita by state or county",class = "title_sidebar", style = "font-size:14px; line-height:1.5;"),
                                                       p("Broken into 5 quantiles; coded by color",class = "title_sidebar", style = "font-size:14px; margin-bottom:-20px; line-height:1.5;")
                                                  )),
                                           column(9,
                                                  shiny::tags$head(shiny::tags$style("section.content { overflow-y: scroll;}")),
                                                  uiOutput("geoMapText"),
                                                  infoBoxOutput("infoBoxOutput"),
                                                  plotOutput("geoMap") %>% 
                                                       withSpinner(size = 1.1, color = "#ff8c04", type = 4),
                                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                           )
                                      ),br(),br(),br(),
                                 )
                            )
                            
                            
                       )
              )
     ),
     
     tags$script(src="test2.js"),
     # tags$script(src="jquery.min.js"),
     tags$script(src="preloader-main.js")
)
server <- function(input, output) {
     
     
     # Summary Metrics YoY ################################
     
     totalOutcomesReact <- reactive({
          totalOutcomes %>% filter(Measure == input$SumMetricMeas) %>% filter(Year %in% input$SumMetricYear)
     })
     
     totalOutcomesReactPlot <- reactive({
          Sys.sleep(2.35)
          df <- totalOutcomesReact() %>%
               arrange(Year)%>% group_by(Year) %>% mutate(MOMChange= percent(Amount/lag(Amount)-1,0.1))  %>% arrange(Month) %>% group_by(Month) %>% mutate(YOYChange = percent(Amount/lag(Amount)-1,0.1)) %>% ungroup
          if(input$SumMetricMeas %in% c("NewCustomers", "orders", "itemsPerOrder", "customers_last_30", "customers_last_60", "customers_last_90")) {  ## !! MAY NEED TO CUSTOM WRITE LIST OF DOLLAR, COMMA VARS !!
               hchart(df, "line",  hcaes(x = Month, y = Amount, group = Year, color = Year, 
                                         text = paste0("<b>Year</b>: ", Year, "<br>", "MOM%: ", MOMChange, "<br>", "YOY%: ", YOYChange, "<br>", "Amount: ", comma(Amount, accuracy = 1)))) %>%
                    hc_xAxis(categories = unique(df$Month), 
                             title = list(text = "Month", margin = 10, style = list(fontSize = "16px", fontWeight = "bold"))) %>%
                    
                    hc_yAxis(title = list(text = input$SumMetricMeas, margin = 10, style = list(fontSize = "16px", fontWeight = "bold"), align = "middle")
                    ) %>%
                    hc_tooltip(useHTML = TRUE, 
                               headerFormat = "<span style='font-size: 14px'>{point.key}</span><br/>", 
                               pointFormat = "{point.text}", valueDecimals = 1) %>%
                    hc_plotOptions(series = list(lineWidth = 3, marker = list(enabled = FALSE))) %>% 
                    hc_add_theme(highchart_theme) %>%
                    hc_title(text = "Monthly Sales by Year", style = list(fontSize = "24px", fontWeight = "bold")) %>%
                    hc_subtitle(text = "2018-2022", style = list(fontSize = "18px")) %>%
                    hc_legend(enabled = TRUE, group = totalOutcomes$Year,
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "Year",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 0, borderColor = "#fff",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0)
               
               
          } else if (input$SumMetricMeas %in% c("itemsPerOrder")) {
               hchart(df, "line",  hcaes(x = Month, y = Amount, group = Year, color = Year, text = paste(sep="\n", paste0("<b>Year</b>:", Year), paste0("<b>MOM%</b>:",MOMChange), paste0("<b>YoY%:</b>", YOYChange),paste0("<b>Amount</b>:",Amount)))) %>%
                    hc_xAxis(categories = unique(df$Month), 
                             title = list(text = "Month", margin = 10, style = list(fontSize = "16px", fontWeight = "bold"))) %>%
                    
                    hc_yAxis(title = list(text = input$SumMetricMeas, margin = 10, style = list(fontSize = "16px", fontWeight = "bold"), align = "middle"), 
                             labels = list(
                                  formatter = JS("function() { return Highcharts.numberFormat(this.value, 0) + ' $'; }")
                             )) %>%
                    hc_tooltip(useHTML = TRUE, 
                               headerFormat = "<span style='font-size: 14px'>{point.key}</span><br/>", 
                               pointFormat = "{point.text}",             valueDecimals = 1) %>%
                    hc_plotOptions(series = list(lineWidth = 3, marker = list(enabled = FALSE))) %>% 
                    hc_add_theme(highchart_theme) %>%
                    hc_title(text = "Monthly Sales by Year", style = list(fontSize = "24px", fontWeight = "bold")) %>%
                    hc_subtitle(text = "2018-2022", style = list(fontSize = "18px")) %>%
                    hc_legend(enabled = TRUE, group = totalOutcomes$Year,
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "Year",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 1.5, borderColor = "#ff8c04",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0) 
          } else {
               hchart(df, "line",  hcaes(x = Month, y = Amount, group = Year, color = Year, 
                                         text = paste0("<b>Year</b>: ", Year, "<br>", "<b>MOM%</b>: ", MOMChange, "<br>", "<b>YOY%</b>: ", YOYChange, "<br>", "<b>Amount</b>: ", comma(Amount, accuracy = 1)))) %>%
                    hc_xAxis(categories = unique(df$Month), 
                             title = list(text = "Month", margin = 10, style = list(fontSize = "16px", fontWeight = "bold"))) %>%
                    
                    hc_yAxis(title = list(text = input$SumMetricMeas, margin = 10, style = list(fontSize = "16px", fontWeight = "bold"), align = "middle"), 
                             labels = list(
                                  formatter = JS("function() { return Highcharts.numberFormat(this.value, 0) + ' $'; }")
                             )) %>%
                    hc_tooltip(useHTML = TRUE, 
                               headerFormat = "<span style='font-size: 14px'>{point.key}</span><br/>", 
                               pointFormat = "{point.text}",             valueDecimals = 1) %>%
                    hc_plotOptions(series = list(lineWidth = 3, marker = list(enabled = FALSE))) %>% 
                    hc_add_theme(highchart_theme) %>%
                    hc_title(text = "Monthly Sales by Year", style = list(fontSize = "24px", fontWeight = "bold")) %>%
                    hc_legend(enabled = TRUE, group = totalOutcomes$Year,
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "Year",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 0, borderColor = "#ff8c04",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0)
          }
     })
     
     output$totalOutcomes <- renderHighchart({
          totalOutcomesReactPlot()
     })
     
     totalOutcomesYoYReact <- reactive({
          df <- totalOutcomes %>% filter(Measure == input$SumMetricMeas) %>% filter(Year %in% input$SumMetricYear)
          minYear <- min(as.numeric(levels(df$Year)))  # still need this.  just filtering on output table
          df %>% arrange(Year, Month) %>% group_by(Month) %>% mutate(YoYChange = Amount/lag(Amount) -1) %>% select(-c(Measure,Amount)) %>% spread(Month, YoYChange) %>% filter(Year != minYear) %>% mutate_at(vars(-Year), funs(sprintf("%.0f%%", 100* .))) #%>% mutate_at(vars(-Year), funs(percent))
     })
     
     output$DnldTotalOutcomesPlot <- downloadHandler(
          filename = function() {
               paste0(input$SumMetricMeas, ".png")
          },
          content = function(file) {
               ggsave(file, plot = totalOutcomesReactPlot(), device = "png")
          }
     )
     
     
     output$DnldTotalOutcomesTable <- downloadHandler(
          filename = function() {
               paste0("SummaryMetrics", input$SumMetricMeas, ".csv")
          },
          content = function(file) {
               write.table(totalOutcomesReact(), file, sep = ",", row.names = FALSE)
          }
     )
     
     ## Cohort Financial Contribution ####
     CohortFinancialReact <- reactive({
          df <- CohortPeriodContribution %>% filter(Metric == input$CohortFinMetric)
          df
     })
     
     NewCustFinancialReact <- reactive({
          df <- newCustPeriodContribution %>% filter(Metric == input$CohortFinMetric)
          df
     })
     
     
     
     output$CohortFinancial <- renderHighchart({
          
          Sys.sleep(1)
          
          df <- CohortFinancialReact()
          # Convert the data to a format compatible with highcharter
          df_grouped <- df %>%
               group_by(FinancialPeriod, Cohort) %>%
               summarize(Amount = sum(Amount)) %>%
               ungroup()
          df_grouped$Pct <- df_grouped$Amount / ave(df_grouped$Amount, df_grouped$FinancialPeriod, FUN = sum)
          
          if(input$CohortFinChartType == "Total") {
               hchart(df_grouped, "column", hcaes(x = FinancialPeriod, y = Amount, group = Cohort)) %>%
                    hc_title(text = "Composition by Cohort",
                             style = list(color = "#333333", fontSize = "28px", fontWeight = "bold")) %>%
                    hc_yAxis(title = list(text = "Total",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "${value}",
                                           style = list(color = "#333333", fontSize = "14px"))) %>%
                    hc_xAxis(title = list(text = "Financial Period",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "{value}",
                                           style = list(color = "#333333", fontSize = "14px")),
                             reversed = TRUE) %>%
                    hc_plotOptions(column = list(stacking = "normal", borderRadius = 2)) %>%
                    hc_legend(enabled = TRUE, group = df_grouped$CustomerOrderNum,
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "Cohort",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 0, borderColor = "#ff8c04",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0) %>%
                    hc_add_theme(highchart_theme)    
          } else {
               hchart(df_grouped, "column", hcaes(x = FinancialPeriod, y = Pct*100, group = Cohort)) %>%
                    hc_title(text = "Composition by Cohort",
                             style = list(color = "#333333", fontSize = "28px", fontWeight = "bold")) %>%
                    hc_yAxis(title = list(text = "% of Total",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "{value}%",
                                           style = list(color = "#333333", fontSize = "14px")),
                             max = 100) %>%
                    hc_xAxis(title = list(text = "Financial Period",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "{value}",
                                           style = list(color = "#333333", fontSize = "14px")),
                             reversed = TRUE) %>%
                    hc_plotOptions(column = list(stacking = "percent", borderRadius = 2)) %>%
                    hc_legend(enabled = TRUE, 
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "Cohort",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 0, borderColor = "#ff8c04",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0) %>%
                    hc_tooltip(shared = TRUE, valueDecimals = 2, valueSuffix = "%") %>%
                    hc_add_theme(highchart_theme)
          }
     })
     output$NewCustFinancial <- renderHighchart({
          df <- NewCustFinancialReact()
          df_grouped <- df %>%
               group_by(FinancialPeriod, CustomerOrderNum) %>%
               summarize(Amount = sum(Amount)) %>%
               ungroup()
          df_grouped$Pct <- df_grouped$Amount / ave(df_grouped$Amount, df_grouped$FinancialPeriod, FUN = sum)
          
          if(input$CohortFinChartType == "Total") {
               
               hchart(df_grouped, "column", hcaes(x = FinancialPeriod, y = Amount, group = CustomerOrderNum)) %>%
                    hc_title(text = "Composition by CustomerOrderNum",
                             style = list(color = "#333333", fontSize = "28px", fontWeight = "bold")) %>%
                    hc_yAxis(title = list(text = "Total",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "${value}",
                                           style = list(color = "#333333", fontSize = "14px"))) %>%
                    hc_xAxis(title = list(text = "Financial Period",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "{value}",
                                           style = list(color = "#333333", fontSize = "14px"))) %>%
                    hc_plotOptions(column = list(stacking = "normal", borderRadius = 2)) %>%
                    hc_legend(enabled = TRUE, group = df_grouped$CustomerOrderNum,
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "CustomerOrderNum",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 0, borderColor = "#ff8c04",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0) %>%
                    hc_add_theme(highchart_theme)  
               
          } else {
               hchart(df_grouped, "column", hcaes(x = FinancialPeriod, y = 100*Pct, group = CustomerOrderNum)) %>%
                    hc_title(text = "Composition by Cohort",
                             style = list(color = "#333333", fontSize = "28px", fontWeight = "bold")) %>%
                    hc_yAxis(title = list(text = "% of Total",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "{value}%",
                                           style = list(color = "#333333", fontSize = "14px")),
                             max = 100) %>%
                    hc_xAxis(title = list(text = "Financial Period",
                                          style = list(color = "#333333", fontSize = "20px", fontWeight = "bold")),
                             labels = list(format = "{value}",
                                           style = list(color = "#333333", fontSize = "14px"))) %>%
                    hc_plotOptions(column = list(stacking = "percent", borderRadius = 2)) %>%
                    hc_legend(enabled = TRUE, 
                              itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                              title = list(text = "Cohort",
                                           style = list(color = "#333333", fontSize = "18px", fontWeight = "bold")),
                              borderWidth = 0, borderColor = "#ff8c04",
                              align = "right", verticalAlign = "middle", layout = "vertical",
                              floating = FALSE, x = 0, y = 0) %>%
                    hc_tooltip(shared = TRUE, valueDecimals = 2, valueSuffix = "%") %>%
                    hc_add_theme(highchart_theme)
          }
     })
     
     output$DnldCohortFinancialTable <- downloadHandler(
          filename = function() {
               paste0("CohortContribution", ".csv")
          },
          content = function(file) {
               write.table(CohortFinancialReact(), file, sep = ",", row.names = FALSE)
          }
     )
     
     output$DnldNewCustFinancialTable <- downloadHandler(
          filename = function() {
               paste0("NewRepeatContribution", ".csv")
          },
          content = function(file) {
               write.table(NewCustFinancialReact(), file, sep = ",", row.names = FALSE)
          }
     )
     
     
     ## Category Economics Summary ####
     output$categoryEcon <- renderReactable({
          Sys.sleep(1.3)
          att <- CapCamelCase(input$categoryEconAtt) # capitalize first letter of string
          d <- eval(parse(text = paste0("attSummary", att)))
          d <- d %>%
               filter(Year == input$categoryEconYear) %>%
               arrange(desc(Customers)) %>%
               mutate(Customers = round(Customers,0)) %>%
               rename(
                    `AOV First Order` = AOVFirstOrder,
                    `Order Repeat Rate 12 Month` = OrderRepeatRate12Month,
                    `Order Repeat Rate` = OrderRepeatRate,
                    `Customer Repeat Rate 12 Month` = CustomerRepeatRate12Month,
                    `Customer Repeat Rate` = CustomerRepeatRate,
                    `LTV 12 Month` = LTV12Month,
                    `Cumulative LTV` = CumulativeLTV
               )
          reactable(
               d,
               defaultPageSize = 25,
               columns = list(
                    Customers = colDef(format = colFormat(separators = TRUE)),
                    `AOV First Order` = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                    `LTV 12 Month` = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                    `Cumulative LTV` = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                    `Order Repeat Rate 12 Month` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                    `Order Repeat Rate` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                    `Customer Repeat Rate 12 Month` = colDef(format = colFormat(percent = TRUE, digits = 1)),
                    `Customer Repeat Rate` = colDef(format = colFormat(percent = TRUE, digits = 1))
               ),
               bordered = FALSE,
               highlight = T,
               resizable = F,
               searchable = FALSE,
               wrap = TRUE,
               striped = TRUE,
               showSortable = TRUE,
               filterable =F
          )
     })
     
     
     
     # Repeat Revenue Trend ####
     
     unitRevenueTrendData <- reactive({
          d <- d %>%
               filter(year(Cohort) %in% input$unitRevenueYears) %>%
               group_by(Cohort, PeriodGrouped) %>%
               summarise(
                    PeriodMonthStart = max(PeriodMonthStart, na.rm = T),
                    unitRevenue = sum(unitRevenue, na.rm = T),
                    Customers = max(Customers, na.rm = T)
               ) %>%
               ungroup
          # will include incomplete periods
          d <- d %>%
               group_by(PeriodGrouped, PeriodMonthStart) %>%
               arrange(Cohort) %>%
               filter(row_number() == 1) %>%
               ungroup
          # d$PeriodGrouped <- recode_factor(d$PeriodGrouped, "(-Inf,3]"="0-3", "(3,6]"="4-6", "(6,12]"="7-12", "(12,18]"="13-18", "(18,24]"="19-24", "(24,36]"="25-36", "(36, Inf]"="36+")
          d$PeriodGrouped <- recode_factor(d$PeriodGrouped, "(-Inf,3]"="Month 3", "(3,6]"="Month 6", "(6,12]"="Month 12", "(12,18]"="Month 18", "(18,24]"="Month 24", "(24,36]"="Month 36", "(36, Inf]"="Month 36+")
          d
     })
     unitRevenueTrendPlot <- function(period) {
          data <- unitRevenueTrendData() %>%
               filter(PeriodGrouped == period)
          
          data$Customers <- data$Customers / 10
          hchart(data, "line", 
                 hcaes(x = PeriodMonthStart, y = round(unitRevenue,2)),
                 color = "#21130d",
                 zIndex = 1,
                 name = "Unit Revenue") %>%
               hc_yAxis_multiples(list(title = list(text = "Unit Revenue"), opposite = FALSE,
                                       style = list(fontFamily = "sans-serif", fontSize = "16px")),
                                  list(title = list(text = "Customers Hitting Period"), opposite = TRUE,
                                       labels = list(formatter = JS("function () {return Highcharts.numberFormat(this.value, 0, ',', ',');}")),
                                       style = list(fontFamily = "sans-serif", fontSize = "16px"))) %>%
               hc_xAxis(type = "datetime",
                        dateTimeLabelFormats = list(month = "%b %Y"),
                        labels = list(rotation = -45, align = "right",
                                      style = list(fontFamily = "sans-serif", fontSize = "14px")),
                        tickInterval = 90 * 24 * 3600 * 1000) %>%
               hc_add_series(data, "column",
                             hcaes(x = PeriodMonthStart, y = Customers, group = "Customers"),
                             color = "#ffa500",
                             fillOpacity = 0.5,
                             yAxis = 1,
                             name = "Customers/10") %>%
               hc_chart(height = 500) %>%
               hc_legend(enabled = FALSE) %>%
               hc_title(text = paste("Unit Revenue and Customers Hitting Period for", period),
                        style = list(fontFamily = "sans-serif", fontSize = "16px"))  %>%
               hc_add_theme(highchart_theme)  %>% 
               hc_chart(height = 400) %>% 
               hc_tooltip(shared = T)
     }
     
     output$unitRevenueTrend3 <- renderHighchart({
          unitRevenueTrendPlot(period = "Month 3")
     })
     
     output$unitRevenueTrend6 <- renderHighchart({
          unitRevenueTrendPlot(period = "Month 6")
     })
     
     output$unitRevenueTrend12 <- renderHighchart({
          unitRevenueTrendPlot(period = "Month 12")
     })
     
     output$unitRevenueTrend18 <- renderHighchart({
          unitRevenueTrendPlot(period = "Month 18")
     })
     output$unitRevenueTrend24 <- renderHighchart({
          unitRevenueTrendPlot(period = "Month 24")
     })
     
     output$unitRevenueTrend36 <- renderHighchart({
          unitRevenueTrendPlot(period = "Month 36")
     })
     
     
     
     ## Repeat Frequency ####
     totalcustomersreact <- reactive({
          repeatFrequency <- repeatFrequency %>%
               filter( year(Cohort) %in% input$totOrdersCompyears) %>% filter(window %in% input$periodwindow)
     })
     
     output$repeatFrequency <- renderHighchart({
          d1 <- totalcustomersreact() %>% 
               mutate(Cohort = factor(Cohort)) %>% 
               mutate(RepeatOrders = forcats::fct_rev(Orders))
          
          hchart(d1, "column", hcaes(x = Cohort, y = pct, group = RepeatOrders, 
                                     tooltip = paste("Cohort: {point.x}<br/>RepeatOrders: {point.group}<br/>Customer%: {point.y:.2%}"))) %>%
               hc_plotOptions(column = list(stacking = "normal", borderWidth = 0)) %>%
               hc_title(text = "% Total Customers", style = list(fontSize = "24px", fontFamily = "Helvetica Neue")) %>%
               hc_xAxis(title = list(text = "Cohort", style = list(fontSize = "18px", fontFamily = "Helvetica Neue")),
                        labels = list(style = list(fontSize = "16px", fontFamily = "Helvetica Neue"))) %>%
               hc_yAxis(title = list(text = "Customer Percentage", style = list(fontSize = "18px", fontFamily = "Helvetica Neue")),
                        labels = list(format = "{value}%", style = list(fontSize = "16px", fontFamily = "Helvetica Neue")))%>%
               hc_add_theme(highchart_theme) %>%
               hc_legend(enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "middle",
                         title = list(text = "RepeatOrders", style = list(fontSize = "18px", fontFamily = "Helvetica Neue")),
                         subtitle = list(text = "Number of repeat orders", style = list(fontSize = "14px", fontFamily = "Helvetica Neue")),
                         itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                         borderWidth = 0, borderColor = "#ff8c04",
                         floating = FALSE, x = 0, y = 0) %>% 
               hc_add_theme(highchart_theme)
          
          
          
     })
     
     cumLTVDistCustReact <- reactive({
          LTVDistribution <- LTVDistribution %>%
               filter( year(Cohort) %in% input$totOrdersCompyears) %>% filter(window %in% input$periodwindow)
     })
     
     output$cumLTVDist <- renderHighchart({
          
          d1 <- cumLTVDistCustReact() %>% 
               mutate(Cohort = factor(Cohort)) %>% 
               mutate(LTV = forcats::fct_rev(CumulativeLTV))
          
          hchart(d1, "column", hcaes(x = Cohort, y = pct, group = LTV, 
                                     tooltip = paste("Cohort: {point.x}<br/>Cumulative LTV: {point.group}<br/>% Total Customers: {point.y:.2%}"))) %>%
               hc_plotOptions(column = list(stacking = "normal", borderWidth = 0)) %>%
               hc_title(text = "% Total Customers by Cohort and Cumulative LTV", style = list(fontSize = "24px", fontFamily = "Helvetica Neue")) %>%
               hc_subtitle(text = "Source: Cumulative LTV Distribution by Reactivated Customers", style = list(fontSize = "16px", fontFamily = "Helvetica Neue")) %>%
               hc_xAxis(title = list(text = "Cohort", style = list(fontSize = "18px", fontFamily = "Helvetica Neue")),
                        labels = list(style = list(fontSize = "14px", fontFamily = "Helvetica Neue"))) %>%
               hc_yAxis(title = list(text = "% Total Customers", style = list(fontSize = "18px", fontFamily = "Helvetica Neue")),
                        labels = list(format = "{value}%", style = list(fontSize = "14px", fontFamily = "Helvetica Neue")))%>%
               hc_add_theme(highchart_theme) %>%
               hc_legend(enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "middle",
                         title = list(text = "Cumulative LTV", style = list(fontSize = "18px", fontFamily = "Helvetica Neue")),
                         itemStyle = list(color = "#333333", fontSize = "14px", fontWeight = "bold"),
                         borderWidth = 0, borderColor = "#ff8c04",
                         floating = FALSE, x = 0, y = 0)
     })
     
     
     
     
     
     
     
     
     
     ## Geography ####
     
     output$geoMapYears <- renderUI({
          Sys.sleep(0.5)
          choices <- geoSummaryState %>% ungroup(.) %>% distinct(Year) %>% filter(!is.na(Year))
          tagList(
               h2("Geography", class = "title_sidebar"), br(),
               sliderInput(inputId = "geoMapYear", label = "Select Year", min = min(choices), max = max(choices), value = c(min(choices), max(choices)), sep = "", ticks = FALSE)
          )
     })
     
     geoMapReact <- reactive({
          geoSummaryState %>%
               filter(Year %in% input$geoMapYear) %>%
               rename(Population = StatePopulation) %>%
               group_by(STATE_ABBREV, Population) %>%
               summarise(Revenue = sum(Revenue)) %>%
               ungroup %>%
               mutate(RevenuePerCapita = Revenue/Population)
     })
     
     # output$geoMapText <- renderUI({
     #      if(input$geoMapOutcome == "PerCapita"){
     #           topState <- geoMapReact() %>% 
     #                arrange(desc(RevenuePerCapita)) %>% 
     #                top_n(1)
     #           tagList(
     #                HTML(
     #                     paste0("The top State is: ", tags$strong(topState$STATE_ABBREV))
     #                ),br(),
     #                HTML(
     #                     paste0("Revenue Per Capita: ", tags$strong(topState$RevenuePerCapita))
     #                )
     #           )
     #           
     #      } else {
     #           topState <- geoMapReact() %>% 
     #                arrange(desc(Revenue)) %>% 
     #                top_n(1)
     #           tagList(
     #                HTML(
     #                     paste0("Top State: ", tags$strong(topState$STATE_ABBREV))
     #                ),br(),
     #                HTML(
     #                     paste0("Revenue: ", tags$strong(topState$Revenue))
     #                )
     #           )
     #           
     #      }
     # })
     
     output$geoMap <- renderPlot({
          req(ifelse(nrow(geoMapReact()>0),1,NA))
          rgb.colors.0 <- rgb(189, 195, 199, maxColorValue = 255) # `light grey` = "#bdc3c7"
          rgb.colors.1 <- rgb(127, 140, 141, maxColorValue = 255) # `dark grey`  = "#7f8c8d"
          rgb.colors.2 <- rgb(241, 196, 15, maxColorValue = 255) # `yellow`     = "#f1c40f"
          rgb.colors.3 <- rgb(243, 156, 18, maxColorValue = 255) # `orange`     = "#f39c12"
          rgb.colors.4 <- rgb(211, 84, 0, maxColorValue = 255) # `red`        = "#d35400"
          rgb.colors <- c(rgb.colors.0, rgb.colors.1, rgb.colors.2, rgb.colors.3, rgb.colors.4)
          
          
          df <- geoMapReact()
          
          if(input$geoMapOutcome == "PerCapita") {
               df$colorbuckets <- as.numeric(cut(df$RevenuePerCapita, breaks = quantile(df$RevenuePerCapita, seq(0, 1, 0.2))))
               leg.txt <- c("Quantile 1", "Quantile 2", "Quantile 3", "Quantile 4", "Quantile 5")
               
               st.fips <- state.fips$abb[match(map("state", plot=FALSE)$names,state.fips$polyname)]
               colorsmatched <- df$colorbuckets[match(st.fips, df$STATE_ABBREV)]
               # browser()
               map("state", fill = TRUE, col = rgb.colors[colorsmatched], lwd = 0.1) # lty = 0 removes borders
               # title("Revenue per Capita", cex.main = 1, col.main = "deepskyblue4") #http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
               # legend("bottomright", leg.txt, horiz = FALSE, fill = rgb.colors, cex=0.6,  inset = 0.15)
          } else {
               df$colorbuckets <- as.numeric(cut(df$Revenue, breaks = quantile(df$Revenue, seq(0, 1, 0.2))))
               leg.txt <- c("Quantile 1", "Quantile 2", "Quantile 3", "Quantile 4", "Quantile 5")
               
               st.fips <- state.fips$abb[match(map("state", plot=FALSE)$names,state.fips$polyname)]
               colorsmatched <- df$colorbuckets[match(st.fips, df$STATE_ABBREV)]
               
               map("state", fill = TRUE, col = rgb.colors[colorsmatched], lwd = 0.1) # lty = 0 removes borders
               title("Revenue", cex.main = 1, col.main = "deepskyblue4") #http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
               legend("bottomright", leg.txt, horiz = FALSE, fill = rgb.colors, cex=0.6,  inset = 0.15)
          }
     },
     height = 600, width = 800
     )
     
     output$infoBoxOutput <- renderInfoBox({
          if(input$geoMapOutcome == "PerCapita"){
               topState <- geoMapReact() %>% 
                    arrange(desc(RevenuePerCapita)) %>% 
                    top_n(1)
               infoBox("Revenue Per Capita", round(topState$RevenuePerCapita,5), HTML(
                    paste0("The top State is: ", tags$strong(topState$STATE_ABBREV))
               ), color = "orange")
               
          } else {
               topState <- geoMapReact() %>% 
                    arrange(desc(Revenue)) %>% 
                    top_n(1)
               infoBox("Revenue", round(topState$Revenue,2), HTML(
                    paste0("The top State is: ", tags$strong(topState$STATE_ABBREV))
               ), color = "orange")
          }
     })
}

shinyApp(ui, server)
