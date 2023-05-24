library(shiny)
library(shinyjs)

ui <- fluidPage(
     useShinyjs(),
     includeCSS("www/mycss.css"),
     tags$div(class = "wrapper",
              tags$div(class = "container",
                       tags$div(class = "content-wrapper",
                                tags$aside(class = "aside",
                                           tags$nav(class = "aside__nav",
                                                    tags$ul(class = "aside__items",
                                                            `data-spollers` = "", `data-one-spoller` = "",
                                                            tags$li(class = "aside__item",
                                                                    tags$button(type = "button", class = "aside__nav-btn _spoller-active", `data-spoller` = "", "Summary metrics"),
                                                                    tags$ul(class = "aside__submenu",
                                                                            tags$li( 
                                                                                 tags$ul(
                                                                                      tags$button(type = "button",class = "aside__nav-btn", 
                                                                                                  `data-toggle` = "tab", href="#summary-metrics-yoy",
                                                                                                  id = "summary-metrics-yoy", "summary metrics yoy", tags$i("→")
                                                                                      )
                                                                                 )
                                                                            )
                                                                    )
                                                            ),
                                                            tags$li(class = "aside__item",
                                                                    tags$button(type = "button", class = "aside__nav-btn", `data-spoller` = "", "Cohort Analysis"),
                                                                    tags$ul(class = "aside__submenu", hidden = "",
                                                                            tags$li( 
                                                                                 tags$ul(
                                                                                      tags$button(type = "button",class = "aside__nav-btn", 
                                                                                                  `data-toggle` = "tab", href="#category-economics-summary",
                                                                                                  id = "category-economics-summary", "Category Economics Summary", tags$i("→")
                                                                                      )
                                                                                 )
                                                                            ),
                                                                            tags$li(
                                                                                 tags$ul(
                                                                                      tags$button(type = "button",class = "aside__nav-btn",
                                                                                                  `data-toggle` = "tab", href="#cohort-financial-contribution",
                                                                                                  id = "cohort-financial-contribution", "Cohort Financial Contribution", tags$i("→")
                                                                                      )
                                                                                 )
                                                                            ),
                                                                            tags$li( 
                                                                                 tags$ul(
                                                                                      tags$button(type = "button",class = "aside__nav-btn", 
                                                                                                  `data-toggle` = "tab", href="#repeat-revenue-trend",
                                                                                                  id = "repeat-revenue-trend", "Repeat Revenue trend", tags$i("→")
                                                                                      )
                                                                                 )
                                                                            )
                                                                    )
                                                            )
                                                    )
                                           ),
                                           tags$div(id = "page-content")
                                )
                                ,
                                div(class= "tab-content",
                                    tags$div(
                                         class = "tab-pane active",
                                         id = "summary-metrics-yoy",
                                         "summary metrics yoy"
                                    ),
                                    tags$div(
                                         class = "tab-pane",
                                         id = "category-economics-summary",
                                         "category economics summary"
                                    ),
                                    tags$div(
                                         class = "tab-pane",
                                         id = "cohort-financial-contribution",
                                         "cohort financial contribution"
                                    ),
                                    tags$div(
                                         class = "tab-pane",
                                         id = "repeat-revenue-trend",
                                         "repeat revenue trend"
                                    )
                                )
                       )
              )
     ),
     tags$script(src="test.js")
)

server <- function(input, output) {
     
}

shinyApp(ui, server)
