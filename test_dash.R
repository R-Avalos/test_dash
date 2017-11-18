# Test Dashabord light framework
library(shiny)
library(shinydashboard)
source("churn.r")

#### Dashboard User Inferface ####
ui <- dashboardPage(
        skin = "blue",
        dashboardHeader(title = "Zoom"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem(text = "Customer Churn",
                                 tabName= "churn"),
                        selectInput(inputId = "cohort", 
                                    label = "Select Cohort", 
                                    choices = c("Location", "Business"), 
                                    selected = "Location")
                        )
                ),
        dashboardBody(
                tabItems(
#### All Courses Tab ####
                        tabItem(tabName = "churn",
                                fluidRow(
                                        column(width = 6,
                                               HTML("<b>Monthly Customer Churn, </b><span style='color:red;'>Total Churn</span><span style='color:grey;'> - </span><span style='color:blue;'>Involuntary Churn</span>"),
                                               plotlyOutput(outputId = "churn")
                                               ),
                                        column(width = 6,
                                               HTML("<b>Revenue Lost to Churn</b>"),
                                               plotlyOutput(outputId = "lost_rev")
                                               )
                                        ),
                                fluidRow(
                                        column(width = 3,
                                               HTML("<b>30 Day Cohort Breakdown</b><br><span style='color:red;'>Churn</span><span style='color:grey;'> - </span><span style='color:black;'>Lost Rev</span>"),
                                               plotlyOutput(outputId = "cohort_test")
                                               ),
                                        column(width = 6,
                                               HTML("<b>Churned Difference from Retained</b><br>Churned Difference from Median Retained Account"),
                                               plotlyOutput(outputId = "health")),
                                        column(width = 3,
                                               HTML("<b>Top 5 Lost Accounts</b>"),
                                               tableOutput("top5")
                                               )
                                        )
                                )
                        )
                )
        )


#### Server #####
server <- function(input,output){
        output$top5 <- renderTable(top5, spacing = "xs")
        
        #### Reactive Cohort Plot
        reactive_cohort_df <- reactive({
                if(input$cohort == "Location"){
                        return(cohort_local)
                } else
                {
                        return(cohort_biz)
                }
                # race_results %>% filter(Chapter == input$chapter_select)
        })
        
        #### Test reactive
        ##### Business Cohort Plot
        output$cohort_test <- renderPlotly({
                data <- reactive_cohort_df()
                p <- plot_ly(data) %>%
                        add_trace(x = ~`Rev %`, y = ~Cohort,
                                  name = "Revenue",
                                  type = "bar",
                                  orientation = "h",
                                  marker = list(color = "rgba(58, 71, 80, 0.25)")) %>%
                        add_trace(x = ~`Churn %`, y = ~Cohort,
                                  name = "Churn",
                                  type = "bar",
                                  orientation = "h",
                                  marker = list(color = "rgba(255, 0, 0, 0.25)")) %>%
                        add_trace(x = ~`Churn %`, y = ~Cohort,
                                  type = "scatter",
                                  mode = "text", 
                                  text = paste0(data$`Churn %`, "%"),
                                  textposition = "top right",
                                  textfont = list(color = "rgba(255, 0, 0, 1)",
                                                  family = "sans serif",
                                                  size = 14)
                        ) %>%
                        add_trace(x = ~`Rev %`, y = ~Cohort,
                                  type = "scatter",
                                  mode = "text", 
                                  text = paste0(data$`Rev %`, "%"),
                                  textposition = "bottom right",
                                  textfont = list(color = "rgba(58, 71, 80, 1)",
                                                  family = "sans serif",
                                                  size = 14)
                        ) %>%
                        layout(barmode = 'overlay',
                               titlefont = list(size = 12),
                               height = 200,
                               margin = list(l = 100, r = 0, b = 0, t = 0, pad = 2),
                               paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               showlegend = FALSE,
                               xaxis = list(title = "",
                                            title = "",
                                            tickmode = "array",
                                            ticksuffix = "%",
                                            type = "marker",
                                            tickfont = list(family = "serif", size = 14),
                                            ticks = "outside",
                                            zeroline = FALSE,
                                            range = c(0, max(data$`Rev %`)+15)
                                            ),
                               yaxis = list(title ="",
                                            tickfont = list(family = "serif", size = 14
                                            )
                               )
                        )
                print(p)
        })
        #####
        
        ### Churned Healthbar
        output$health <- renderPlotly({
                plot_ly(diff_df) %>%
                        add_trace(x = ~cohort_attributes, y = ~diff_avg_churned,
                                  name = "churned",
                                  type = "bar",
                                  marker = list(color = "rgba(255, 0, 0, 0.5)")) %>%
                        layout(paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               height = 200,
                               showlegend = FALSE,
                               xaxis = list(title = "",
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            tickangle = 0,
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside",
                                            zeroline = FALSE),
                               yaxis = list(title ="",
                                            ticksuffix = "%",
                                            tickfont = list(family = "serif", size = 14)
                               )
                        )
        })
        
        ### Churn Lost Revenue
        output$lost_rev <- renderPlotly({
                plot_ly(lost_rev, x = ~date) %>%
                        add_trace(y = ~monthly_rev_lost, 
                                  type = "scatter",
                                  mode = "lines",
                                  line = list(color = "red"),
                                  hoverinfo = 'text',
                                  text = ~paste0("<span style='color:grey'>Revenue Lost to Churn </span><b>$",
                                                 prettyNum(monthly_rev_lost,big.mark = ","),
                                                 "</b></br>",
                                                 "</br>",
                                                 "<span style='color:grey'>Date </span>",
                                                 date
                                  )
                        ) %>%
                        layout(title = "",
                               paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               margin = list(r = 20),
                               hoverlabel = list(font = list(color = "black"),
                                                 bgcolor = "white",
                                                 bordercolor = "white"),
                               showlegend = FALSE,
                               xaxis = list(showgrid = FALSE,
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            autorange = TRUE,
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside"
                               ),
                               yaxis = list(showgrid = FALSE,
                                            range = c(0, max(lost_rev$monthly_rev_lost)+200),
                                            title = "",
                                            tickmode = "array",
                                            tickpreffix = "$",
                                            type = "marker",
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside",
                                            zeroline = FALSE
                               )
                        )
                
        })
        #####
        
        ### Churn Timeline 
        output$churn <- renderPlotly({
                plot_ly(monthly_churn, x = ~date) %>%
                add_trace(y = ~total_churn, 
                          type = "scatter",
                          mode = "lines",
                          line = list(color = "red"),
                          hoverinfo = 'text',
                          text = ~paste0("<span style='color:grey'>Total Churn Rate </span><b>",
                                         total_churn,
                                         "</b>%</br>",
                                         "</br>",
                                         "<span style='color:grey'>Date </span>",
                                         date
                          )
                ) %>%
                add_trace(y = ~involuntary_churn, name = "Involuntary Churn",
                          type = "scatter",
                          mode = "lines",
                          line = list(color = "blue"),
                          text = ~paste0("<span style='color:grey'>Involuntary Churn </span><b>",
                                         involuntary_churn,
                                         "</b>%</br>"
                          )
                ) %>%
                layout(title = "",
                       paper_bgcolor = "transparent",
                       plot_bgcolor = "transparent",
                       margin = list(r = 20),
                       hoverlabel = list(font = list(color = "black"),
                                         bgcolor = "white",
                                         bordercolor = "white"),
                       showlegend = FALSE,
                       xaxis = list(showgrid = FALSE,
                                    title = "",
                                    tickmode = "array",
                                    type = "marker",
                                    autorange = TRUE,
                                    tickfont = list(family = "serif", size = 10),
                                    ticks = "outside"
                       ),
                       yaxis = list(showgrid = FALSE,
                                    range = c(0, max(monthly_churn$total_churn)+1),
                                    title = "",
                                    tickmode = "array",
                                    ticksuffix = "%",
                                    type = "marker",
                                    tickfont = list(family = "serif", size = 10),
                                    ticks = "outside",
                                    zeroline = FALSE
                       ),
                       annotations = list(
                                 x = max(monthly_churn$date),
                                 y = last(monthly_churn$total_churn),
                                 text = paste0(last(monthly_churn$total_churn), "%"),
                                 xref = "x",
                                 yref = "y",
                                 showarrow = FALSE,
                                 ax = 20,
                                 ay = 20,
                                 xanchor = 'left',
                                 font = list(color = 'red',
                                             family = 'sans serif',
                                             size = 14)
                                 )
                       )
        })
}

shinyApp(ui, server) #preview dashboard
