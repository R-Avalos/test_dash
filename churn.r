# Churn Data
library(lubridate)
library(dplyr)
library(plotly)

date <- c("2017-1-1", "2017-2-1", "2017-3-1", "2017-4-1", "2017-5-1", "2017-6-1", "2017-7-1", "2017-8-1", "2017-9-1", "2017-10-1", "2017-11-1", "2017-12-1")
total_churn <- c(3, 3, 3.3, 3.2, 3.1, 2.8, 2.9, 3, 2.7, 2.3, 2.8, 2.5)
involuntary_churn <- c(1.3, 1, .3, .25, .75, .8, .9, 1.3, .7, .5, .4, .6)
monthly_churn <- data.frame(date, total_churn, involuntary_churn)
monthly_churn$date <- ymd(monthly_churn$date)


### Lost Revenue Data
monthly_rev_lost <- c(3000, 15000, 8000, 7000, 9000, 6000, 5000, 7500, 6450, 5550, 4000, 3850)
lost_rev <- data.frame(date, monthly_rev_lost)
lost_rev$date <- ymd(lost_rev$date)

### Cohort Data
business_type <- c("Enterprise", "Education", "Med Business", "Small Business", "Individual")
business_churn <- c(3, 1.25, 2, 4.25, 5.5)
business_revenue <- c(250, 200, 120, 42, 10)
cohort_biz <- data.frame(business_type, business_churn, business_revenue)
cohort_biz$per_rev <- round(cohort_biz$business_revenue/sum(cohort_biz$business_revenue)*100,2)
cohort_biz$business_type <- factor(cohort_biz$business_type, levels = unique(cohort_biz$business_type)[order(cohort_biz$per_rev, decreasing = FALSE)])
colnames(cohort_biz) <- c("Cohort", "Churn %", "Rev mil", "Rev %")

location <- c("Americas", "Europe", "China", "Pacific", "India")
location_churn <- c(2.00, 5.25, 4.00, 3.15, 2.5)
location_rev <- c(240, 140, 100, 80, 62)
cohort_local <- data.frame(location, location_churn, location_rev)
cohort_local$per_rev <- round(cohort_local$location_rev/sum(cohort_local$location_rev)*100,2)
cohort_local$location <- factor(cohort_local$location, levels = unique(cohort_local$location)[order(cohort_local$per_rev, decreasing = FALSE)])
colnames(cohort_local) <- c("Cohort", "Churn %", "Rev mil", "Rev %")


### Top 5 Churned
churn_name <- c("Kitten Armada Co.", "Llama Farm", "Shady ICO", "British Tacos", "Drone Coffee Delivery")
churn_rev <- c(900, 500, 400, 150, 90)
churn_users <- c(600, 40, 100, 4, 25)
# churn_age_months <- c(36, 30, 2, 1, 8)
top5 <- data.frame(churn_name, churn_rev, churn_users)
colnames(top5) <- c("Company", "Revenue", "Users")

#### How are churned customers different from retained?

cohort_attributes <- c("ARPU", "Users", "Age", "MAU", "DAU", "Sticky", "<5min Meet", "Faults", "Ticket")
diff_avg_churned <- c(1.25, -5, -10, -5, -25, -35, 5, 16, 5)
diff_df <- data.frame(cohort_attributes, diff_avg_churned)
diff_df$cohort_attributes <- factor(diff_df$cohort_attributes, levels = unique(diff_df$cohort_attributes)[order(diff_df$diff_avg_churned, decreasing = FALSE)])

# ### Barplot Diff ####
# plot_ly(diff_df) %>%
#         add_trace(x = ~cohort_attributes, y = ~diff_avg_churned,
#                   name = "churned",
#                   type = "bar",
#                   marker = list(color = "rgba(255, 0, 0, 0.5)")) %>%
#         layout(paper_bgcolor = "transparent",
#                plot_bgcolor = "transparent",
#                showlegend = FALSE,
#                xaxis = list(title = "",
#                             title = "",
#                             tickmode = "array",
#                             type = "marker",
#                             tickfont = list(family = "serif", size = 14),
#                             ticks = "outside",
#                             zeroline = FALSE),
#                yaxis = list(title ="",
#                             ticksuffix = "%",
#                             tickfont = list(family = "serif", size = 14)
#                ),
#                annotations = list(
#                        list(xref = "x", yref = "y",
#                             x = 2.5, 
#                             y = max(diff_df$diff_avg_churned),
#                             text = "<b>Churned Difference</b><br><span style='color:black;'>Churned difference from Median Retained Accounts</span>",
#                             showarrow = FALSE,
#                             align = "left")
#                )
#         )
#         
# 
# 
# 
# ### Barplot biz type ####
# plot_ly(cohort_biz) %>%
#         add_trace(x = ~`Rev %`, y = ~Cohort,
#                   name = "Revenue",
#                   type = "bar",
#                   orientation = "h",
#                   marker = list(color = "rgba(58, 71, 80, 0.25)")) %>%
#         add_trace(x = ~`Churn %`, y = ~Cohort,
#                   name = "Churn",
#                   type = "bar",
#                   orientation = "h",
#                   marker = list(color = "rgba(255, 0, 0, 0.5)")) %>%
#         add_trace(x = ~`Churn %`, y = ~Cohort,
#                   type = "scatter",
#                   mode = "text", 
#                   text = paste0(cohort_biz$`Churn %`, "%"),
#                   textposition = "right",
#                   textfont = list(color = "rgba(255, 0, 0, 1)",
#                                   family = "sans serif",
#                                   size = 14)
#         ) %>%
#         add_trace(x = ~`Rev %`, y = ~Cohort,
#                   type = "scatter",
#                   mode = "text", 
#                   text = ~`Rev %`,
#                   textposition = "right",
#                   textfont = list(color = "rgba(58, 71, 80, 1)",
#                                   family = "sans serif",
#                                   size = 14)
#         ) %>%
#         layout(barmode = 'overlay',
#                paper_bgcolor = "transparent",
#                plot_bgcolor = "transparent",
#                showlegend = FALSE,
#                xaxis = list(title = "",
#                             title = "",
#                             tickmode = "array",
#                             ticksuffix = "%",
#                             type = "marker",
#                             tickfont = list(family = "serif", size = 14),
#                             ticks = "outside",
#                             zeroline = FALSE),
#                yaxis = list(title ="",
#                             tickfont = list(family = "serif", size = 14)
#                ),
#                annotations = list(
#                        list(xref = "x", yref = "y",
#                             x = max(cohort_local$`Rev %`), 
#                             y = 1,
#                             text = "<b>Cohort Breakdown</b><br><span style='color:red;'>30 Day Churn %</span><br><span style='color:black;'>Total Revenue %</span>",
#                             showarrow = FALSE,
#                             align = "right")
#                )
#         )
# #####
# 
# ### Lost Revenue Plot ####
# plot_ly(lost_rev, x = ~date) %>%
#         add_trace(y = ~monthly_rev_lost, 
#                   type = "scatter",
#                   mode = "lines",
#                   line = list(color = "red"),
#                   hoverinfo = 'text',
#                   text = ~paste0("<span style='color:grey'>Revenue Lost to Churn </span><b>$",
#                                  prettyNum(monthly_rev_lost,big.mark = ","),
#                                  "</b></br>",
#                                  "</br>",
#                                  "<span style='color:grey'>Date </span>",
#                                  date
#                   )
#         ) %>%
#         layout(title = "",
#                paper_bgcolor = "transparent",
#                plot_bgcolor = "transparent",
#                margin = list(r = 20),
#                hoverlabel = list(font = list(color = "black"),
#                                  bgcolor = "white",
#                                  bordercolor = "white"),
#                showlegend = FALSE,
#                xaxis = list(showgrid = FALSE,
#                             title = "",
#                             tickmode = "array",
#                             type = "marker",
#                             autorange = TRUE,
#                             tickfont = list(family = "serif", size = 10),
#                             ticks = "outside"
#                ),
#                yaxis = list(showgrid = FALSE,
#                             range = c(0, max(lost_rev$monthly_rev_lost)+200),
#                             title = "",
#                             tickmode = "array",
#                             tickpreffix = "$",
#                             type = "marker",
#                             tickfont = list(family = "serif", size = 10),
#                             ticks = "outside",
#                             zeroline = FALSE
#                ),
#                annotations = list(
#                        list(xref = "x", yref = "y",
#                             x = min(lost_rev$date) + 30,
#                             y = max(lost_rev$monthly_rev_lost) + 100,
#                             text = "<b>Revenue Lost to Churn</b>",
#                             showarrow = FALSE,
#                             align = "left")
#                )
#         )
# 
# 
# 
# ### Churn Plot #####
# plot_ly(monthly_churn, x = ~date) %>%
#         add_trace(y = ~total_churn, 
#                   type = "scatter",
#                   mode = "lines",
#                   line = list(color = "red"),
#                   hoverinfo = 'text',
#                   text = ~paste0("<span style='color:grey'>Total Churn Rate </span><b>",
#                                  total_churn,
#                                  "</b>%</br>",
#                                  "</br>",
#                                  "<span style='color:grey'>Date </span>",
#                                  date
#                   )
#         ) %>%
#         add_trace(y = ~involuntary_churn, name = "Involuntary Churn",
#                   type = "scatter",
#                   mode = "lines",
#                   line = list(color = "blue"),
#                   text = ~paste0("<span style='color:grey'>Involuntary Churn </span><b>",
#                                  involuntary_churn,
#                                  "</b>%</br>"
#                   )
#         ) %>%
#         layout(title = "",
#                paper_bgcolor = "transparent",
#                plot_bgcolor = "transparent",
#                margin = list(r = 20),
#                hoverlabel = list(font = list(color = "black"),
#                                  bgcolor = "white",
#                                  bordercolor = "white"),
#                showlegend = FALSE,
#                xaxis = list(showgrid = FALSE,
#                             title = "",
#                             tickmode = "array",
#                             type = "marker",
#                             autorange = TRUE,
#                             tickfont = list(family = "serif", size = 10),
#                             ticks = "outside"
#                ),
#                yaxis = list(showgrid = FALSE,
#                             range = c(0, max(monthly_churn$total_churn)+2),
#                             title = "",
#                             tickmode = "array",
#                             ticksuffix = "%",
#                             type = "marker",
#                             tickfont = list(family = "serif", size = 10),
#                             ticks = "outside",
#                             zeroline = FALSE
#                ),
#                annotations = list(
#                        list(xref = "x", yref = "y",
#                             x = min(monthly_churn$date)+30,
#                             y = max(monthly_churn$total)+2,
#                             text = "Customer Churn last 12 Months<br><span style='color:red;'>Total Churn</span><br><span style='color:blue;'>Involuntary Churn</span>",
#                             showarrow = FALSE,
#                             align = "left")
#                )
#         )
# 
