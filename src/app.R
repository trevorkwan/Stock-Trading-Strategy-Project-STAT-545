# load packages
library(shiny)
# options(shiny.autoreload = TRUE)
library(tidyverse)
library(quantmod)
library(lubridate)
library(dplyr)
library(TTR)
library(data.table)

# load data and create stock df with prices
faang_list <- c("META", "AMZN","AAPL", "NFLX", "GOOGL")
df_list <- list()

for (i in 1:length(faang_list)) {
    stock <- faang_list[i]
    
    # get data from yahoo finance
    data <- getSymbols(stock, src = "yahoo", auto.assign = FALSE)
    data <- as.data.frame(data)
    
    # add column to label the stock
    data$Stock <- rep(x = stock, times = nrow(data))
    
    # add date column
    data$Date <- row.names(data)
    data$Date <- ymd(data$Date)
    
    # set column names
    colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Stock", "Date")
    
    # add DEMA to df
    DEMA_list <- list()
    
    for (y in 1:200){
        new_data <- copy(data)
        new_data$DEMA_number <- as.numeric({{y}})
        new_data$DEMA <- DEMA(x = new_data$Adjusted, n = y)
        DEMA_list[[y]] <- new_data
    }
    
    DEMA_df <- bind_rows(DEMA_list)
    df_list[[i]] <- DEMA_df
}

# combine DEMA dfs
faang_df <- bind_rows(df_list)

# replace NA with original adjusted values
faang_df <- faang_df %>% mutate(DEMA = coalesce(DEMA, Adjusted))

# define ui (only does the display)
ui <- fluidPage(
    titlePanel("Faang Stock Price with DEMA"),
    
    p("Faang Stock Price plots the adjusted closing price of Faang stock and two double
      exponential moving averages (DEMA) within a specified time frame. The app simulates the DEMA crossover
      trading strategy, calculating:"),
    
    div("1. Aggregate Profit/Loss"),
    div(em("This is the cumulative profit or loss within the specified time frame if you had bought and sold
      exactly 1 share at each buy and sell point starting at $0.")),
    
    div("2. Aggregate Rate of Return"),
    div(em("This is the sum of all rate of returns if you had bought and sold at each specified buy and
      sell point.")),
    div(em("Rate of Return = (sell price - buy price) / buy price")),
    
    br(),
    
    p("The DEMA crossover trading strategy executes a buy when the shorter term DEMA is greater than the longer
      term DEMA (crossing upwards), and executes a sell when the shoter term DEMA is less than the longer
      term DEMA (crossing downwards)."),
    
    sidebarPanel(
        
        # date range input
        dateRangeInput('dateRange',
                       label = 'Select a date range.',
                       max = max(faang_df$Date),
                       min = min(faang_df$Date),
                       start = "2020-01-01", end = max(faang_df$Date)),
        
        # button for stock type
        radioButtons("stock",
                     label = "Select a stock type.",
                     choices = c("META", "AMZN","AAPL", "NFLX", "GOOGL")),
        
        # slider 1 for DEMA
        sliderInput("Number1", label = "Select a short term DEMA value.",
                    min = 1,
                    max = 200,
                    value = 20,
                    step = 1),
        
        # slider 2 for DEMA
        sliderInput("Number2", label = "Select a long term DEMA value.",
                    min = 1,
                    max = 200,
                    value = 50,
                    step = 1),
    ),
    mainPanel(
        h4("Stock Price Chart"),
        plotOutput("plot"),
        span("Aggregate Profit/Loss:",
             textOutput("net_amount")
        ),
        span("Aggregate Rate of Return:",
             textOutput("rate_amount"))
    )
)

# define server (where the number crunching happens)
server <- function(input, output) {
    
    # define reactive filtered datasets
    faang_filtered <- reactive({
        faang_df %>% 
            filter(Stock == input$stock,
                   DEMA_number %in% c(input$Number1,input$Number2),
                   Date < input$dateRange[2],
                   Date > input$dateRange[1])
    })
    
    faang_short <- reactive({
        minimum = min(c(input$Number1, input$Number2))
        
        faang_filtered() %>% 
            filter(DEMA_number == minimum)
    })
    
    faang_long <- reactive({
        maximum = max(c(input$Number1, input$Number2))
        
        faang_filtered() %>% 
            filter(DEMA_number == maximum)
    })
    
    ## outputs
    # get stock plot
    output$plot <- renderPlot({
        ggplot(faang_filtered()) +
            geom_line(aes(x = Date, y = Adjusted, color = Stock)) +
            geom_line(aes(x = Date, y = DEMA, color = as.character(DEMA_number))) + 
            labs(y = "Adjusted Closing Price", color = "DEMA") +
            scale_color_manual(values = c("blue", "red", "black"))
    })
    
    # get total net profit/loss
    output$net_amount <- renderText({
        flag = FALSE
        total = 0
        rate = 0
        
        for (z in 1:(nrow(faang_filtered())/2)){
            
            if ((faang_short()$DEMA[z] > faang_long()$DEMA[z])&(!flag)){
                total = total - faang_short()$Adjusted[z]
                flag = TRUE
            } else if ((faang_short()$DEMA[z] < faang_long()$DEMA[z])&(flag == TRUE)){
                current = total + faang_long()$Adjusted[z]
                rate = rate + ((total - current/100))
                total = total + faang_long()$Adjusted[z]
                flag = FALSE
            }
        }
        
        total
    })
    
    output$rate_amount <- renderText({
        flag = FALSE
        total = 0
        rate = 0
        
        for (z in 1:(nrow(faang_filtered())/2)){
            
            if ((faang_short()$DEMA[z] > faang_long()$DEMA[z])&(!flag)){
                total = total - faang_short()$Adjusted[z]
                buy_price = faang_short()$Adjusted[z]
                flag = TRUE
                # print(paste0("buy",buy_price))
            } else if ((faang_short()$DEMA[z] < faang_long()$DEMA[z])&(flag == TRUE)){
                sell_price = faang_long()$Adjusted[z]
                rate = rate + ((sell_price - abs(buy_price))/abs(buy_price))
                total = total + faang_long()$Adjusted[z]
                flag = FALSE
                # print(paste0("sell",sell_price))
                # print(paste0("rate", rate))
            }
        }
        
        rate
    })
} 


### for testing purposes
# test_faang <- faang_df %>%
#     filter(Stock == "FB",
#            DEMA_number %in% c(20, 50),
#            Date < "2020-10-08",
#            Date > "2020-08-20")
# 
# faang_20 <- test_faang %>%
#     filter(DEMA_number == 20)
# 
# faang_50 <- test_faang %>%
#     filter(DEMA_number == 50)
# 
# flag = FALSE
# total = 0
# for (i in 1:(nrow(test_faang)/2)){
# 
#     if ((faang_20$DEMA[i] > faang_50$DEMA[i])&(!flag)){
#         total = total - faang_20$Adjusted[i]
#         flag = TRUE
#         print(paste0("buy",total))
# 
#     } else if ((faang_20$DEMA[i] < faang_50$DEMA[i])&(flag == TRUE)){
#         total = total + faang_50$Adjusted[i]
#         flag = FALSE
#         print(paste0("sell",total))
#     }
# }
# print(total)


# combine into shinyApp
shinyApp(ui = ui, server = server)
