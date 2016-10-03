library(RMySQL)
library(shiny)
library(quantmod)
library(dplyr)
library(magrittr)
library(bdscale)
library(scales)
library(ggplot2)
library(rvest)
library(XML)

#Define UI
ui <- shinyUI(fluidPage(
  titlePanel("Stock Screener"),
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Enter a Ticker: Prices from YahooFinance, Fundamentals from FinViz.com"),
                        
                        textInput("symb", "Symbol", "SPY"),
                        
                        dateRangeInput("dates", 
                                       "Date range",
                                       start = "2016-01-01", 
                                       end = as.character(Sys.Date())),
                        
                        br(),
                        br(),
                        
                        checkboxInput("fv", "Show Fundamentals", 
                                                    value = TRUE),
                        

                        actionButton("goButton",
                                      "Run", value = FALSE)
                        ),
                
                mainPanel(
                        plotOutput("plot"),
                        dataTableOutput("tblout")
                        )
        )
))


server <- shinyServer(function(input, output) {
        
        tableSumm <- eventReactive(input$goButton, {
                
          if(input$fv){
            
            ticker <- input$symb
            # smallList <- tradeList[tradeList$Ticker==ticker,]
            
            website <- paste0("http://finviz.com/quote.ashx?t=",ticker)
            
            fvResult <- readHTMLTable(website)
            
            # transform list into df
            try(fin_hl <- fvResult[[4]], silent = FALSE)
            try(fin_hl <- fin_hl[14:nrow(fin_hl),], silent = FALSE)
            
            try(df <- data.frame(Category=character(0), Value=character(0)),silent = FALSE)
            
            for(i in 1:(ncol(fin_hl)/2)) {
              try(tmp <- fin_hl[,((i-1)*2+1):(i*2)], silent = FALSE)
              try(names(tmp) <- c("Category","Value"), silent = FALSE)
              try(df <- rbind(df, tmp), silent = FALSE)
            }
            
            df <- df %>% subset(Category %in% c("Market Cap","Income","Sales","Sales past 5Y","Sales Q/Q",
                                                       "EPS (ttm)","EPS Q/Q","EPS past 5Y","P/E",
                                                       "P/FCF","Gross Margin","Oper. Margin","Profit Margin",
                                                       "Shs Outstand","Short Float","52W Range"))
            
            df
            
          }
                
        })
        
        
        chart <- eventReactive(input$goButton, {

                        ticker <- input$symb
                        fromDate <- input$dates[1]
                        toDate <- input$dates[2]
                        
                        #query for quotes
                        quotes <- getSymbols(ticker, from = fromDate, to = toDate, adjust = TRUE, auto.assign = FALSE)
                        
                        #clean up DF
                        input <- data.frame(quotes) %>% 
                                set_names(c("open", "high", "low", "close", "volume", "adjusted")) %>%
                                mutate(date=as.Date(rownames(.)))
                        
                        #add technical indicators
                        input$ma50 <- SMA(input$close,50)
                        input$ma21 <- SMA(input$close,21)
                        input$ma9 <- SMA(input$close,9)
                        bbands <- as.data.frame(BBands(input$close, n=20,sd=2))
                        bbands <- bbands[,c("up","dn")]
                        input <- cbind(input,bbands)
                        
                        
                        p1 <- ggplot(input, aes(x=date, ymin=low, ymax=high, lower=pmin(open,close), upper=pmax(open,close), fill=open<close, group=1, middle=pmin(open,close))) + 
                                geom_boxplot(stat='identity') +
                                # theme_dark() +
                                ggtitle(paste0(ticker,": Daily")) +
                                xlab('') + 
                                ylab('') + 
                                theme(legend.position='none') +
                                scale_x_bd(business.dates=input$date, max.major.breaks=10, labels=date_format("%b '%y")) +
                                geom_line(aes(x=date,y=ma9),color="blue") +
                                geom_line(aes(x=date,y=ma21),color="red") +
                                geom_line(aes(x=date,y=ma50),colour="green") +
                                geom_line(aes(x=date,y=up),color="purple") +
                                geom_line(aes(x=date,y=dn),color="purple")
                        p1
                        
        })

        output$tblout <- renderDataTable({

                tableSumm()

        })
        
        output$plot <- renderPlot({
                
                chart()
                
        })

})

shinyApp(ui = ui, server = server)


