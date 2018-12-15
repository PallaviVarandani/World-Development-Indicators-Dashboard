#Loading the Libraries
library(shiny)
library(igraph)
library(network)
library(sna)
library(ndtv)
library(extrafont)
library(networkD3)
library(shinythemes)
library(readr)
library(stats)
library(base)
library(rworldmap)
library(animation)
library(caTools)
library(RColorBrewer)
library(classInt)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(dtplyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(maps, warn.conflicts = FALSE, quietly = TRUE)
library(reshape)
library(graphics)
library(reshape2)
library(plotly)

#Designing the User Interface
ui <- fluidPage(theme = shinytheme("superhero"),
                
                titlePanel(title=h1("Development Analysis Of Africa", align="center")),
                
                sidebarPanel(
                  br(),
                  h4(helpText("")),
                  
                  selectInput("Type", label = h3("Select the Category:"), choices = c("General","Specific")),
                  conditionalPanel(condition = "input.Type == 'General'",selectInput("Indicators", label = h3("Select Indicators:"), as.list(counts$IndicatorName)),
                                   uiOutput("years")),
                  actionButton(inputId = "go", label = "RUN")
                ),
                
                mainPanel(
                  uiOutput("type")
                )
)

#Designing the Server
server <- shinyServer(function(input, output){
#Year slider
  output$years <- renderUI({
    id1 <- input$Indicators
    id <- counts[counts$IndicatorName == id1,]
    conditionalPanel(condition = "input.Indicators = id1",
                     sliderInput("Years", label = h3("Select the year:"), value = 2000, min = id$FirstYear, max = id$LastYear,step = 1 ))
  })
#Main Output
  output$type <- renderUI({
    check1 <- input$Type == "General"
    check2 <- input$Type == "Specific"
    
    if( check1) {
      tabsetPanel(
        tabPanel("Map",
                 plotOutput(outputId = "plot1")),
        tabPanel("Tabular View",
                 tableOutput("table")),
        tabPanel("TOP 5s", plotlyOutput(outputId = "plot2")),
        tabPanel("BOTTOM 5s", plotlyOutput(outputId = "plot3")),
        tabPanel("Limitations of Selected Indicator", textOutput(outputId = 'Limitation'))
      )
    }
    
    else if(check2){
      tabsetPanel(
        tabPanel(
                "Economic Policy and Debt", 
                 h3("GDP per capita (current US$)"), plotOutput(outputId = 'plot4'),
                 h3("GDP per capita growth (annual %)"),plotOutput(outputId = "plot5"),
                 h3("GNI per capita growth(annual %)"), plotOutput(outputId = 'plot6')
                
                 ),
        tabPanel(
          "Education", 
           h3("Literacy rate, adult total (% of people ages 15 and above)"),plotOutput(outputId = "plot7") 
        ),
        tabPanel(
          "Environment",
          h3("Access to electricity (% of population)"),plotOutput(outputId = "plot8"),
          h3("Population density (people per sq. km of land area)"),plotOutput(outputId = "plot9")
        ),
        tabPanel(
          "Financial Sector", 
           h3("Inflation, consumer prices (annual %)"),plotOutput(outputId = "plot10") 
        ),
        tabPanel(
          "Health", 
          h3("Population growth (annual %)"), plotOutput(outputId = 'plot11'),
          h3("Life expectancy at birth, total (years)"), plotOutput(outputId = 'plot12'),
          h3("Mortality rate, under-5 (per 1,000 live births)"), plotOutput(outputId = 'plot13'),
          h3("People using at least basic drinking water services (% of population)"),plotOutput(outputId = "plot14")
          
        ),
        tabPanel(
          "Infrastructure", 
          h3("Research and development expenditure (% of GDP)"), plotOutput(outputId = 'plot15'),
          h3("Mobile cellular subscriptions"),plotOutput(outputId = "plot16"), 
          h3("Individuals using the Internet (% of population)"),plotOutput(outputId = "plot17")
          
        ),
        tabPanel(
          "Private Sector & Trade", 
          h3("Time required to start a business (days)"),plotOutput(outputId = "plot18"), 
          h3("Time required to build a warehouse (days)"),plotOutput(outputId = "plot19")
        ),
        tabPanel(
          "Public Sector", 
          h3("Armed forces personnel, total"),plotOutput(outputId = "plot20")
        ),
        tabPanel(
          "Social Protection & Labor",
          h3("Labor force, total"),plotOutput(outputId = "plot21")
        )
        
      )
      
    }
    else{
      print("Not Applicable")
    }
    
  })
####Functions for the Output###
  data <- eventReactive(input$go,{
    id <- input$Indicators
    id <- counts[counts$IndicatorName == id,]
  })
##Map Plot
  map <- eventReactive(input$go,{
    indicatorName <- input$Indicators
    indicatorYear <- input$Years
    filtered <- Indicators[Indicators$IndicatorName==indicatorName & Indicators$Year==indicatorYear,]
    #filtered <- Indicators[Indicators$IndicatorName==Indicators$IndicatorName[1] & Indicators$Year==2000,]
    map.world <- merge(x=map_data(map="world"),
                       y=filtered[,c("CountryName","Value")],
                       by.x="region",
                       by.y="CountryName",
                       all.x=TRUE)
    map.world <- map.world[order(map.world$order),]
    
    
      ggplot(map.world) +
      geom_map(map=map.world, aes(map_id=region, fill=Value)) +
      borders("world",colour="black")+
      coord_map(xlim = c(-20, 55),ylim = c(-40, 40)) +
      scale_fill_gradient(low = "blue", high = "red", guide = "colourbar") +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            legend.title=element_blank(),
            legend.position="bottom", 
            legend.key.width = unit(6, "lines"),
            legend.text = element_text(size = 10)
      ) +
      ggtitle(paste0(indicatorName, " in ", indicatorYear))
    
  })
##Table Plot
  table <- eventReactive(input$go, {
    indicatorName <- input$Indicators 
    indicatorYear <- input$Years 
    filtered <- Indicators[Indicators$IndicatorName==indicatorName & Indicators$Year==indicatorYear,] 
    filtered[,c("CountryName", "Value","Income.Group", "Region")]
  })
##Highest Country graph
  top <- eventReactive(input$go, {
    Indicators %>% 
      filter(IndicatorName == input$Indicators) -> mData
    
    mData %>% 
      filter(Year == input$Years) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == input$Years) %>% 
      arrange(desc(Value)) %>% 
      select(CountryName) %>% 
      slice(1:5) -> top5Cont
    
    top5 = left_join(top5Cont, cData, "CountryName")
    
    top5 = top5[,c('CountryName','Year','Value')]
    
    top5_m = rbind(top5,Average)
    
    pre <- lm(Value~.,data = top5_m)
    
    maxyear = max(top5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(top5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(top5_m$CountryName),x)
    
    pretop5_m <- data.frame(CountryName,Year)
    
    pretop5_m$Value <- predict(pre,pretop5_m)
    
    top5_pre <- rbind(top5_m,pretop5_m)
    
    p <- ggplot(data=top5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                      color = "blue", size=1.5) +
            ggtitle(paste0("Highest Performing ", input$Indicators)) + 
            ylab("Values")
    print(ggplotly(p))
    
    
  })
##Lowest Country graph 
  bottom <- eventReactive(input$go, {
    Indicators %>% 
      filter(IndicatorName == input$Indicators) -> mData
    
    mData %>% 
      filter(Year == input$Years) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == input$Years) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    p <- ggplot(data=bot5_pre, 
                 aes(x=Year, y=Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing ", input$Indicators)) + 
            ylab("Values")
    
    print(ggplotly(p))
    
  })
###Limitation of Indicators
  Limitation <- eventReactive(input$go,{
    id <- input$Indicators
    id <- Indicators[Indicators$IndicatorName == id,]
    print(as.character(unique(id$Limitations.and.exceptions)))
  })
###Specific of GDP percapita current US $
  plot4 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'GDP per capita (current US$)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing GDP per capita (current US$)")) + 
            ylab("Values"))
 
    
  })
###Specific of GDP per capita growth (annual %)  
  plot5 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'GDP per capita growth (annual %)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing GDP per capita growth (annual %)")) + 
            ylab("Values"))
    
  })
###Specific of GNI per capita growth (annual %)  
  plot6 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'GNI per capita growth (annual %)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing GNI per capita growth (annual %)")) + 
            ylab("Values"))
    
  })
###Specific of Literacy rate, adult total (% of people ages 15 and above)
  plot7 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Literacy rate, adult total (% of people ages 15 and above)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing Literacy rate, adult total (% of people ages 15 and above)")) + 
            ylab("Values"))
    
  })
###Specific of Access to electricity (% of population)
  plot8 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Access to electricity (% of population)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                aes(x=Year, y= Value, 
                    group=CountryName, colour=CountryName)) + 
      geom_line(size = 1) + 
      geom_vline(xintercept = maxyear, linetype="dotted", 
                 color = "blue", size=1.5) +
      ggtitle(paste0("Lowest Performing Access to electricity (% of population)")) + 
      ylab("Values"))
    
  })
###Specific of  Population density (people per sq. km of land area)
  plot9 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Population density (people per sq. km of land area)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Population density (people per sq. km of land area))'")) + 
            ylab("Values"))
    
  })
###Specific of Inflation, consumer prices (annual %)
  plot10 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Inflation, consumer prices (annual %)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Inflation, consumer prices (annual %)'")) + 
            ylab("Values"))
    
  })
  ###Specific of  Population growth (annual %)
  plot11 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Population growth (annual %)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Population growth (annual %)'")) + 
            ylab("Values"))
    
  })
  ###Specific of Life expectancy at birth, total (years)
  plot12 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Life expectancy at birth, total (years)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Life expectancy at birth, total (years)'")) + 
            ylab("Values"))
    
  })
  ###Specific of Mortality rate, under-5 (per 1,000 live births)
  plot13 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Mortality rate, under-5 (per 1,000 live births)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Mortality rate, under-5 (per 1,000 live births)'")) + 
            ylab("Values"))
    
  })
###Specific of  People using at least basic drinking water services (% of population)
  plot14 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'People using at least basic drinking water services (% of population)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'People using at least basic drinking water services (% of population)'")) + 
            ylab("Values"))
    
  })
  ###Specific of Research and development expenditure (% of GDP)
  plot15 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Research and development expenditure (% of GDP)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Research and development expenditure (% of GDP)'")) + 
            ylab("Values"))
    
  })
###Specific of Mobile cellular subscriptions 
  plot16 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Mobile cellular subscriptions') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Mobile cellular subscriptions'")) + 
            ylab("Values"))
    
  })
###Specific of Individuals using the Internet (% of population)
  plot17 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Individuals using the Internet (% of population)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Individuals using the Internet (% of population)'")) + 
            ylab("Values"))
    
  })
###Specific Of Time required to start a business (days)
  plot18 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Time required to start a business (days)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Time required to start a business (days)")) + 
            ylab("Values"))
    
  })
###Specific of  Time required to build a warehouse (days)
  plot19 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Time required to build a warehouse (days)') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Time required to build a warehouse (days)")) + 
            ylab("Values"))
    
  })
###Specific of Armed forces personnel, total
  plot20 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Armed forces personnel, total') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Armed forces personnel, total'")) + 
            ylab("Values"))
    
  })
###Specific of Labor force, total
  plot21 <- eventReactive(input$go,{
    Indicators %>% 
      filter(IndicatorName == 'Labor force, total') -> mData 
    
    mData %>% 
      filter(Year == max(mData$Year)) %>% 
      select(CountryCode) -> conSet
    
    mData %>%
      group_by(Year) %>%
      summarize(Value = mean(Value)) -> Average
    Average$CountryName = 'Averages'
    
    cData = left_join(conSet, mData, by = "CountryCode")
    
    cData %>% 
      filter(Year == max(mData$Year)) %>% 
      arrange(Value) %>% 
      select(CountryName) %>% 
      slice(1:5) -> bot5Cont
    
    bot5 = left_join(bot5Cont, cData, "CountryName")
    
    bot5 = bot5[,c('CountryName','Year','Value')]
    
    bot5_m = rbind(bot5,Average)
    
    pre <- lm(Value~.,data = bot5_m)
    
    maxyear = max(bot5_m$Year)
    
    Year = c()
    
    for (i in maxyear:2030) {
      Year =append(Year,rep(i,length(unique(bot5_m$CountryName))))
    }
    
    x = length(unique(Year))
    
    CountryName <- rep(unique(bot5_m$CountryName),x)
    
    prebot5_m <- data.frame(CountryName,Year)
    
    prebot5_m$Value <- predict(pre,prebot5_m)
    bot5_pre <- rbind(bot5_m,prebot5_m)
    
    print(ggplot(data=bot5_pre, 
                 aes(x=Year, y= Value, 
                     group=CountryName, colour=CountryName)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = maxyear, linetype="dotted", 
                       color = "blue", size=1.5) +
            ggtitle(paste0("Lowest Performing 'Labor force, total'")) + 
            ylab("Values"))
    
  })
  
  output$plot1 <- renderPlot({ map()})
  output$table <- renderTable({table()})
  output$plot2 <- renderPlotly({ top()})
  output$plot3 <- renderPlotly({ bottom()})
  output$Limitation <- renderPrint({Limitation()})
  output$plot4 <- renderPlot({plot4()})
  output$plot5 <- renderPlot({plot5()})
  output$plot6 <- renderPlot({plot6()})
  output$plot7 <- renderPlot({plot7()})
  output$plot8 <- renderPlot({plot8()})
  output$plot9 <- renderPlot({plot9()})
  output$plot10 <- renderPlot({plot10()})
  output$plot11 <- renderPlot({plot11()})
  output$plot12 <- renderPlot({plot12()})
  output$plot13 <- renderPlot({plot13()})
  output$plot14 <- renderPlot({plot14()})
  output$plot15 <- renderPlot({plot15()})
  output$plot16 <- renderPlot({plot16()})
  output$plot17 <- renderPlot({plot17()})
  output$plot18 <- renderPlot({plot18()})
  output$plot19 <- renderPlot({plot19()})
  output$plot20 <- renderPlot({plot20()})
  output$plot21 <- renderPlot({plot21()})
})

#Calling the Application
shinyApp(ui = ui , server = server)

