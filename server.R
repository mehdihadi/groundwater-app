library(dplyr)
library(data.table)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)

# read data
#data <- tbl_df(fread('Washington_DC.csv',header = T))


# modify data
  data <- data %>%
    mutate(incident_date = as.Date(data$Date, format = "%m/%d/%Y")) 
    #mutate(consumingType = as.factor(masraf))  %>%
    #mutate(content = paste('<b>',consumingType,'</b>'))
    #mutate(content = paste(sep = '<br/>', content,lon,lat)) 
#   mutate(year_month = as.yearmon(incident_date))

# Customize icons
  crimeIcons <- iconList(
    "صنعت"  = makeIcon(iconUrl = 'icon/industry.png', iconWidth = 35, iconHeight = 35),
    "کشاورزی" = makeIcon(iconUrl = 'icon/Agri.png',iconWidth = 35,iconHeight = 35),
    "دام و طيور"  = makeIcon(iconUrl = 'icon/poultry.png', iconWidth = 35, iconHeight = 35),
    "شرب روستايي"  = makeIcon(iconUrl = 'icon/rural.png', iconWidth = 35, iconHeight = 35),
    "فضاي سبز"  = makeIcon(iconUrl = 'icon/green.png', iconWidth = 35, iconHeight = 35),
    "خدمات"  = makeIcon(iconUrl = 'icon/service.png', iconWidth = 35, iconHeight = 35),
    "گلخانه"  = makeIcon(iconUrl ='icon/greenhouse.png' ,iconWidth = 35 ,iconHeight = 35),
    "شرب شهري" = makeIcon(iconUrl ='icon/urban.png' ,iconWidth = 35 ,iconHeight = 35),
    "بسته بندي"  = makeIcon(iconUrl ='icon/packaging.png' ,iconWidth = 35 ,iconHeight = 35),
    "آبزي پروري" = makeIcon(iconUrl ='icon/fishing.png' ,iconWidth =35 ,iconHeight = 35),
    "گياهان داروئي"  = makeIcon(iconUrl = 'icon/medherb.png', iconWidth = 35, iconHeight = 35)
    
  )

  names(crimeIcons) <-  na.omit(unique(data$masraf))

server <- function(input, output) {
  
  ####################### count connected users: some part of code in Global
  onSessionStart = isolate({
    
    users$count <- users$count + 1
    
    
    visit<<-data.frame(Tvisit = last(visits$Tvisit)+1,
                       visit=1,
                       Date= as.Date(as.POSIXlt(Sys.time())),
                       Time=sub(".*\\s+", "", Sys.time() ) 
                       
                       
    ) 
    
    visits <<- rbind(visits,visit)
    saveRDS(visits, "visits.rds")
    
  }) 
  
  
  onSessionEnded(function() {
    isolate({
      users$count <<-users$count - 1
      
    })
  })
  
  
  
  output$Version <- renderUI({  
    
    div(style="vertical-align:center; text-align:center; margin-top: 8px; height=25px;",
        tags$span( style="color: black; font-size:10px;",paste0("کاربر آنلاین: ", users$count," ") ),
        
        tags$span(style="color: black; font-size:10px;", paste0("بازدید: ", last(visits$Tvisit)," ") ), 
        
        tags$span(style="color: black; font-size:10px;", paste0("بازدید امروز:", last(aggregate(visit~Date,visits,FUN = "sum" )[order(as.Date(aggregate(visit~Date,visits,FUN = "sum" )$Date, format="%d/%m/%Y")),]$visit)) )
    )
    
  })
  
 
  
  ############################
  
  
  
  filteredData <- reactive({
    data %>%
      filter(name.ostan %in% input$Province )  %>%
      filter(incident_date > input$dates[1] & incident_date < input$dates[2]) 
     # filter(day_of_week %in% input$day_of_week) %>%
     # filter(hour_of_day >= input$time_of_day & hour_of_day <= input$time_of_day)
  })

observe({       
  
  DF<-filteredData()%>%
    filter(masraf %in% input$consumeType )
  
  output$WellsMap <- renderLeaflet({
   leaflet(DF,options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles(group = 'OSM') %>%
      addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
      addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
      addMarkers(~lon,~lat, 
         popup = ~paste0("<h4>مشخصات چاه</h4>","<b>عمق:</b>",omgh.chah,"<br>","<b>سال حفاری:</b>",sal.hafr.chah,
                         "<br>","<b>مصرف:</b>",masraf,
                         "<br>","<b>برداشت سالیانه(m3):</b>",takhlieh.salaneh,
                         "<br>","<b>نوع سازند:</b>",noe.sazand,
                         ifelse(input$consumeType=="کشاورزي",paste0("<br>","<b>کشت غالب:</b>",kesht.galeb),""),
                         "<br>","<b>دما:</b>",dama.ab,
                         "<br>","<b>هدایت الکتریکی:</b>",Ec,sep=" "),
         icon = ~crimeIcons[masraf],clusterOptions = markerClusterOptions()
      ) %>%
      
      addLayersControl(
        baseGroups = c('OSM', 'Esri', 'CartoDB'),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
})
 
  
  
  
  # output$plot1 <- renderDygraph({
  #   dydf <- matrix()
  #   for (i in 1:length(input$crimeType)){
  #     temp <- filteredData() %>%
  #       filter(incidentType %in% input$crimeType[i]) %>%
  #       group_by(year_month) %>%
  #       summarise(count = n()) %>%
  #       arrange(year_month)
  #     temp1 <- xts(temp$count, temp$year_month)
  #     if(dim(temp1)[[1]] == 0) {
  #       temp1 <- 0
  #     }
  #     dydf <- cbind(dydf, temp1)
  #   }
  #   dydf <- dydf[,-1]
  #   dydf[is.na(dydf)] <- 0
  #   colnames(dydf) <- input$crimeType
  #   dygraph(dydf, main = 'Incidents Trend/Month') %>%
  #     dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"),
  #               drawPoints = TRUE, pointSize = 2) %>%
  #     dyHighlight(highlightCircleSize = 5,
  #                 highlightSeriesBackgroundAlpha = 0.2,
  #                 hideOnMouseOut = FALSE,
  #                 highlightSeriesOpts = list(strokeWidth = 3)) %>%
  #     dyAxis("y", label = 'Count') %>%
  #     dyRangeSelector(fillColor = '#651365')
  # })



temp <- reactive({
  
  filteredData()%>%
    group_by(noe.sazand,lat,lon,Ec,masraf,takhlieh.salaneh) %>%
    summarise(Date) %>%
    arrange(Date)%>% 
    drop_na
  
})



observe({
  DF<-temp()%>%
    filter(masraf %in% input$consumeType ) 
  
  output$plot1 <- renderHighchart({
    
    
    thakkliyeh<- aggregate(takhlieh.salaneh~ Date,DF,FUN = "sum" )
   # EC<- aggregate(Ec~ Date,temp,FUN = "mean" )
    
    thakkliyeh <- xts(thakkliyeh$takhlieh.salaneh/1000000,  thakkliyeh$Date)
    #temp2 <- xts(EC$Ec,  thakkliyeh$Date)
    #tt<-cbind(temp1, temp2) 
    names(thakkliyeh)<-"Withdrawal"
       
######################### dygraph code
     # dygraph(thakkliyeh, main = 'میزان برداشت آب' ) %>%
     #  dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"),
     #            drawPoints = TRUE, pointSize = 2) %>%
     #  dyHighlight(highlightCircleSize = 5,
     #              highlightSeriesBackgroundAlpha = 0.2,
     #              hideOnMouseOut = FALSE,
     #              highlightSeriesOpts = list(strokeWidth = 3)) %>%
     #  dyAxis("y", label = 'Withdrawal (MCM)') %>%
     #  dyRangeSelector(fillColor = '#651365')%>%
     #      dyCSS("www/dygraph.css")
###################################### hightchart code
    my_theme <- hc_theme(
      chart = list(
        backgroundColor = "#f4fcfe", 
        style = list(
          fontFamily = "vazir" 
           
        )
      )
    )
    
    highchart(type = "stock") %>% 
       hc_title( text =paste("(MCM)میزان برداشت آب",input$consumeType,input$Province),style = list(fontSize="12px" ,color = "#22A884", useHTML = TRUE))%>% 
       hc_add_series(thakkliyeh,name = "withdrawal",color = "blue")%>%  
       hc_exporting(enabled = TRUE)%>%
       hc_yAxis(min = 0,  title = list(text = paste("(MCM)میزان برداشت آب",input$consumeType),style = list(fontSize="8px" ,color = "blue", useHTML = TRUE))) %>%
       hc_legend(enabled = TRUE) %>%
       hc_add_theme(my_theme)
  })
  
})  

observe({
  
  output$plot2 <- renderHighchart({
    
    
    masrafsum<- aggregate(takhlieh.salaneh~ masraf,temp(),FUN = "sum" )
    
    
    my_theme <- hc_theme(
      chart = list(
        backgroundColor = "#f4fcfe",
        
        style = list(
          fontFamily = "vazir"
 
        ) 
        
      )

    )
 
    
    
    masrafsum %>%
      hchart(
        "pie", hcaes(x = masraf, y = takhlieh.salaneh  ),
        name = "برداشت"
      )%>%
      hc_title( text =paste("توزیع برداشت آب در" ,input$Province),style = list(fontSize="12px" ,color = "#22A884", useHTML = TRUE))%>%
      hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")%>%
      hc_add_theme(my_theme)%>%
      hc_plotOptions(pie =list(dataLabels = list(
                                                 backgroundColor= 'black',
                                                 borderRadius= 7,
                                                 padding= 4,
                                                 color = "blue",
                                                 enabled = TRUE,style = list( fontFamily= 'vazir', fontWeight="italic", fontSize="8px" ,color = "blue", useHTML = T)))) 
      
    
      
     
  })
  
})  

################################################## Country report
output$downloadCountryReport <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.',"html" 
          
          #       switch(
          #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
          # )
          
    )
  },
  content = function(file) {
    src <- normalizePath("CountryReport.Rmd" )
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, "CountryReport.Rmd", overwrite = TRUE)
    
    
    out <- rmarkdown::render("CountryReport.Rmd",rmarkdown::html_document()
                             #                          switch(
                             #   input$format,
                             #   PDF = rmarkdown::pdf_document(), HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
                             # ) 
    )
    file.rename(out, file)
  }
)

  
}
