
#-----------set local seting to prevent persian cahracter change----
oldloc <- Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "persian")
Sys.setlocale("LC_CTYPE", oldloc)
#----------------------------------------------

load("QQ14.RData")
data<-body
unique(body$masraf)
unique(body$name.ostan)
#library(jalcal)

data<- body%>%
  rename(
    takhlieh.salaneh=`takhlieh.salaneh(M^3)`
  )%>%
  mutate(Date=jal2greg(sal.amar, mah.amar, rooz.amar, asDate = TRUE))%>%
  filter(sal.amar !=  0)

data$name.ostan <- gsub('[0-9.]', '', data$name.ostan)

temp <- data[which(data$name.ostan==" تهران") ,] %>%
  group_by(noe.sazand,lat,lon,Ec,masraf,takhlieh.salaneh) %>%
  summarise(Date) %>%
  arrange(Date)

masrafsum<- aggregate(takhlieh.salaneh~ masraf,temp ,FUN = "sum" )

library(echarts4r)

masrafsum |>
  head() |>
  tibble::rownames_to_column("model") |> 
  e_charts(masraf) |>
  e_pie(takhlieh.salaneh, legend = T, label = list(show=T)) |>
  e_labels(show = F,
         formatter = "{c} \n {d}%",
         position = "inside")
  
masrafsum |>
  head() |>
  tibble::rownames_to_column("model") |> 
  e_charts(masraf) |>
  e_pie(takhlieh.salaneh, radius = c("50%", "70%"), label = list(show=T)) |>
  e_title("Donut chart",
          textAlign  = "center",
          left ="50%") %>%
  e_labels(show = TRUE,
           formatter = "{c} \n {d}%",
           position = "inside") %>%
  e_legend(right = 0, 
           orient = "vertical")






library(tidyr)
temp<-temp %>% drop_na

summasraf<- aggregate(takhlieh.salaneh~ masraf,temp,FUN = "sum" )


hc <- summasraf %>%
  hchart(
    "pie", hcaes(x = masraf, y = takhlieh.salaneh),
    name = "Fruit consumption"
  )%>%
  hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")






thakkliyeh<- aggregate(takhlieh.salaneh~ Date,temp,FUN = "sum" )
EC<- aggregate(Ec~ Date,temp,FUN = "mean" )

temp1 <- xts( thakkliyeh$takhlieh.salaneh,  thakkliyeh$Date)
temp2 <- xts(EC$Ec,  thakkliyeh$Date)
tt<-cbind(temp1, temp2)

names(temp1)<-"w"

dygraph(tt, main = 'تخلیه سالیانه')
dygraph(temp1,main = 'تخلیه سالیانه')%>%
  dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"),
                        drawPoints = TRUE, pointSize = 2) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyAxis("y", label = 'Count') %>%
  dyRangeSelector(fillColor = '#651365')


na.exclude(unique(data$masraf))
data$name.ostan <- gsub('[0-9.]', '', data$name.ostan)

library(quantmod)
library(highcharter)
highchart(type = "stock") %>% 
  hc_add_series(temp1,name = "withdrawal",color = "blue")%>%  
  hc_exporting(enabled = TRUE)%>%
  hc_yAxis(min = 0, title = list(text = "میزان برداشت (MCM)")) %>%
  hc_legend(enabled = TRUE) 
 

hchart(temp1 )

crimeIcons <- iconList(
  Agriculture = makeIcon(iconUrl = './icon/Robbery.png',iconWidth = 35,iconHeight = 35),
  Industry = makeIcon(iconUrl = './icon/Theft of Vehicle.png', iconWidth = 35, iconHeight = 35),
  Service = makeIcon(iconUrl = './icon/Theft from Vehicle.png', iconWidth = 35, iconHeight = 35),
  Green = makeIcon(iconUrl = './icon/Breaking & Entering.png', iconWidth = 35, iconHeight = 35),
  Med.herbs = makeIcon(iconUrl = './icon/Theft.png', iconWidth = 35, iconHeight = 35),
  Rural.water = makeIcon(iconUrl = './icon/Sexual Assault.png', iconWidth = 35, iconHeight = 35),
  DAM = makeIcon(iconUrl = './icon/Assault with Deadly Weapon.png', iconWidth = 35, iconHeight = 35),
  ABZI = makeIcon(iconUrl ='./icon/Homicide.png' ,iconWidth =35 ,iconHeight = 35),
  Packaging = makeIcon(iconUrl ='./icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
  Urban.water = makeIcon(iconUrl ='./icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
  Greenhouse = makeIcon(iconUrl ='./icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35)
)
crimeIcons <- iconList(
  `Robbery` = makeIcon(iconUrl = './icon/Robbery.png',iconWidth = 35,iconHeight = 35),
  `Theft of Vehicle` = makeIcon(iconUrl = './icon/Theft of Vehicle.png', iconWidth = 35, iconHeight = 35),
  `Theft from Vehicle` = makeIcon(iconUrl = './icon/Theft from Vehicle.png', iconWidth = 35, iconHeight = 35),
  `Theft` = makeIcon(iconUrl = './icon/Breaking & Entering.png', iconWidth = 35, iconHeight = 35),
  `Sexual Assault` = makeIcon(iconUrl = './icon/Theft.png', iconWidth = 35, iconHeight = 35),
  `Assault with Deadly Weapon` = makeIcon(iconUrl = './icon/Sexual Assault.png', iconWidth = 35, iconHeight = 35),
  `Homicide` = makeIcon(iconUrl = './icon/Assault with Deadly Weapon.png', iconWidth = 35, iconHeight = 35),
  `Arson` = makeIcon(iconUrl ='./icon/Homicide.png' ,iconWidth =35 ,iconHeight = 35),
  `Arson` = makeIcon(iconUrl ='./icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
  `Arson` = makeIcon(iconUrl ='./icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
  `Arson` = makeIcon(iconUrl ='./icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35)
)

data <- data %>%
  mutate(incident_date = as.Date(data$Date, format = "%m/%d/%Y")) %>%
  mutate(consumingType = as.factor(data$masraf)) 

crimeIcons[consumingType]
crimeIcons <- iconList(
      "صنعت"  = makeIcon(iconUrl = 'icon/Theft of Vehicle.png', iconWidth = 35, iconHeight = 35),
      "کشاورزی" = makeIcon(iconUrl = 'icon/Robbery.png',iconWidth = 35,iconHeight = 35),
      "دام و طيور"  = makeIcon(iconUrl = 'icon/Assault with Deadly Weapon.png', iconWidth = 35, iconHeight = 35),
      "شرب روستايي"  = makeIcon(iconUrl = 'icon/Sexual Assault.png', iconWidth = 35, iconHeight = 35),
      "فضاي سبز"  = makeIcon(iconUrl = 'icon/Breaking & Entering.png', iconWidth = 35, iconHeight = 35),
   "خدمات"  = makeIcon(iconUrl = 'icon/Theft from Vehicle.png', iconWidth = 35, iconHeight = 35),
   "گلخانه"  = makeIcon(iconUrl ='icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
   "شرب شهري" = makeIcon(iconUrl ='icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
   "بسته بندي"  = makeIcon(iconUrl ='icon/Arson.png' ,iconWidth = 35 ,iconHeight = 35),
   "آبزي پروري" = makeIcon(iconUrl ='icon/Homicide.png' ,iconWidth =35 ,iconHeight = 35),
   "گياهان داروئي"  = makeIcon(iconUrl = 'icon/Theft.png', iconWidth = 35, iconHeight = 35)

)

names(crimeIcons) <- na.omit(unique(data$masraf))

box
