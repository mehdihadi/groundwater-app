library(tidyr)
library(jalcal)
library(dplyr)
library(argonR)
library(quantmod)
library(highcharter)
library(shinycssloaders)
#library(echarts4r)


#-----------set local seting to prevent persian cahracter change----
#-----------set local seting to prevent persian cahracter change----
oldloc <- Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "persian")
#----------------------------------------------


load("QQ14.RData")

data<- body%>%
  rename(
    takhlieh.salaneh=`takhlieh.salaneh(M^3)`
    )%>%
  mutate(Date=jal2greg(sal.amar, mah.amar, rooz.amar, asDate = TRUE))%>%
  filter(sal.amar !=  0)

data$name.ostan <- gsub('[0-9.]', '', data$name.ostan)


users<<-reactiveValues(count = 0) 
################# Visits
# visits<-data.frame(Tvisit = 0,
#                     visit=0,
#                     Date=as.Date(as.POSIXlt(Sys.time())),
#                     Time=sub(".*\\s+", "", Sys.time() )
# )
# saveRDS(visits, "visits.rds")
visits <- readRDS("visits.rds")

