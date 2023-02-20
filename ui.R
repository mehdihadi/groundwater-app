library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyWidgets)
library(shinycssloaders)
# type <- c("Robbery" = "Robbery", "Theft of Vehicle"= "Theft of Vehicle",
#   "Theft from Vehicle" =  "Theft from Vehicle", "Breaking & Entering" = "Breaking & Entering",
#   "Theft" = "Theft", "Assault with Deadly Weapon" = "Assault with Deadly Weapon",
#   "Sexual Assault" =  "Sexual Assault", "Homicide" = "Homicide","Arson" = "Arson")
 

 
header <- dashboardHeader( title = p("منابع آب زیرزمینی ایران"), titleWidth = 400 ,
                           
                           
                           tags$li(a(href = 'http://tums.ac.ir',
                                     img(src = 'TUMS.png', 
                                         title = "Click to visit TUMS",
                                         height = "30px"),
                                     style = "padding-top:10px; padding-left:5px;padding-right:5px;padding-bottom:10px;"
                           ),
                           class = "dropdown"),  
                           
                           tags$li(a(href = 'https://ier.tums.ac.ir/',
                                     img(src = 'IER1.png',
                                         title = "Click to visit IER",
                                         height = "30px"),
                                     style = "padding-top:10px;padding-left:5px; padding-right:5px; padding-bottom:10px;"
                           ),
                           class = "dropdown")
                           
                           
)
#https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp


body <- dashboardBody(
  
  ################ delete this when need to switch language
  
  tags$head( tags$link(id="rtl",rel = "stylesheet", type = "text/css", href = "bootstrap-rtl.css"), ## render a right to left style
             tags$link(id="rtl",rel = "stylesheet", type = "text/css", href = "bootstrap-rtl.min.css"),
             tags$link(rel = "stylesheet", type = "text/css", href = "leafletpopup.css") 
            # tags$link(rel = "stylesheet", type = "text/css", href = "highcharts.css") 
  ),
  
  ############################## set background image 
  setBackgroundImage(shinydashboard = T,    
                     src = "bgr.jpg"
  ),
  ##############################
  
  ################
  
#     tags$head(
#        
#       tags$style("#myplot .div{ 
#    text-align: right; 
# }")  
#           
#           ),


  
  
fluidRow(
  
  column(width =9,
         box(width = NULL,title = tagList(shiny::icon("info-circle",class = 'fa-lg'), "درباره داشبورد" ), solidHeader = T, collapsible = T, status = 'info',
             strong("داشبورد منابع آب زیرزمینی ایران"),
             "یک ابزار پویا به منظور نمایش اطلاعات آمار پایه چاههای آب کشور از نظر نوع مصارف و میزان برداشت می باشد. اطلاعات استفاده شده در این داشبورد برگرفته از",
             a("دفتر مطالعات پایه شرکت مدیریت منابع آب ایران",href="https://data.wrm.ir/"),
             "می باشد که به صورت غیر برخط دریافت، پردازش و نمایش داده شده است. با انتخاب نام استان و نوع مصرف، موقعیت مکانی چاههای استان بر روی نقشه نمایش و نمودارهای روند برداشت و توزیع مصرف برای آن استان ترسیم خواهدشد. همچنین با انتخاب هر چاه بر روی نقشه اطلاعات آماری پایه مربوط به آن چاه نمایش داده می شود." 
         ),
         box(width = NULL, solidHeader = TRUE,
             withSpinner(leafletOutput('WellsMap',height = 500),type=1,hide.ui = F)),
         box(width = NULL, 
             
             fluidRow( 
               column(width =7,  
                      withSpinner( highchartOutput('plot1'),type=1,hide.ui = F)
                     
               ),
               column(width =5,
                      withSpinner( highchartOutput('plot2'),type=1,hide.ui = F)
                      
               )      
             ) 
             
         )
  ),
  column(width =3,
         box(width = NULL, title =tagList(shiny::icon("filter",class = 'fa-lg'), "فیلتر داده") ,
             solidHeader = T, collapsible = T, status = 'primary',
             
             selectizeInput('Province','استان', choices = unique(data$name.ostan), width = 380,
                            selected = "تهران",multiple = F),
             
             selectizeInput('consumeType','نوع مصرف', choices = na.exclude(unique(data$masraf)), width = 380,
                            selected = "شرب شهري",multiple = F),
             
             
             dateRangeInput('dates', label = "بازه زمانی",width = 380,
                            start = min(data$Date,na.rm = T), end = max(data$Date,na.rm = T),
                            min = min(data$Date,na.rm = T), max = max(data$Date,na.rm = T)
             ),
             
             tags$style(type="text/css", "#downloadCountryReport {background-color:orange;color: black;  height: 30px; width: 100%;font-size: 14px}"),
             downloadButton( "downloadCountryReport" , "Make a report "),
             # 
             # 
             #  selectizeInput('day_of_week','Days of Week', width = 380,
             #                choices = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
             #                selected = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
             #                multiple = T),
             # 
             # 
             #  sliderInput('time_of_day','Time of Day', min = 0, max = 23,width = 380,
             #             value = c(0,23), step = 1),
             
             # submitButton(text = "Submit",icon =icon('filter'))
         ),
         
         
         box(width = NULL,title = tagList(shiny::icon("info-circle",class = 'fa-lg'), "طراحی داشبورد"), solidHeader = T, collapsible = T, status = 'info',
             
             fluidRow(
               
               argonColumn( style = " border-radius: 10px",
                            width = 12,
                            br(style="height:10px;font-size:10px;"),
                            tags$form(
                              tags$legend(tags$img( src="dr hadi.jpg", class="img-circle", alt="User Image",style= "height:40px; width: 40px" ), a('مهدی هادی', href = 'https://ier.tums.ac.ir/%D8%AF%DA%A9%D8%AA%D8%B1-%D9%85%D9%87%D8%AF%DB%8C-%D9%87%D8%A7%D8%AF%DB%8C') ,style = 'font-size:14px;font-weight:bold') ,
                              p( "دانشیار مرکز تحقیقات کیفیت آب علوم پزشکی تهران" ,style = 'font-size:11px;font-weight:bold'),  
                            #  icon('link', class = 'fa-lg'), a('پژوهشکده محیط زیست', href = 'http://ier.tums.ac.ir', target = '_blank',style = 'font-size:12px;font-weight:bold'),
                              icon('link', class = 'fa-lg'), a('صفحه شخصی', href = 'http://ier1.tums.ac.ir', target = '_blank',style = 'font-size:12px;font-weight:bold'),
                              icon('link ', class = 'fa-lg'), a('سایر ابزارها', href = 'http://ier1.tums.ac.ir/applications.html', target = "_blank",style = 'font-size:12px;font-weight:bold'),
                              icon('linkedin ', class = 'fa-lg'), a('لینکدین', href = 'https://www.linkedin.com/in/mahdi-hadi-7081808', target = "_blank",style = 'font-size:12px;font-weight:bold'),
                              tags$address(style = 'font-size:12px;',
                              tags$strong("ایمیل:"), tags$a( href="mailto:m.hadi1981@gmail.com","m.hadi1981@gmail.com"),
                              br(style="height:10px;font-size:10px;"),
                              tags$strong("تلفن:"), tags$a(href="tel:+989189061738","+989189061738")
                              ),
                              tags$legend(tags$img( src="IER1.png", alt="",style="height:40px; width: 40px"), "مرکز تحقیقات کیفیت آب، پژوهشکده محیط زیست دانشگاه علوم پزشکی تهران" ,style = 'font-size:11px;font-weight:bold') ,
                              tags$address(style = 'font-size:12px; ',
                                           tags$strong( "آدرس:" ),
                                           p("تهران، میدان انقلاب، خیابان کارگر شمالی، نرسیده به بلوارکشاورز، پلاک 1547، طبقه 8",style = 'font-size:11px') ,
                                           tags$strong("تلفن:"), tags$a( href="tel:+982188988135","+982188988135")
                                          
                              )
                              
                             ),
                            uiOutput('Version')     
                       
               ),
               
               
             ) 
             
             
             
             
         )
         
         
  )
  
),
includeMarkdown("Contact.Rmd")
            )

                      
ui <- dashboardPage(skin = 'blue',
                    title = titlePanel(
                      windowTitle = "irGWater",
                      title = tags$head(tags$link(rel="shortcut icon", 
                                                  href="icon.png", 
                                                  type="image/vnd.microsoft.icon"))),
                    header,
                    dashboardSidebar(disable = T),
                    body
                    )



