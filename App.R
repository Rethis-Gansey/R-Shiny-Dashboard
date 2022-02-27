

#Encodoptions(encoding = "UTF-8")

# Install packages
PackageNames <- c("shiny", "tidyverse", "XML", "rvest", "shinydashboard","rgdal","highcharter","viridisLite","treemap","readxl","leaflet","dplyr","tidyverse","dygraphs","plotly","lubridate","directlabels","magrittr","readr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Loading the necessary package
library(shiny)
library(tidyverse)
library(XML)
library(rvest)
library(shinydashboard)
library(rgdal)
library(highcharter)
library(viridisLite)
library(treemap)
library(readxl)
library(leaflet)
library(dplyr)
library(tidyverse)
library(dygraphs)
library(plotly)
library(directlabels)
library(lubridate)
library(magrittr)
#library(oceanis)
library(readr)
library(rvest)

# Download the Covid19 information databases from the John Hopkins University Github repository, accessible through the following links
#We download the database on the information of each country
#We download the HTML page containing the data
tablec = read_html("https://github.com/CSSEGISandData/COVID-19/blob/web-data/data/cases_country.csv")
#We extract the first data table from the page
tablec1= tablec%>%html_table(fill = TRUE)
tablec2 = tablec1[[01]]
#As the table is kept, the variable names are not on the first line. We correct according and then delete the extra line
colnames(tablec2)=tablec2[1,]
tablec2=tablec2[-1,]
base_coviz=tablec2[,-1]

#We download the database of daily cases for each country
base_coviz_downl = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_time.csv")

#We create a variable that contains the date of the last update of the data
final_date = base_coviz_downl$Report_Date_String[length(base_coviz_downl$Report_Date_String)]

#We load the data files to build the map and merge them with the existing data
world_spdf <- readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_spdf@data$Deaths <-base_coviz$Deaths[match(world_spdf@data$ISO3,base_coviz$ISO3)]
world_spdf@data$Deaths <- as.numeric(as.character(world_spdf@data$Deaths))
world_spdf@data$Confirmed <-base_coviz$Confirmed[match(world_spdf@data$ISO3,base_coviz$ISO3)]
world_spdf@data$Confirmed <- as.numeric(as.character(world_spdf@data$Confirmed))
world_spdf@data$Recovered <-base_coviz$Recovered[match(world_spdf@data$ISO3,base_coviz$ISO3)]
world_spdf@data$Recovered <- as.numeric(as.character(world_spdf@data$Recovered))
world_spdf@data$Active <-base_coviz$Active[match(world_spdf@data$ISO3,base_coviz$ISO3)]
world_spdf@data$Active <- as.numeric(as.character(world_spdf@data$Active))
world_spdf@data$Incident_Rate <-base_coviz$Incident_Rate[match(world_spdf@data$ISO3,base_coviz$ISO3)]
world_spdf@data$Incident_Rate <- as.numeric(as.character(world_spdf@data$Incident_Rate))
world_spdf@data$Mortality_Rate <-base_coviz$Mortality_Rate[match(world_spdf@data$ISO3,base_coviz$ISO3)]
world_spdf@data$Mortality_Rate <- as.numeric(as.character(world_spdf@data$Mortality_Rate))

#We withdraw the observations for which the incident rate is not available and convert the data into numerical format
aq<-world_spdf@data[world_spdf@data$Incident_Rate !="NA",]
ab<-as.data.frame(aggregate(aq[c('Incident_Rate','Mortality_Rate')],list(aq$SUBREGION),mean))
world_spdf@data$zone_inc <-ab$Incident_Rate[match(world_spdf@data$SUBREGION,ab$Group.1)]
world_spdf@data$zone_inc <- as.numeric(as.character(world_spdf@data$zone_inc))
world_spdf@data$zone_death <-ab$Mortality_Rate[match(world_spdf@data$SUBREGION,ab$Group.1)]
world_spdf@data$zone_death <- as.numeric(as.character(world_spdf@data$zone_death))

# We merge the data already loaded with country information (latitude, longitude)
base_coviz_downl$new_lat<-base_coviz$Lat[match(base_coviz_downl$Country_Region,base_coviz$Country_Region)]
base_coviz_downl$new_long<-base_coviz$Long_[match(base_coviz_downl$Country_Region,base_coviz$Country_Region)]
# For more clarity on the figures, for Benin we remove all dates where confirmed cases are not yet obtained
base2<-base_coviz_downl[which(base_coviz_downl$Country_Region=="Benin" & base_coviz_downl$Confirmed>0),]
#We select some countries
base_pays_lim<-base_coviz_downl[which(base_coviz_downl$Country_Region=="Benin" | base_coviz_downl$Country_Region=="Togo" | base_coviz_downl$Country_Region=="Burkina Faso" | base_coviz_downl$Country_Region=="Niger" ),]


#colors
confirmed_color <- "purple"
active_color <- "#1f77b4"
recovered_color <- "forestgreen"
death_color <- "red"




#############   FRONT END OF THE APPLICATION  #####################################

# header
header <- dashboardHeader(title = 'COVID19')

# menu bar
sider <- dashboardSidebar(
  sidebarMenu(
    menuItem('Benin', tabName = 'dashboard', icon = icon('dashboard')),
    menuItem('Evolution in Benin', tabName = 'evol_benin', icon=icon("fas fa-chart-line",lib="font-awesome")),
    menuItem('Evolution in and around Benin', tabName = 'limitrophe',icon=icon("fas fa-chart-line",lib="font-awesome")),
    menuItem('About', tabName = 'apropos',icon=icon("fas fa-info-circle",lib="font-awesome")),
    p(br(),br(),em("Realized by Rethis GANSEY"),br(),"Contact me at :",br(),span("eliasgansey@gmail.com", style="color:blue"))))

# dashboardBod
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'dashboard',
            h2('Date :',final_date),
            h2('Some statistics on Benin'),
            fluidRow(
              valueBoxOutput("confirmed", width = 3),
              valueBoxOutput("Deaths", width = 3),
              valueBoxOutput("Incident_Rate", width = 3),
              valueBoxOutput("Mortality_Rate", width = 3)
            ),
            fluidRow(
              box(width = 9, 
                  title = "World map",
                  leafletOutput("carte")),
              p(width = 3,
                p(box(
                  selectInput("var0", "Select a variable",
                              choices =list("World"="All","Africa by Zone"= "afric_zone")))),
                p(box(
                  selectInput("var", "Select a variable",
                              choices =list("Incident Rate"="Incident_Rate","Mortality Rate"= "Mortality_Rate"))))
              ))
    ),
    
    tabItem(tabName = 'evol_benin', 
            h2('Evolution of Covid 19 in Benin'),
            fluidRow(
              box(width = 9,
                  title = "",
                  plotlyOutput("plot_benin1")),
              box(width = 3,
                  selectInput("var1", "Select a variable",
                              choices = list("Confirmed"="Confirmed","Deaths"= "Deaths"))
              ),
              
            ),
            
            fluidRow(
              box(width = 12,
                  title = "",
                  plotlyOutput("plot_benin2"))
              
            )),
    tabItem(tabName = 'limitrophe', 
            h2('Evolution of Covid 19 in Benin and neighbouring countries'),
            
            fluidRow(
              box(width = 9,
                  title = " ",
                  plotlyOutput("plot_lim")),
              box(width = 3,
                  selectInput("var2","Select a variable", choices =list("Confirmed"="Confirmed","Deaths"= "Deaths")))
            )),
    tabItem(tabName = 'apropos', 
            div(class='apropos',
                p("This Dashboard is realized with R and in particular with the Shiny framework."),
                
                h2("Data used :"),
                
                p("The data used comes from the online data repository", a("GitHub repository", href= "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/"), "for data visualization on COVID19 (which contains information on almost every country in the world). This repository is operated by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) and is also supported by the ESRI Living Atlas team and the Johns Hopkins University Applied Physics Laboratory (JHU APL)."),
                
                em("Realized by Rethis Elias GANSEY."),
                br(),
                
                p("Contact me at", span("eliasgansey@gmail.com", style="color:blue")))
            
    )
  )
)


ui <- dashboardPage(header,sider,body)


##################### BACK END OF THE APPLICATION ###################################################

server <-function(input, output){
  # Loading the database
  # For more clarity on the figures, for Benin we remove all dates where confirmed cases are not yet obtained
  base1<-base_coviz[which(base_coviz$Country_Region=="Benin" & base_coviz$Confirmed>0),]
  # We define the values to be displayed
  indic1<-base1$Confirmed
  indic2<-base1$Deaths
  indic3<-round(as.numeric(base1$Incident_Rate),2)
  indic4<-round(as.numeric(base1$Mortality_Rate),2)
  
  #The boxes to display the values
  output$confirmed <- renderValueBox({
    valueBox(
      value = indic1,
      subtitle = "Confirmed cases",
      icon = icon("users"),
      color = "red")
    
  })
  
  output$Deaths <- renderValueBox({
    valueBox(
      value = indic2,
      subtitle = "Deceased cases",
      icon = icon("fa-frown-open"),
      color = "black")
    
  })
  
  output$Incident_Rate <- renderValueBox({
    valueBox(
      value = indic3,
      subtitle = "Incident Rate",
      icon = icon("fas fa-walking",lib = "font-awesome"),
      color = "green")
    
  })
  
  output$Mortality_Rate <- renderValueBox({
    valueBox(
      value = indic4,
      subtitle = "Mortality Rate",
      icon = icon("fas fa-user-md"),
      color = "orange")
    
  })
  
  

  
  
 #Construction of the map 
  output$carte <-renderLeaflet({
    
    variable0<-input$var0
    variable <-input$var
    
    if (variable0=="All"){
      if (variable =="Incident_Rate"){
        mybins <- c(0,100,500,1000,5000,10000,15000,Inf)
        mypalette <- colorBin( palette="YlOrRd",domain=world_spdf@data$Incident_Rate, na.color="transparent", bins=mybins)
        mytext <- paste(
          "Countries: ", world_spdf@data$NAME,"<br/>", 
          "Incidence rate (per 100,000 people): ", world_spdf@data$Incident_Rate, 
          sep="") %>%
          lapply(htmltools::HTML)
        
        # Final Map
        leaflet(world_spdf) %>% 
          addTiles()  %>% 
          setView( lat=10, lng=0 , zoom=2) %>%
          addPolygons( 
            fillColor = ~mypalette(Incident_Rate), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>% addLegend( pal=mypalette, values=~Incident_Rate, opacity=0.9, title = "Incidence rate", position = "bottomleft" )
        
      }
      else if (variable =="Mortality_Rate"){
        mybins <- c(0,1,10,20,30,50)
        mypalette <- colorBin( palette="YlOrRd",domain=world_spdf@data$Mortality_Rate, na.color="transparent", bins=mybins)
        # Prepare the text for tooltips:
        mytext <- paste(
          "Countries: ", world_spdf@data$NAME,"<br/>", 
          "Mortality Rate: ", world_spdf@data$Mortality_Rate, 
          sep="") %>%
          lapply(htmltools::HTML)
        
        # Final Map
        leaflet(world_spdf) %>% 
          addTiles()  %>% 
          setView( lat=10, lng=0 , zoom=2) %>%
          addPolygons( 
            fillColor = ~mypalette(Mortality_Rate), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>% addLegend( pal=mypalette, values=~Mortality_Rate, opacity=0.9, title = "Mortality Rate", position = "bottomleft" )
      }
    }
    else if(variable0=="afric_zone"){
      if (variable =="Incident_Rate"){
        mybins <- c(0,100,500,1000,2000,5000,10000,Inf)
        mypalette <- colorBin( palette="YlOrRd",domain=world_spdf@data$zone_inc, na.color="transparent", bins=mybins)
        mytext <- paste(
          "Zone: ", world_spdf@data$SUBREGION,"<br/>", 
          "Average incidence rate (per 100,000 people): ", world_spdf@data$zone_inc, 
          sep="") %>%
          lapply(htmltools::HTML)
        
        # Final Map
        new_base<-world_spdf@data[world_spdf@data$REGION=="2",]
        leaflet(world_spdf) %>% 
          addTiles()  %>% 
          setView( lat=10, lng=0 , zoom=2) %>%
          addPolygons( 
            fillColor = ~mypalette(world_spdf@data$zone_inc), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>% addLegend( pal=mypalette, values=~zone_inc, opacity=0.9, title = "Average incidence rate", position = "bottomleft" )
        
      }
      else if (variable =="Mortality_Rate"){
        mybins <- c(0,0.01,0.1,1,2,3,10)
        mypalette <- colorBin( palette="YlOrRd",domain=world_spdf@data$zone_death, na.color="transparent", bins=mybins)
        # Prepare the text for tooltips:
        mytext <- paste(
          "Countries: ", world_spdf@data$SUBREGION,"<br/>", 
          "Mortality Rate: ", world_spdf@data$zone_death, 
          sep="") %>%
          lapply(htmltools::HTML)
        
        # Final Map
        leaflet(world_spdf) %>% 
          addTiles()  %>% 
          setView( lat=10, lng=0 , zoom=2) %>%
          addPolygons( 
            fillColor = ~mypalette(zone_death), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>% addLegend( pal=mypalette, values=~zone_death, opacity=0.9, title = "Mortality_Rate", position = "bottomleft" )
      }
    }
  })


  
  output$plot_benin1 <-renderPlotly({
    base_pays_lim%>% filter(Report_Date_String >= "2020-03-13", Country_Region %in% c("Benin"))%>%
      plotly::plot_ly( ) %>%
      plotly::add_trace(
        x = ~Report_Date_String, 
        y = ~get(input$var1), 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F),text = ~paste('</br> Date: ', Report_Date_String,
                                                '</br> Number: ', get(input$var1),
                                                '</br> Countries: ', Country_Region
        )) %>%
      plotly::layout(
        title = input$var1,
        yaxis = list(title = "Cumulative number of cases"),
        xaxis = list(title = "Date"),
        legend = list(x = 0.1, y = 0.9))
    
  })
  
  output$plot_benin2 <- renderPlotly({
    base_pays_lim%>% filter(Report_Date_String >= "2020-03-13", Country_Region %in% c("Benin"))%>%
      plotly::plot_ly( ) %>%
      plotly::add_trace(
        x = ~Report_Date_String, 
        y = ~Confirmed,
        split = "Confirmed cases", 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F),text = ~paste('</br> Date: ', Report_Date_String,
                                                '</br> Number: ', Confirmed,
                                                '</br> Countries: ', Country_Region
        )) %>%
      plotly::add_trace(
        x = ~Report_Date_String,
        y = ~Deaths,
        split = "Deaths cases",
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F),text = ~paste('</br> Date: ', Report_Date_String,
                                                '</br> Number: ', Deaths,
                                                '</br> Countries: ', Country_Region
        )) %>%
      plotly::layout(
        title = "Evolution of COVID19 in Benin",
        yaxis = list(title = "Cumulative number of cases"),
        xaxis = list(title = "Date"),
        legend = list(x = 0.1, y = 0.9))
    
  })
  
  output$plot_lim <-renderPlotly({
    base_pays_lim%>% filter(Report_Date_String >= "2020-03-13", Country_Region %in% c("Benin", "Togo","Burkina Faso","Niger"))%>%
      plot_ly(
        x = ~Report_Date_String, 
        y = ~get(input$var2),
        split = ~Country_Region,
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F),text = ~paste('</br> Date: ', Report_Date_String,
                                                '</br> Number: ', get(input$var2),
                                                '</br> Countries: ', Country_Region)
      ) %>% 
      layout(
        xaxis = list(
          title = "Date",
          zeroline = F,
          dtick = 1
        ),
        yaxis = list(
          title = "",
          zeroline = F
        )
      )
  })
  
  
}


shinyApp(ui, server)
