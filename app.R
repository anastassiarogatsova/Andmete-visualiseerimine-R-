library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(shinyWidgets)
library(psych)
library(corrplot)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(rvest)
library(maps)
library(reshape2)
library(raster)
library(rgdal)
library(sp)
library(plotly)

#Andmete puhastus
andmed=read.csv('main_data.csv', header = T)
andmed <- na.omit(andmed)  
andmed$Status = as.factor(andmed$Status)
andmed$Country = as.factor(andmed$Country)
andmed$Year = as.numeric(andmed$Year)
andmed$Adult.Mortality = as.numeric(andmed$Adult.Mortality)
andmed$infant.deaths = as.numeric(andmed$infant.deaths)
andmed$Hepatitis.B = as.numeric(andmed$Hepatitis.B)
andmed$Measles = as.numeric(andmed$Measles)
andmed$Polio = as.numeric(andmed$Polio)
andmed$Diphtheria = as.numeric(andmed$Diphtheria)
andmed<-andmed[-c(12,19,20)]
andmed<-andmed[-c(8)]
andmed<-andmed[-c(3)]

#Andmete lisamine kaardi loomiseks
world_data = read.csv('iso3.csv', header = T)
andmed['ISO3']<- world_data$iso3[match(andmed$Country, world_data$name)]

#SpatialPolygonDataFram-i lisamine kaardi rakenduse jaoks
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
unzip("world_shape_file.zip")
world_spdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")


CountryChoices <- unique(andmed$Country)
yearChoices <- unique(andmed$Year)
metricChoices <- names(andmed)[3:17]


#------------------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Oodatav eluiga"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Sissejuhatus", tabName = "Sissejuhatus"),
            menuItem(text = "Andmestik", 
                     startExpanded = FALSE,
                     menuSubItem(text = "Kirjeldus", tabName = "DataChar"),
                     menuSubItem(text = "Andmetabel", tabName = "CountryData" ),
                     selectInput(inputId = "countrySelect", label = "Riik:", choices = CountryChoices, selected = "Estonia"),
                     sliderInput("year", label = "Aasta:", min = 2000, max = 2015, value = 2010, sep = "")),
            menuItem(text = "Graakifud",
                     menuSubItem(text = "Tunnuste sõltuvus (riik ja aasta)", tabName = "countryPlot"),
                     selectInput(inputId = "countrySelect2", label = "Riik:", choices = CountryChoices, selected = "Estonia"),
                     selectInput("metricSelect", label = "Tunnus:", choices = metricChoices, selected = "Population"),
                     sliderInput("years", label = "Aastate vahemik:", min = 2000, max = 2015, value = c(2010, 2014), sep = "")
                     
            ),
            menuItem(text = "Andmestiku analüüs", tabName = "analys",
                     menuSubItem(text = "Korrelatsiooni graafikud", tabName = "correlation")
                     ),
            menuItem(text = "Kaart", tabName = "map")
            
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Sissejuhatus",
                    h1(strong("Oodatav eluiga, ülemaailmsed näidikud aastatel 2000 - 2015")),
                    h3("Õppeaine:","Andmete visualiseerimine (Virumaa) - ITB8812"),
                    h3("Õppejõud:", "Olga Dunajeva"),
                    h3("Autor:","Anastassia Rogatšova"),
                    h2(strong("Sissejuhatus")),
                    fluidRow(
                        column(5, imageOutput("image") ),
                        column(7,
                    h3(strong("Projekti kirjeldus:")),
                    h4("Riigi arengu üks peamisi prioriteete on elanikke potentsiaali arendamine ja elutaseme tõstmine. Selleks, nii arengu- kui ka arenenud riigid on huvitatud parematest viisidest oma kodaniku tervisliku seisundi parandamises.", br(), br(),
                       "Oodatava eluea peetakse elanikkonna tervisliku seisundi oluliseks mõõdikuks. Selleks, et pikendada riigi elanikke oodatava eluiga, peavad tervishoiupoliitika kujundajad teadma, millised on peamised eeldatava eluea pikendamise tegurid."), 
                    h4("Antud projekti eesmärk on uurida oodatava eluea mõjutavaid faktoreid ja visualiseerida andmeid. Andmestik on allalaaditud Kaggle.com veebilehe andmestike andmebaasist. Andmestik on .csv formaadis.
                       Esialgses andmestikkus oli 22 tunnust ja 2938 objekte, kuid peale andmestiku puhastamist ja ebavajalikke tunnuste eemaldamist tunnuste ja objektide arv vähenes (kasutatavas andmestikkus on 18 tunnust ja 1649 objekte)."),
                    h4(a("https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who")),
                    h3(strong("Projekti eesmärgid:")),
                    h4("- Anda ülevaade projekti jaoks kasutatud andmestikust;", br(),
                       "- Visualiseerida andmestiku tunnuseid riikide järgi;" , br(),
                       "- Visualiseerida korrelatsioonimaatriksi andmete analüüsi jaoks;", br(),
                       "- Näidata oodatava eluea jaotust riikide järgi kaardi rakenduse abil."), br(),
                                )
                            ),
                        ),
            tabItem(tabName = "DataChar",
                    h1(strong("Andmestiku struktuur ja kirjeldus"),
                    fluidRow(
                        column(3,
                    h3(strong("Tunnused:")),
                    h4("1.	Country (riik)", br(),br(),
                       "2.	Life.expectancy (oodatav eluiga)", br(),br(),
                       "3. 	Adult.Mortality (surmade arv täiskasvanute seas)", br(),br(),
                       "4.	infant.deaths (surmade arv imikute seas)", br(),br(),
                       "5.	Alcohol (alkoholi tarbimine elanikkonna seas)", br(),br(),
                       "6.	BMI (keskmine elanikkonna kehaindeks)", br(),br(),
                       "7.	Population (elanikke arv)",br(),br(),
                       "8.	Diphtheria (difteeria juhtumite arv)", br(),br(),
                       "9.	Schooling (hariduse kestvuse arv)", br(),br(),
                       "10. GDP (Sisemajanduse kogutoodang elaniku kohta)",br(),br(),
                       "11. Year (aasta)", br(),br(),
                       "12.	Hepatitis.B (hepatiit, maksupõletik)", br(),br(),
                       "13.	Measles (leetrid)", br(),br(),
                       "14.	Polio (lastehalvatus)", br(),br(),
                       "15.	Total.expenditure (Valitsuse kulutused tervishoiule)",br(),br(),
                       "16.	HIV.AIDS (Surmajuhtumeid 1000 elussünni kohta HIV/AIDS)", br(),br(),
                       "17.	Income.composition.of.resourses (Inimarengu indeks ressursside sissetulekute koosseis)", br(), br(),
                       "18. ISO3 - teisest admestikkust lisatud rea kaardi rakenduse loomiseks"
                       
                        )
                               ),
                        column(9, 
                               br(),
                               verbatimTextOutput("str_andmed"), br(), 
                               verbatimTextOutput("summary")
                        
                              )
                    )
                       )
                    ),
            tabItem(tabName = "CountryData",
                    h1(strong("Andmetabel")),
                    DTOutput("dataTable2"), style = "overflow-x: scroll;",
                    DTOutput("dataTable")
                    
                    ),
            tabItem(tabName = "countryPlot",
                    h1(strong("Graafikud")),
                    plotlyOutput("yearPlotCountry"),
                    plotOutput("yearBoxplotCountry"),
                    plotOutput("boxplotCountries")
            ),
            
            tabItem(tabName = "correlation",
                    h1(strong("Andmestiku analüüs")),
                        fluidRow(
                            column(2,
                                   prettyRadioButtons("rb", h3("Graafiku valik:"),
                                                choiceNames = list(
                                                    tags$span(style = "font-size: 15px", "Sõltuvuste tabel"),
                                                    tags$span(style = "font-size: 15px", "Korrelatsiooni maatriks")
                                                ),
                                                choiceValues = list("solt", "corr"), bigger = TRUE, fill = TRUE, selected = "solt"
                                                )),
                            column(10,
                            plotOutput("rbPlot", width = "100%", height = "700px")
                                   )
                        )),
            
            tabItem(tabName = "map",
            tags$style(type = 'text/css', '#mymap {height: calc(100vh - 80px) !important;}'),
            fluidRow(column(12, align = "center", selectInput(inputId = "mapYear", label = "Valige aasta:", choices = yearChoices, selected = "2014"))),
            
            leafletOutput('mymap' ))
            
            
          
                    
        )
    )
)

#------------------------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output){

  output$image <- renderImage({

    list(src = "www/image.jpeg", contentType = 'image/jpeg',width = 500, height = 300,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
   
    
    # Rendering summary
    output$summary <- renderPrint({
        summary(andmed)
    })
    
    # Rendering str(andmed)
    output$str_andmed <- renderPrint({
        str(andmed)
    })
    
    yearData <- reactive({
        
        filteredData <- subset(andmed, Year == input$Year)
        
        final <- filteredData[order(filteredData[input$metricSelect], decreasing = T), ]
        final2 <- final[1:10,]
        
        final2$Coutry <- factor(final2$Country,levels = unique(final2$Country))
        
        return(final2)
        
    })
    
    CountryData <- reactive({
        
        filteredData <- subset(andmed, Year == input$year)
        
        return(filteredData)
        
    })
    
    CountryData2 <- reactive({
      
        filteredData <- subset(andmed, Country == input$countrySelect2 )
        print(filteredData)
        
        return(filteredData)
        
    })
    
    CountryYearData <- reactive({
        
        filteredData2 <- subset(andmed, Year == input$year & Country == input$countrySelect)
        
        return(filteredData2)
        
    })
    
 
    
    
    output$topDEC <- renderPlotly({
        if (input$topYear == "2015"){
            filteredData2 <- subset(andmed, andmed$Year == input$topYear)
            plot <- ggplot(tail(filteredData2,2), mapping = aes_string(x = "Country", y = sort(filteredData2$Life.expectancy, decreasing = FALSE)[1:2])) 
            plot <- plot + geom_col(fill = "#F7A711") 
            plot <- plot + ggtitle(paste0("Top 10 riigi madalama oodatava elueaga ", input$topYear, " aastal" ))
            plot  
        }
        else if(input$topYear != "2015"){
            filteredData2 <- subset(andmed, andmed$Year == input$topYear)
            plot <- ggplot(tail(filteredData2,10), mapping = aes_string(x = "Country", y = sort(filteredData2$Life.expectancy, decreasing = FALSE)[1:10])) 
            plot <- plot + geom_col(fill = "#F7A711") 
            plot <- plot + ggtitle(paste0("Top 10 riigi madalama oodatava elueaga ", input$topYear, " aastal" ))
            plot
            
            }
        
    })
    
    output$dataTable2 <- renderDT({
        
        data <- CountryYearData()
        
        datatable(data, options = list(pageLength = nrow(data)))
        
    })
 
    output$dataTable <- renderDT({
        
        data <- CountryData()
        
        datatable(data, options = list(pageLength = 20))
        
    })
    
    output$yearPlotCountry <- renderPlotly({
      
        plot <- ggplot(CountryData2(), mapping = aes_string(x = "Year", y = input$metricSelect))+xlim(input$years[1]-1, input$years[2]+1) 
        plot <- plot + geom_col(fill = "#0181ad") 
        plot <- plot + ggtitle(paste0(input$metricSelect, " Per Year For ", input$countrySelect2))
        plot
        
    })
    
    output$yearBoxplotCountry <- renderPlot({
        
        plot <- ggplot(CountryData2(), mapping = aes_string(x = "Year", y = input$metricSelect))+xlim(input$years[1]-1, input$years[2]+1) 
        plot <- plot + geom_boxplot(fill = "#01ace4") + coord_flip() + scale_y_log10()
        plot <- plot + ggtitle(paste0(input$metricSelect, " Per Year For ", input$countrySelect2))
        plot
    })
    
    
    output$boxplotCountries <- renderPlot({
        
        plot <- ggplot(andmed, mapping = aes_string(x = "Year", y = input$metricSelect))+xlim(input$years[1]-1, input$years[2]+1) 
        plot <- plot + geom_boxplot(fill = "#b3ebfe") + coord_flip() + scale_y_log10()
        plot <- plot + ggtitle(paste0(input$metricSelect, " erindid kõike riikide kohta (summaarselt)"))
        plot
    })
    
    
    output$rbPlot <- renderPlot({
        
    if(is.null(input$rb)){
        par(bg = "#ECF0F5")
        pairs.panels(Filter(is.numeric,andmed))
          }
    else if (input$rb=="solt"){
        par(bg = "#ECF0F5")
        pairs.panels(Filter(is.numeric,andmed))
          }
     else if (input$rb == "corr"){
        par(bg = "#ECF0F5")
        corrplot(cor(Filter(is.numeric, andmed)), method = 'circle', type = 'lower', insig='blank',
                 addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

     }
    })
    
    output$mymap <- renderLeaflet({
        
        #Oodatava eluiga tunnuse lisamine SpatialPolygonDataFram-i kaardi rakenduse jaoks
        andmed_merge <- subset(andmed, andmed$Year == input$mapYear)
        world_spdf@data$Life.expectancy <- andmed_merge$Life.expectancy[match(world_spdf@data$ISO3, andmed_merge$ISO3)]
        
        bins=c(50,55,60,65,70,75,80,85,90,Inf)
        pal = colorBin(palette = "YlOrBr", domain=world_spdf$POP2005, na.color = "transparent", bins=bins)
        
        customLabel = paste("Riik: ", world_spdf$NAME, "<br/>", "Oodatav eluiga ", input$mapYear, " aastal: ", round(world_spdf$Life.expectancy, 2), sep = "") %>% 
            lapply(htmltools::HTML)
        
        varname<-switch(input$mapYear,
                        "2015" = "Oodatav eluiga 2015 aastal",
                        "2014" = "Oodatav eluiga 2014 aastal",
                        "2013" = "Oodatav eluiga 2013 aastal",
                        "2012" = "Oodatav eluiga 2012 aastal",
                        "2011" = "Oodatav eluiga 2011 aastal",
                        "2010" = "Oodatav eluiga 2010 aastal",
                        "2009" = "Oodatav eluiga 2009 aastal",
                        "2008" = "Oodatav eluiga 2008 aastal",
                        "2007" = "Oodatav eluiga 2007 aastal",
                        "2006" = "Oodatav eluiga 2006 aastal",
                        "2005" = "Oodatav eluiga 2005 aastal",
                        "2004" = "Oodatav eluiga 2004 aastal",
                        "2003" = "Oodatav eluiga 2003 aastal",
                        "2002" = "Oodatav eluiga 2002 aastal",
                        "2001" = "Oodatav eluiga 2001 aastal",
                        "2000" = "Oodatav eluiga 2000 aastal")
        
        leaflet(world_spdf) %>%
            
            addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
            
            addPolygons(fillColor = ~pal(world_spdf@data$Life.expectancy),
                        fillOpacity = 0.9,
                        stroke = TRUE,
                        color="white",
                        highlight=highlightOptions(
                            weight = 5,
                            fillOpacity = 0.3
                        ),
                        label = customLabel,
                        weight = 0.3,
                        smoothFactor = 0.2) %>%
            
            addLegend(
                pal=pal,
                values = world_spdf@data$Life.expectancy,
                position = "topright",
                title = varname
            )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
