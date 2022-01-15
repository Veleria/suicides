# install.packages("shiny")
# install.packages("plotly")
library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(dplyr)
library(summarytools)
library(countrycode)


#-------------------------------------- ANDMED -----------------------------------------#

#https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016

data <- read.csv("data/suicide.csv", header = T, sep = ",") #, encoding = "UTF-8"
names(data)[1] <- "country"
names(data)[10] <- "gdp_year"
names(data)[11] <- "gdp_capita"
data["country.year"] <- NULL
data["HDI.for.year"] <- NULL
data = data[data$year != "2016",]
data <- na.omit(data)


data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))
data$sex <- factor(data$sex, levels = unique(data$sex))
data$generation <- factor(data$generation, levels = unique(data$generation))
# data$CODE = countrycode(data$country, 'country.name', "iso3c") #lisan riigi koodi map joonistamiseks



# #-------------- menshe dannyh na server------------------
# data["gdp_year"] <- NULL
# data["gdp_capita"] <- NULL
# data = data[data$year > 2000,]
# 
# data$continent <- countrycode(sourcevar = data[, "country"],
#                             origin = "country.name",
#                             destination = "continent")
# data = data[data$continent == "Europe",]
# data["continent"] <- NULL
# write.csv(data, "data/suicide_server.csv")
# data <- NULL
# data <- read.csv("data/suicide_server.csv", header = T, sep = ",") #, encoding = "UTF-8"
# #--------------------------------------------



struktuur <- str(data)
s <- summary(data)

uniq_year <- sort(unique(data$year))
uniq_country <- sort(unique(data$country))
min_year = min(uniq_year)
max_year = max(uniq_year)
mean_all = round((sum(data$suicides_no) / sum(data$population)) * 100000, 2)

set.seed(3) # just to get the same result
x <- sample(nrow(data),50)
d=data[x,]

# grouped_country <- data %>% group_by(country)
# grouped_year <- data %>% group_by(year) 
# # grouped_generation <- data %>% group_by(generation)
# grouped_age <- data %>% group_by(age)
# grouped_sex <- data %>%  select(country, year, sex, suicides_no) %>% group_by(country, year, sex) %>% summarize(sum = as.integer(sum(suicides_no)))

filtered_map <- data %>%  
    group_by(country) %>%
    summarize(mean = round(mean(suicides.100k.pop),2)) %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c"))


#-------------------------------------------------------------------------------- LIIDES ---------------------------------------------------------------------#


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("www/styles.css"),

    navbarPage(
        "Suicide Rates", 
        id = "main_navbar",
        
        #---------- Peamine leht ------------#
        
        tabPanel(
            "Kirjeldus",
            titlePanel(paste("Enesetappude määrade ülevaade ", min_year," kuni ", max_year, sep ="")),hr(),
            sidebarPanel(
                p(img(src = 'pink.png',height="50%", width="80%")),
                helpText(HTML("Andmete visualiseerimise projekt<br>Valeria Juštšenko")),
                p("Eesmärgid........................................................................."), br(),
                h2("Rakenduse kirjeldus"),
                a(href = "https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016", "Link andmestikule", target = "_blank"),br(),
                
                # h4("Kuna andmestik on suur, nouab aega andmete laadimiseks"),
                p("See andmestik pärineb neljast teisest aja ja kohaga
                seotud andmekogumist ning loodi selleks,
                  et leida signaale, mis on korrelatsioonis suurenenud enesetappude
                  määraga erinevate rühmade vahel kogu sotsiaal-majandusliku spektri
                  ulatuses."), 
                p(textOutput("rows")), 
                p("Tab 1 sisaldab andmetabeli, selle struktuuri ja lühikokkuvõte kirjeldus, "),
                p("Tab 2 sisaldab kaart andmete visualiseerimiseks"),
                p("Tab 3 sisaldab maailma statistika visualiseerimine"),
                p("Tab 4 sisaldab statistika visualiseerimine riikide kaupa"),
                # p("Kaartide joonistamiseks oli kasutatud plotly teek"),
                 h4("Tunnused"),
                p(HTML("country - riik <br> year - aasta<br> sex - sugu<br> age - vanuserühm<br>
                suicides_no - enesetappude arv<br>
                population - elanikkond (vanuserühma keskmise põhjal) <br>
                suicides.100k.pop - tähistab enesetapu põhjustatud surmajuhtumite arvu kokku 100 000 surma korral <br>
                gdp_for_year - Sisemajanduse kogutoodang (SKT) aastas <br>
                gdp_per_capita - SKT elaniku kohta on mõõdik, mis jagab riigi SKT inimese kohta ja arvutatakse riigi SKT jagamisel selle rahvaarvuga. <br>
                generation - põlvkond (vanuserühma keskmise põhjal).")),
                p("Olid kustutatud HDI for year (aasta inimarengu indeks) ja country.year (country-year liitvõti) tunnused.")),br(),
            
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Struktuur", h3("Struktuur"), verbatimTextOutput("str")),
                    tabPanel("Summary", h3("Summary"), verbatimTextOutput("summ")),
                    tabPanel("Summarytools", h3("Summary (summarytools teek)"), uiOutput("summary")),
                    tabPanel("Korrelatsioon", h3("Korrelatsioon"), plotOutput("pm"))
                ),
                
                #----------  Tabel  ----------#
                radioButtons("view", "Näidata andmed:", c("Ei" = "0", "Jah" = "1")),
                conditionalPanel(condition = "input.view == 1",
                                 h3("Andmed"),
                                 DT::dataTableOutput("tabelFiltered")
                )
            )
        ), 
        
        #------------------ Kaart plotly---------------------#
        
        tabPanel(
            "Kaart",
            titlePanel("Maailma enesetappude määrade ülevaade. Kaart."),hr(),
            sidebarPanel(
                h4(paste("Kokku enesetappude arv alates ", min_year," to ", max_year, ' (keskmine 100 000 elaniku kohta)', sep ="")), 
                
                br(),
                p('Source:',  a(href = "https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016", "Link andmetele", target = "_blank")),
                p("Sellel vahelehel kuvatakse kuidas paigaldatakse enesetappude arv kokku voi keskmine 100 000 elaniku kohta (vaikimisi)"),
                p("Samuti on võimalik vaadata statistika määratud vahemikel."),
                p("Kaartide joonistamiseks oli kasutatud plotly teek. Ka oli proovitud highcharter teek, aga see tegi rakendus aeglasemaks."),
                selectInput(
                    'tunnus',
                    'Select Tunnus',
                    choices =  c("Enesetapud 100 000 kohta" = "suicides.100k.pop", "Enesetapud kokku" = "suicides_no"),
                    selected = "suicides.100k.pop"
                ),
                sliderInput("slider_years_map", label = h3("Slider Range"), 
                            min = min_year,
                            max = max_year,
                            step = 1,
                            value = c(min_year, max_year))
            ),
            mainPanel(
                h3(textOutput("year1")), # Enesetappude määrade ülevaade aastate kaupa
                plotlyOutput('map'),
                h3(textOutput("year2")), #, "riikide loigus"
                plotlyOutput('barchart'),
            )
        ),  
        
        #---------- Statistika proov ----------#

        tabPanel(
          "proov",
          titlePanel(paste("Proov")),hr(),
          sidebarPanel(
            checkboxInput("OneMore", label = h5("Kas lisada riigid?"), F),
            conditionalPanel(
              condition = "input.OneMore == 1",
              h3("Andmed"),
              selectizeInput('riigid','Riigid',choices = c("All", uniq_country), multiple = TRUE, selected = "Estonia", options = list(maxItems = 5)
              ),
            ),
          ),
          mainPanel(
            
            fluidRow(
              column(width=6,
                     plotlyOutput('proov')
              ),
              column(width=6,
                     plotlyOutput("yearPlot"),
              )
            ),
            
            
                    # plotlyOutput('proov'),
                    conditionalPanel(
                      condition = "input.OneMore == 1",
                      h3("Andmed"),
                      # plotlyOutput('proov')
                    ),conditionalPanel(
                      condition = "input.OneMore == 0",
                      h3("Andmed"),
                      # plotlyOutput("years_trendine")
                    )
                    )
          
        ), 
        
        #---------- Statistika by World ----------#
        
        tabPanel("Maailma statistika",
                 titlePanel(paste("Maailma enesetappude määrade ülevaade ", min_year," to ", max_year, sep ="")),hr(),
                 sidebarPanel(
                     p("Enesetappude määrade ülevaade (100 000 elaniku kohta)"), hr(),
                     # h3("Sugu riikide ja aastate kaupa"),
                     # plotlyOutput("age_by_world")
                     p("Trendijoon"),
                     p("Sugu"),
                     p("Vanus"),
                     p("Põlvkond"),
                 ),
                 mainPanel(
                   tabsetPanel(
                     type = "tabs",
                   tabPanel("Trendijoon",
                            h3(paste("Globaalsed enesetapud (100 000 kohta) ",min_year," kuni ",max_year,' aastate jooksul',sep = "")),
                              
                            fluidRow(  
                              column(width=8,
                                     h3("Trendijoon maailmas"), plotlyOutput("years_trendine")),
                              column(width=4,
                            ))),
                   
                   
                     
                   tabPanel("Sugu",
                      fluidRow(  
                        column(width=8,
                            h3("Trendijoon sugu järgi"), plotlyOutput("trend_sex")),
                        column(width=4,
                            h3("Sugu"), plotlyOutput("gender_all"))  )
                   ),
                   
                   tabPanel("Vanus",
                            fluidRow(
                              column(width=8,
                                     h3("Vanus"), plotlyOutput("trend_age")  ),
                              column(width=4,
                                     h3("Trendijoon vanuse järgi"), plotlyOutput("age_pie_world") 
                                     # plotlyOutput("age_bar_world")
                                     )
                            )),
                   tabPanel("Põlvkond",
                            fluidRow(
                              column(width=8,
                                     h3("Trendijoon põlvkonna järgi"), plotlyOutput("trend_generation") ),
                              column(width=4,
                                     h3("Põlvkond"), plotlyOutput("generation_all") )
                            )),
                   
                 )
                 )
        ),
        
        

        
        #---------- Statistika riikide kaupa-----------#
        
        tabPanel(
            "Statistika riikide kaupa",
            titlePanel("Enesetappude määrade ülevaade aasta ja riigi kaupa"), hr(),
            sidebarPanel(
                h3("Riigid"),
                selectInput(
                    "country",
                    "Country:",
                    choices =  uniq_country,
                    selected = "Estonia"
                ), 
                selectInput(
                    "tunnus_all",
                    "Tunnused:",
                    # choices =  c("suicides.100k.pop", "suicides_no",  "population"),
                    choices =  c("Enesetapud 100 000 kohta" = "suicides.100k.pop", "Enesetapud" = "suicides_no"),
                    selected = "suicides.100k.pop"
                ),  hr(),
                # selectInput(
                #     "suicides_by_year",
                #     "Years:",
                #     choices =  uniq_year,
                #     selected = 2010
                # ),
                sliderInput("slider_years_gpaph", label = h3("Aastate piirkond"),
                            min = min_year,
                            max = max_year,
                            step = 1,
                            value = c(min_year, max_year))
            ),
            mainPanel(
                       h3(paste("Globaalsed enesetapud (100 000 kohta) ", min_year," kuni ", max_year, ' aastate jooksul', sep ="")),

              fluidRow(
                column(width=8,
                       # h3(paste("Generation tunnuse ", selectInput$generation_by_year," ja ", selectInput$generation_by_year, ' aastate kaupa', sep ="")),
                       h3("Trendijoon sugu järgi"), plotlyOutput("yearsLine"),
                       ),
                #---------- Statistika sugu kaupa-----------#       
                column(width=4, h3("Vanuse, riikide ja aastate kaupa. Proov2"),plotlyOutput('proov2'))  
                ),
              
              fluidRow(
                  #---------- Statistika vanuse kaupa-----------#
                  column(width=4,h3("Vanuse jargi"), plotlyOutput("age_pie")),  # plotlyOutput("age_bar")
                  #---------- Statistika sugu kaupa-----------#       
                  column(width=4, h3("Sugu, riikide ja aastate kaupa"), plotlyOutput("gender")),  
              #     ),
              # fluidRow(  
                  # column(width=4, h3("Proov2"),  plotlyOutput("age_bar")),
                   #-------- Statistika  generation kaupa---------#
                  column(width=4,  h3("Polvkonna ja aastate kaupa"), plotlyOutput("generation"))  ),
              ),
    )))



#-------------------------------------------------------------------- SERVERI OSA --------------------------------------------------------------------------#

server <- function(input, output) {
  
          #------------------------ Proov --------------------------#
          
          filtered_proov <- reactive({
            data %>%
              # select(country, year, age, sex, suicides.100k.pop) %>%
              filter(country == input$country) %>%
              filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
              group_by(age, sex) %>%
              summarize(sum = round(sum(.data[[input$tunnus_all]]))) #round(mean(suicides.100k.pop), 2)
          })
        
          output$proov2 <- renderPlotly({
            title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'}
            bar_age <- plot_ly(
              filtered_proov()%>% filter(sex == 'male'),
              x = ~ age,
              y = ~ sum,
              type = 'bar', 
              name = 'mees'
            )
            bar_age <- bar_age %>% add_trace(data=filtered_proov()%>% filter(sex == 'female'), values = ~sum, labels = ~age, textinfo='label+percent', name="naine")
            bar_age <- bar_age %>% layout(yaxis = list(title = title), xaxis = list(title = 'Vanus'), barmode = 'stack')
          })
          
          # output$proov <- renderPlotly({
          #   plot_ly(filtered_proov(), x =~sum, y =~age, color = ~uniq_year, colors = "Accent") %>%
          #   add_histogram()
          # })
          
          
          
          filtered_riik <- reactive({
            validate(
              need(input$riigid != "", "Palun valige riik")
            )
            data %>%  
              filter(country == input$riigid) %>%
              group_by(country, year) %>% 
              summarize(mean = round((sum(suicides_no) / sum(population)) * 100000, 2)) # mean(suicides.100k.pop))
          })
          
          output$proov <- renderPlotly({
            if(nrow(filtered_riik()) == 0) return()
            trendine <-
              plot_ly(
                filtered_riik(),
                x = ~ year,
                y = ~ mean,
                name = "keskmine", 
                color = ~mean,
                type = 'scatter',
                mode = 'lines+markers'
              )
            trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad'))%>% hide_legend()
            trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                               hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine") # %>%   
            # trendine <- trendine %>%   layout(text = mean_all)
            # hide_colorbar(trendine)
          })
  
                #------------------------ Tekst --------------------------#
  
  output$year1 <- renderText({ 
    paste("Enesetappude määrade ülevaade aastate kaupa: ", input$slider_years_map[1], " kuni ", input$slider_years_map[2], " (keskmine 100 000 elaniku kohta)", sep ="")
  })
  
  output$year2 <- renderText({
    paste("Enesetappude määrade ülevaade aastate kaupa: ", input$slider_years_map[1], " kuni ", input$slider_years_map[2], " (keskmine 100 000 elaniku kohta)"," riikide lõigus", sep ="")
  })
  
  output$rows <- renderText({
    paste0("Kirjet kokku: ", nrow(data), sep ="")
  })
  
    
                #------------------------ Peamine leht --------------------------#
    
    #---------- Tabel -----------#
    output$tabelFiltered <- DT::renderDataTable({
        data
    })
    
    #---------- Struktuur -----------#
    output$str <- renderPrint({
        str(data)
    })
    
    #---------- SUMMARY -----------#
    output$summ <- renderPrint({
        s
    })
    
    #----- SUMMARY by summarytools -----#
    
    output$summary <- renderUI({
        data %>%
            dfSummary(varnumbers = FALSE, valid.col = FALSE, graph.magnif = 0.8) %>%
            print(method = "render", headings = TRUE, Data.frame = NULL,
                  footnote = NA, bootstrap.css = FALSE)
    })
    
    #---------- Сorrelation -----------#
    # library(GGally);
    # library(ggplot2)
    # library(ggcorrplot)
    
    
    library(corrplot)
    output$pm <- renderPlot({
        table = cor(Filter(is.numeric, data))
        corrplot(table, method = "color",   
                 type = "lower", order = "hclust", 
                 addCoef.col = "black",
                 tl.col="black", tl.srt = 45, 
                 sig.level = 0.01, insig = "blank",
                 diag = FALSE )

        # pm <- ggpairs(data = Filter(is.numeric, data),
        #               mapping = aes(color = data$gdp_year),
        #               columns = c("suicides_no", "population", "suicides.100k.pop", "gdp_capita"))
        # pm
        
    })
    
                #----------------- Kaart. Plotly by few years -------------------#

    total <- reactive({
        data %>%
            filter(year >= input$slider_years_map[1] & year <= input$slider_years_map[2]) %>%
            group_by(country) %>%
            summarize(mean = round(mean(.data[[input$tunnus]]),2)) %>%
            mutate(iso3 = countrycode(country, "country.name", "iso3c"))
    })
    
    total_sum <- reactive({
      data %>%
        filter(year >= input$slider_years_map[1] & year <= input$slider_years_map[2]) %>%
        group_by(country) %>%
        summarize(mean = round(sum(.data[[input$tunnus]]),2)) %>%
        mutate(iso3 = countrycode(country, "country.name", "iso3c"))
    })
    
    output$map <-  renderPlotly({
      title = if(input$tunnus=='suicides.100k.pop'){'Keskmine'} else {'Kokku'}
      l =  list(color = toRGB("grey"), width = 0.7)
      if(input$tunnus=='suicides.100k.pop')
        {fig <- plot_geo(total())}
      else
        {fig <- plot_geo(total_sum())}
        
        # fig <- plot_geo(total())
        fig <- fig %>% add_trace(
            type="choroplethmapbox",
            z = ~ mean,
            color = ~ mean,
            # colors = 'Blues',
            text = ~ country,
            locations = ~ iso3,
            marker = list(line = l),showlegend = FALSE
        )
        fig <- fig %>% colorbar(title = title)
        fig <- fig %>% layout(
            dragmode = "Zoom",
            # autosize = F, 
            # width = 1000, height = 1000,
            mapbox=list(
                style="carto-positron",
                # title = 'Enesetappude määrade ülevaade aastatel 1985–2016 ', textposition ="top center",
                zoom =2,
                center=list(lon= -95.71, lat=37.09))
        )
        fig
    })
    
    #---- Kaart. Plotly barchart by country few years -----#

    output$barchart <-  renderPlotly({
      title = if(input$tunnus=='suicides.100k.pop'){'Enesetappu 100 000 \nelaniku kohta'} else {'Enesetapud kokku'}
      title2 = if(input$tunnus=='suicides.100k.pop'){'Keskmine'} else {'Kokku'}
      if(input$tunnus=='suicides.100k.pop')
      {
        barchart <- total() %>%
          arrange(mean)  %>%
          # head(10) %>%
          plot_ly(x = ~country, y = ~mean, color = ~mean, type = "bar", colors = "viridis",showlegend = FALSE)
        barchart <- barchart %>% colorbar(title = title2)  %>% layout(yaxis = list(title = title), xaxis = list(title = 'Riigid'), barmode = 'stack')
        }
      else
      {
        barchart <- total_sum() %>%
          arrange(mean)  %>%
          # head(10) %>%
          plot_ly(x = ~country, y = ~mean, color = ~mean, type = "bar", colors = "viridis",showlegend = FALSE)
        barchart <- barchart %>% colorbar(title = title2) %>% layout(yaxis = list(title = title), xaxis = list(title = 'Riigid'), barmode = 'stack')
        }
      
      # barchart <- total() %>%
      #       arrange(mean)  %>%
      #       # head(10) %>%
      #       plot_ly(x = ~country, y = ~mean, color = ~mean, type = "bar", colors = "viridis")
      # barchart <- barchart %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Riigid'), barmode = 'stack')

    })

    
    #---------------------------------- Statistika by World ----------------------------------#
                #----------------- Statistika aastate kaupa ------------------#
    
    filtered_yearsLine <- reactive({
        data %>%  
            group_by(year) %>% 
            summarize(mean = round((sum(suicides_no) / sum(population)) * 100000, 2)) # mean(suicides.100k.pop))
    })
    
    output$years_trendine <- renderPlotly({
        trendine <-
            plot_ly(
                filtered_yearsLine(),
                x = ~ year,
                y = ~ mean,
                name = "keskmine", 
                color = ~mean,
                type = 'scatter',
                mode = 'lines+markers'
            )
        trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad'))%>% hide_legend()
        trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                           hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")    
        # hide_colorbar(trendine)
    })
    
                #----------- Statistika aastate ja riigi kaupa ---------------#
    filtered_years_by_country <- reactive({
      
        data %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            filter(country == input$country) %>%
            group_by(year) %>%
            summarize(mean = mean(.data[[input$tunnus_all]]))  # summarize(mean = round((sum(suicides_no) / sum(population)) * 100000, 2))
    })
    
    filtered_years_by_country_sum <- reactive({
      data %>%
        filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
        filter(country == input$country) %>%
        group_by(year) %>%
        summarize(mean = round(sum(.data[[input$tunnus_all]]),2)) 
    })
    
    output$yearsLine <- renderPlotly({
      title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku, inim'}

      years_trend <-
            plot_ly(
              if(input$tunnus_all=='suicides.100k.pop')
              {filtered_years_by_country()}
              else
              {filtered_years_by_country_sum()},
                x = ~ year,
                y = ~ mean,
                color = ~mean,
                type = 'scatter',
                mode = 'lines+markers',
              hovertemplate = paste(
                "%{x:.0f} aastas <br>%{y:.0f} %{yaxis.title.text}"
              )
            )
        years_trend <- years_trend %>% layout(yaxis = list(title = title), xaxis = list(title = 'Aastad'))
        
        hide_colorbar(years_trend)
    })
    
    
    #--------------------- Satatistika sugu kohta / Sex ----------------------#
    
    filtered_sex <- reactive({
      data %>%
        select(year, sex, suicides.100k.pop) %>%
        # filter(sex == 'male') %>%
        group_by(sex, year) %>%
        summarise(mean = round(mean(suicides.100k.pop),2))
    })
    
    output$trend_sex <- renderPlotly({
      woman <-  filtered_sex() %>% filter(sex == 'female')
      man <-  filtered_sex() %>% filter(sex == 'male')
      trendine <-
        plot_ly(
          man,
          x = ~ year,
          y = ~ mean,
          # values = ~ sex,
          # color = ~mean,
          type = 'scatter',
          mode = 'lines+markers', name="Mees"
        )
      trendine <- trendine %>% add_trace(data=woman, values = ~mean, labels = ~sex, textinfo='label+percent', name="Naine") 
      trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) 
      trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                         hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")
    })
    
    
    #--------------------- Satatistika vanuse kohta / Age ----------------------#
                #---------Vanus kokku   ----------#
    
    filtered_age_world <- reactive({
        data %>%
            group_by(age) %>%
            summarise(mean = round(mean(suicides.100k.pop), 2)) 
    })

    output$age_pie_world <- renderPlotly({
        pie_age <- filtered_age_world() %>% 
            plot_ly(type='pie', labels = ~age, values = ~mean, textinfo='label+percent')
    })

    output$age_bar_world <- renderPlotly({
        bar_age <- plot_ly(
            filtered_age_world(),
            x = ~ age,
            y = ~ mean,
            type = 'bar',
            name = 'Sum'
        )
        bar_age <- bar_age %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Vanus'), barmode = 'stack')
    })
    
    filtered_trend_age <- reactive({
      data %>%
        select(year, age, suicides.100k.pop) %>%
        group_by(age, year) %>%
        summarise(mean = as.integer(mean(suicides.100k.pop))) # mean(suicides.100k.pop))
    })
    

    output$trend_age <- renderPlotly({
      trendine <-
        plot_ly(
          filtered_trend_age() %>% filter(age == '5-14 years'),
          x = ~ year,
          y = ~ mean,
          # values = ~ sex,
          # color = ~mean,
          type = 'scatter',
          mode = 'lines+markers', name="5-14 years"
        )
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '15-24 years'), values = ~mean, labels = ~age, textinfo='label+percent', name="15-24 years") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '25-34 years'), values = ~mean, labels = ~age, textinfo='label+percent', name="25-34 years") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '35-54 years'), values = ~mean, labels = ~age, textinfo='label+percent', name="35-54 years") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '55-74 years'), values = ~mean, labels = ~age, textinfo='label+percent', name="55-74 years") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '75+ years'), values = ~mean, labels = ~age, textinfo='label+percent', name="75+ years") #y=~mean, 
      
      trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) #%>% hide_legend()
      hide_colorbar(trendine)
      trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                         hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")
    })
    

                #---------Vanus aastate ja riigi kaupa ----------#
    filtered_age_by <- reactive({
        data %>%
        select(country, year, age, suicides.100k.pop) %>%
            filter(country == input$country) %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            group_by(age) %>%
            summarize(mean = round(mean(.data[[input$tunnus_all]]))) #round(mean(suicides.100k.pop), 2)
    })
    
    filtered_age_by_sum <- reactive({
      data %>%
        select(country, year, age, suicides_no) %>%
        filter(country == input$country) %>%
        filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
        group_by(age) %>%
        summarize(mean = round(sum(.data[[input$tunnus_all]]))) #round(mean(suicides.100k.pop), 2)
    })
    
    output$age_pie <- renderPlotly({
      title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'}
        
      
      pie_age <- 
        if(input$tunnus_all=='suicides.100k.pop')
        {filtered_age_by()}
      else
      {filtered_age_by_sum()} 
      pie_age <- pie_age %>%  plot_ly(type='pie', labels = ~age, values = ~mean, textinfo='label+percent', domain= list(row = 0, column=0))
        pie_age <- pie_age %>%
            layout(title = title,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
 
    output$age_bar <- renderPlotly({
        title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'}
        bar_age <- plot_ly(
            filtered_age_by(),
            x = ~ age,
            y = ~ sum,
            type = 'bar',
            name = 'Sum'
        )
        # bar_age <- bar_age %>% add_trace(data=filtered_age_by()%>% filter(age == '75+ years'), values = ~sum, labels = ~age, textinfo='label+percent', name="Estonia")
        bar_age <- bar_age %>% layout(yaxis = list(title = title), xaxis = list(title = 'Vanus'), barmode = 'stack')
    })


    # output$age_both <- renderPlotly({
    #     plot_ly(filtered_age_by(), x = ~ age, y = ~sum, type = 'bar') %>%
    #         layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Vanus',domain = c(0, 0.5)), barmode = 'stack') %>%
    #         add_trace(filtered_age_pie(), labels = ~age, values = ~sum, textinfo='label+percent', type = 'pie', 
    #                   domain = list(x = c(0.5, 1)))
    #     # subplot(list(pie_age, bar_age))  #  %>% hide_legend(), nrows=2, margin = 0.05
    # })
    


    
    #------------------------------- generation --------------------------------#
    
    filtered_generation_all <- reactive({
        data %>%
            group_by(generation) %>%
            summarize(sum = round(sum(suicides.100k.pop)))
    })

    output$generation_all <- renderPlotly({
        fig5 <-plot_ly(
            filtered_generation_all(),
            x = ~ generation,
            y = ~ sum,
            type = 'bar',
            name = 'Summa by generation'
        )
        fig5 <- fig5 %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Polvkonna nimetus'))
    })
    
              #------ generation by tunnuste kaupa ------#
    filtered_generation <- reactive({
        data %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            filter(country == input$country) %>%
            group_by(generation) %>%
            summarize(sum = round(sum(.data[[input$tunnus_all]])))
    })
    
    output$generation <- renderPlotly({
      title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'}
      generation_bar <- plot_ly(
            filtered_generation(),
            x = ~ generation,
            y = ~ sum,
            type = 'bar',
            name = 'Summa by generation'
        )
      generation_bar <- generation_bar %>% layout(yaxis = list(title = title), xaxis = list(title = 'Polvkond'), barmode = 'stack')
        
    })
    
    filtered_trend_generation <- reactive({
      data %>%
        select(year, generation, suicides.100k.pop) %>%
        group_by(generation, year) %>%
        summarise(mean = round(mean(suicides.100k.pop))) # mean(suicides.100k.pop))
    })
    
    
    output$trend_generation <- renderPlotly({
      trendine <-
        plot_ly(
          filtered_trend_generation() %>% filter(generation == 'Silent'),
          x = ~ year,
          y = ~ mean,
          type = 'scatter',
          mode = 'lines+markers', name="Generation X"
        )
      trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                         hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")
      trendine <- trendine %>% add_trace(data=filtered_trend_generation() %>% filter(generation == 'Silent'), values = ~mean, labels = ~generation, textinfo='label+percent', name="Silent") #y=~mean,
      trendine <- trendine %>% add_trace(data=filtered_trend_generation() %>% filter(generation == 'G.I. Generation'), values = ~mean, labels = ~generation, textinfo='label+percent', name="G.I. Generation") #y=~mean,
      trendine <- trendine %>% add_trace(data=filtered_trend_generation() %>% filter(generation == 'Boomers'), values = ~mean, labels = ~generation, textinfo='label+percent', name="Boomers") #y=~mean,
      trendine <- trendine %>% add_trace(data=filtered_trend_generation() %>% filter(generation == 'Millenials'), values = ~mean, labels = ~generation, textinfo='label+percent', name="Millenials") #y=~mean,
      trendine <- trendine %>% add_trace(data=filtered_trend_generation() %>% filter(generation == 'Generation Z'), values = ~mean, labels = ~generation, textinfo='label+percent', name="Generation Z") #y=~mean,
      trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad'))

      hide_colorbar(trendine)
    })

    
    #------------------------ Statistika sugu kaupa-------------------------# 

    filtered_gender_all <- reactive({
        data %>% 
        group_by(sex) %>%
        summarize(sum = as.integer(sum(suicides.100k.pop))) # suicides_no
    })
    
    output$gender_all <-  renderPlotly({
      
        gender_pie <- filtered_gender_all() %>% 
            plot_ly(type='pie', labels = ~sex, values = ~sum, textinfo='label+percent')
        gender_pie <- gender_pie %>% 
            layout(title = 'Enesetappu 100 000 elaniku kohta',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })  
    
    
      #---------- riikide ja aastate kaupa -----------#
    
    filtered_gender_by <- reactive({
        data %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            filter(country == input$country) %>%
            group_by(sex) %>%
            summarize(sum = round(sum(.data[[input$tunnus_all]])))
    })
    
    output$gender <-  renderPlotly({
        fig2 <- filtered_gender_by() %>% 
            plot_ly(type='pie', labels = ~sex, values = ~sum, textinfo='label+percent')
        
        fig2 <- fig2 %>% 
            layout(title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'},
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
