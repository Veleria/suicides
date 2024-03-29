# install.packages("shiny")
# install.packages("plotly")
library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(dplyr)
library(summarytools)
library(countrycode)
library(corrplot)

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
data$age <- gsub(" years","", data$age)
data$gdp_year <- as.numeric(gsub(",","", data$gdp_year))/1000000
names(data)[8] <- "gdp_year_mln"
# data %>% mutate('gdp_year' = as.numeric(gsub(",","", 'gdp_year')))

data$age <- factor(data$age, levels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+"))
data$sex <- factor(data$sex, levels = unique(data$sex))
data$generation <- factor(data$generation, levels = unique(data$generation))
# data$CODE = countrycode(data$country, 'country.name', "iso3c") #lisan riigi koodi map joonistamiseks


# #-------------- menshe dannyh na server------------------
# data = data[data$year > 2000,]
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

set.seed(3)
x <- sample(nrow(data),50)
d=data[x,]

filtered_map <- data %>%  
    group_by(country) %>%
    summarize(mean = round(mean(suicides.100k.pop),2)) %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c"))


#-------------------------------------------------------------------------------- LIIDES ---------------------------------------------------------------------#


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("www/styles.css"),

    navbarPage(
        "Enesetappude määrad", 
        id = "main_navbar",
        
        #---------- Peamine leht ------------#
        
        tabPanel(
            "Kirjeldus",
            titlePanel(paste("Enesetappude määrade ülevaade ", min_year," kuni ", max_year, ". Projekti kirjeldus.", sep ="")),hr(),
            sidebarPanel(
                p(img(src = 'pink.png',height="50%", width="80%")),
                helpText(HTML("Andmete visualiseerimise projekt<br>Valeria Juštšenko")),
                p("Selle projekti eesmärgid on analüüsida iga riigi suitsiidide määrad ja nende seost enesetappude määra vahel soo, 
                  vanuse ja põlvkonnaga aastatel 1985–2016. "), 
                br(),
                h2("Rakenduse kirjeldus"),
                a(href = "https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016", "Link andmestikule", target = "_blank"),br(),
                p("See andmestik pärineb neljast teisest aja ja kohaga
                seotud andmekogumist ning loodi selleks,
                  et leida signaale, mis on korrelatsioonis suurenenud enesetappude
                  määraga erinevate rühmade vahel kogu sotsiaal-majandusliku spektri
                  ulatuses."), 
                p(textOutput("rows")), 
                p(HTML("Vaheleht <b>'Kirjeldus'</b> sisaldab andmetabeli, selle struktuuri ja lühikokkuvõte kirjeldus, ")),
                p(HTML("Vaheleht <b>'Kaart'</b> sisaldab kaart andmete visualiseerimiseks")),
                p(HTML("Vaheleht <b>'Maailma statistika'</b> sisaldab maailma statistika visualiseerimine")),
                p(HTML("Vaheleht <b>'Statistika riikide kaupa'</b> sisaldab statistika visualiseerimine riikide kaupa,
                  kus on võimalik valida ka aastate vahemik visualiseerimiseks")),
                h4("Tunnused"),
                p(HTML("country - riik <br> year - aasta<br> sex - sugu<br> age - vanuserühm<br>
                suicides_no - enesetappude arv<br>
                population - elanikkond (vanuserühma keskmise põhjal) <br>
                suicides.100k.pop - tähistab enesetapu põhjustatud surmajuhtumite arvu kokku 100 000 surma korral <br>
                gdp_for_year - Sisemajanduse kogutoodang (SKT) aastas <br>
                gdp_per_capita - SKT elaniku kohta on mõõdik, mis jagab riigi SKT inimese kohta ja arvutatakse riigi SKT jagamisel selle rahvaarvuga. <br>
                generation - põlvkond (vanuserühma keskmise põhjal).")),
                p("Olid kustutatud HDI for year (aasta inimarengu indeks) ja country.year (country-year liitvõti) tunnused.")),
            
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Struktuur", h3("Struktuur"), verbatimTextOutput("str")),
                    tabPanel("Kokkuvõte", h3("Summary"), verbatimTextOutput("summ")),
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
            titlePanel("Maailma enesetappude määrade ülevaade. Kaart."), hr(),
            sidebarPanel(
                h4(paste("Kokku enesetappude arv alates ", min_year," to ", max_year, ' (keskmine 100 000 elaniku kohta või kokku inimesi)', sep ="")), 
                br(),
                p('Source:',  a(href = "https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016", "Link andmetele", target = "_blank")),
                p("Sellel vahelehel kuvatakse kuidas paigaldatakse enesetappude arv kokku voi keskmine 100 000 elaniku kohta (vaikimisi)"),
                p("Samuti on võimalik vaadata statistika määratud vahemikel."),
                p("Kaartide joonistamiseks oli kasutatud plotly teek. Ka oli proovitud highcharter teek, aga see tegi rakendus aeglasemaks ja sellepärast oli lõppus kustutatud"),
                selectInput(
                    'tunnus',
                    'Select Tunnus',
                    choices =  c("Enesetapud 100 000 kohta" = "suicides.100k.pop", "Enesetapud kokku" = "suicides_no"),
                    selected = "suicides.100k.pop"
                ),
                sliderInput("slider_years_map", label = h3("Aastate piirkond"), 
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

        #---------- Statistika by World ----------#
        
        tabPanel("Maailma statistika",
                 titlePanel(paste("Maailma enesetappude määrade ülevaade ", min_year," to ", max_year, sep ="")),hr(),
                 sidebarPanel(
                     h4("Enesetappude määrade ülevaade (100 000 elaniku kohta)"), hr(),
                     p("Sellel vahelehel kuvatakse kuidas paigaldatakse enesetappude arv keskmiselt 100 000 elaniku kohta"),
                     
                     # h3("Sugu riikide ja aastate kaupa"),
                     HTML("<b>Trendijoon</b> - maailma enesetappude trende aastate jooksul.</br>"),
                     HTML("On võimalus lisada riikide trendjooned võrdlemiseks"),
                     checkboxInput("OneMore", label = h5("Kas valida riigid võrdlemiseks?"), F),
                     conditionalPanel(
                       condition = "input.OneMore == 1",
                       selectizeInput('riigid','Riigid',choices = c("All", uniq_country), multiple = TRUE, selected = "Estonia", options = list(maxItems = 5)
                       ),
                     ),
                     HTML("<b>Sugu</b> - Kui suur on meeste ja naiste enesetappude protsent maailmas? </br> 
                          Mis aastal on enesetappude arv suurem meeste ja naiste seas?</br>"),
                     HTML("<b>Vanus</b> - Mis aastal on enesetappude arv suurem vanuse kaupa? </br>
                          Milline on enesetappude jaotus erinevate vanuserühmade vahel? </br>"),
                     HTML("<b>Põlvkond</b> - Mis aastal on enesetappude arv suurem põlvkondade kaupa? </br>
                          Millised on enesetappude määrad erinevate põlvkondade vahel?</br>"),
                 ),
                 mainPanel(
                   h3(paste("Globaalsed enesetapud (100 000 kohta) ",min_year," kuni ",max_year,' aastate jooksul',sep = "")),
                   tabsetPanel(
                     type = "tabs",
                   tabPanel("Trendijoon",
                            fluidRow(  
                              # column(width=8,
                                     # h3("Trendijoon maailmas"), plotlyOutput("years_trendine")),
                              column(width=8,
                                     # plotlyOutput('proov'),
                                         conditionalPanel(
                                           condition = "input.OneMore == 1",
                                           h3("Andmed"),
                                           plotlyOutput('proov')
                                         ),conditionalPanel(
                                           condition = "input.OneMore == 0",
                                           h3("Andmed"),
                                           plotlyOutput("years_trendine")
                                         )
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
                                     h3("Trendijoon vanuse järgi"), plotlyOutput("trend_age")  ),
                              column(width=4,
                                     h3("Vanus protsentides"), plotlyOutput("age_pie_world") 
                                     )
                            )),
                   tabPanel("Põlvkond",
                            fluidRow(
                              column(width=8,
                                     h3("Trendijoon põlvkonna järgi"), plotlyOutput("trend_generation") ),
                              column(width=4,
                                     h3("Põlvkond"), plotlyOutput("generation_all") )
                            ))))),
        
        
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
                h4(textOutput("year_min")),
                # h4(uiOutput("year_min_slider")),
                sliderInput("slider_years_gpaph", label = h3("Aastate piirkond"),
                            min = min_year,
                            max = max_year,
                            step = 1,
                            value = c(min_year, max_year)),
                
            ),
            mainPanel(
                       # h3(paste("Enesetapud ", min_year," kuni ", max_year, ' aastate jooksul', sep ="")),
                       h3(textOutput("year3")),

              fluidRow(
                column(width=8,
                       # h3(paste("Generation tunnuse ", selectInput$generation_by_year," ja ", selectInput$generation_by_year, ' aastate kaupa', sep ="")),
                       h3("Trendijoon aasta järgi"), plotlyOutput("yearsLine"),
                       ),
                #---------- Statistika sugu kaupa-----------#       
                column(width=4, h3("Vanuse, riikide ja aastate kaupa."),plotlyOutput('age_sex_bar'))  
                ),
              
              fluidRow(
                  #---------- Statistika vanuse kaupa-----------#
                  column(width=4,h3("Vanuse järgi"), plotlyOutput("age_pie")), 
                  #---------- Statistika sugu kaupa-----------#       
                  column(width=4, h3("Sugu, riikide ja aastate kaupa"), plotlyOutput("gender")),  
                   #-------- Statistika  generation kaupa---------#
                  column(width=4,  h3("Polvkonna ja aastate kaupa"), plotlyOutput("generation"))  ),
              ),
    )))


#-------------------------------------------------------------------- SERVERI OSA --------------------------------------------------------------------------#


server <- function(input, output, session) {
  
   #------------------------       Proov        --------------------------#
          
          filtered_riik <- reactive({
            validate(
              need(input$riigid != "", "Palun valige riik")
            )
            data %>%  
              group_by(country, year) %>% 
              # filter(country == input$riigid) %>%
              summarize(mean = round(mean(suicides.100k.pop), 2)) #sum(suicides_no) / sum(population)) * 100000, 2))
          })
          
          # output$proov <- renderPlotly({
          #   # trendine <-
          #   #   plot_ly(
          #   #     filtered_riik(),
          #   #     x = ~ year,
          #   #     y = ~ mean,
          #   #     name = "keskmine", 
          #   #     color = ~mean,
          #   #     type = 'scatter',
          #   #     mode = 'lines+markers'
          #   #   )
          #   # trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) %>% hide_legend()
          #   # # trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
          #   #                                    # hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine") # %>%
          #   # # hide_colorbar(trendine)
          #   
          #   trendine <-
          #     plot_ly(filtered_riik(), x = ~ year, y = ~ mean, name = "keskmine", color = ~mean, mode = 'lines+markers') %>%
          #     filter(country %in% input$riigid) %>%
          #     group_by(country) %>%
          #     add_lines()
          #   trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) %>% hide_legend()
          # })
          
          output$proov <- renderPlotly({
            trendine <-
              plot_ly(
                filtered_riik(),
                x = ~ year,
                y = ~ mean,
                name = " ",
                color = ~ country,
                # type = 'scatter',
                mode = 'markers',
                marker = list(size = 6),
                hovertemplate = paste(
                  "%{x:.0f} aastal <br>%{y:.2f} enesetappu <br>100t/elaniku kohta"
                )
              ) %>%
              filter(country %in% input$riigid) %>%
              group_by(country) %>%
              add_lines()
            # trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines" ,marker = list(size = 0), name = " ", line = list(width = 1, dash = 'dash'))
            trendine <-
              trendine %>% layout(
                yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'),
                xaxis = list(title = 'Aastad'),
                hovertemplate = paste("Keskmine %{y:.2f} kokku")
              ) %>% colorbar(title = "Keskmine") %>% hide_legend()
            
            # hide_colorbar(trendine)
          })
          

                #------------------------ Tekst --------------------------#
  
  output$year1 <- renderText({ 
    paste("Enesetappude määrade ülevaade aastate kaupa: ", input$slider_years_map[1], " kuni ", input$slider_years_map[2], sep ="")
  })
  
  output$year2 <- renderText({
    paste("Enesetappude määrade ülevaade aastate kaupa: ", input$slider_years_map[1], " kuni ", input$slider_years_map[2]," riikide lõigus", sep ="")
  })
  
  output$year3 <- renderText({
    paste("Enesetapud ", input$slider_years_gpaph[1], " kuni ", input$slider_years_gpaph[2]," aastate jooksul ",input$country," riigis", sep ="")
  })
  
  output$rows <- renderText({
    paste0("Kirjet kokku: ", nrow(data), sep ="")
  })
  
  output$year_min <- renderText({
    min <- data %>%  
      filter(country == input$country) %>%
      summarize(min = min(year))
    # min <- min[0]
    # min <- as.integer(min)
    paste0(input$country, " riigis minimaalne aasta on ", min)
  })
  
  output$year_min_slider <- renderText({
    min <- data %>%  
      filter(country == input$country) %>%
      summarize(min = min(year))
    min <- as.integer(min)
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

    filtered_map <- reactive({
        dt <- data %>%
            filter(year >= input$slider_years_map[1] & year <= input$slider_years_map[2]) %>%
            group_by(country) 
        if(input$tunnus=='suicides.100k.pop')
          {dt <- dt %>% summarize(mean = round(mean(.data[[input$tunnus]]),2))}
        else
        {dt <- dt %>% summarize(mean = round(sum(.data[[input$tunnus]]),2))}
        dt <- dt %>% mutate(iso3 = countrycode(country, "country.name", "iso3c"))
    })

    output$map <-  renderPlotly({
      title = if(input$tunnus=='suicides.100k.pop'){'Keskmine'} else {'Kokku'}
      l =  list(color = toRGB("grey"), width = 0.7)
        fig <- plot_geo(filtered_map())
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
        barchart <- filtered_map() %>%
          arrange(mean)  %>%
          # head(10) %>%
          plot_ly(x = ~country, y = ~mean, color = ~mean, type = "bar", colors = "viridis",showlegend = FALSE)
        barchart <- barchart %>% colorbar(title = title2)  %>% layout(yaxis = list(title = title), xaxis = list(title = 'Riigid'), barmode = 'stack')
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
                name = " ", 
                color = ~mean,
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = paste("%{x:.0f} aastal <br>%{y:.2f} enesetappu <br>100t/elaniku kohta" )
            )
        trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) %>% hide_legend()
        trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                           hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")    
        # hide_colorbar(trendine)
    })
    
                #----------- Statistika aastate ja riigi kaupa ---------------#
    filtered_years_by_country <- reactive({
      
        dt <- data %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            filter(country == input$country) %>%
            group_by(year) #%>%
            # summarize(mean = round(mean(.data[[input$tunnus_all]])))  # summarize(mean = round((sum(suicides_no) / sum(population)) * 100000, 2))
            if(input$tunnus_all=='suicides.100k.pop')
            {dt <- dt %>% summarize(mean = round(mean(.data[[input$tunnus_all]]),2))}
            else
            {dt <- dt %>% summarize(mean = round(sum(.data[[input$tunnus_all]])))}
    })
    
    output$yearsLine <- renderPlotly({
      title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku, inim'}
      years_trend <-
            plot_ly(
                filtered_years_by_country(),
                x = ~ year,
                y = ~ mean,
                # color = ~mean,
                type = 'scatter',
                mode = 'lines+markers',
              hovertemplate = paste(
                "%{x:.0f} aastas <br>%{y:.2f} %{yaxis.title.text}"
              )
            )
        years_trend <- years_trend %>% layout(yaxis = list(title = title), xaxis = list(title = 'Aastad'))  %>% hide_legend()
        if(input$tunnus_all=='suicides.100k.pop')
        {
        years_trend <- years_trend %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                           hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")  }
        hide_colorbar(years_trend)
        
    })
    
    #------------------- Satatistika vanuse ja sugu kaupa --------------------#
    filtered_age_sex <- reactive({
      dt <- data %>%
        # select(country, year, age, sex, suicides.100k.pop) %>%
        filter(country == input$country) %>%
        filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
        group_by(age, sex) #%>%
      if(input$tunnus_all=='suicides.100k.pop')
      {dt %>%summarize(sum = round(mean(.data[[input$tunnus_all]]),2))}
      else
      {dt %>% summarize(sum = round(sum(.data[[input$tunnus_all]])))}
    })
    
    output$age_sex_bar <- renderPlotly({
      title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'}
      bar_age <- plot_ly(
        filtered_age_sex()%>% filter(sex == 'male'),
        x = ~ age,
        y = ~ sum,
        type = 'bar', 
        name = 'mees'
      )
      bar_age <- bar_age %>% add_trace(data=filtered_age_sex()%>% filter(sex == 'female'), values = ~sum, labels = ~age, textinfo='label+percent', name="naine")
      bar_age <- bar_age %>% layout(yaxis = list(title = title), xaxis = list(title = 'Vanus'), barmode = 'stack')
    })
    
    #--------------------- Satatistika sugu kaupa / Sex ----------------------#
    
    filtered_sex <- reactive({
      data %>%
        select(year, sex, suicides.100k.pop) %>%
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
          type = 'scatter',
          mode = 'lines+markers', name="Mees",
          hovertemplate = paste("%{x:.0f} aastal <br>%{y:.2f} enesetappu <br>100t/elaniku kohta" )
        )
      trendine <- trendine %>% add_trace(data=woman, values = ~mean, labels = ~sex, textinfo='label+percent', name="Naine") 
      trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) 
      trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = "keskmine", 
                                         line = list(width = 1, dash = 'dash'),
                                         hovertemplate = paste("%{y:.2f} inim." )) %>% colorbar(title = "Keskmine")
    })
    
    
    #-------------------- Satatistika vanuse kaupa / Age ---------------------#
                #--------- Vanus kokku ----------#
    
    filtered_age_world <- reactive({
        data %>%
            group_by(age) %>%
            summarise(mean = round(mean(suicides.100k.pop), 2)) 
    })

    output$age_pie_world <- renderPlotly({
        pie_age <- filtered_age_world() %>% 
            plot_ly(type='pie', labels = ~age, values = ~mean, textinfo='label+percent')
    })

    filtered_trend_age <- reactive({
      data %>%
        select(year, age, suicides.100k.pop) %>%
        group_by(age, year) %>%
        summarise(mean = round(mean(suicides.100k.pop),2)) # mean(suicides.100k.pop))
    })
    

    output$trend_age <- renderPlotly({
      trendine <-
        plot_ly(
          filtered_trend_age() %>% filter(age == '5-14'),
          x = ~ year,
          y = ~ mean,
          # values = ~ sex,
          # color = ~mean,
          type = 'scatter',
          mode = 'lines+markers', name="5-14 aastat",
          hovertemplate = paste(
            "%{x:.0f} aastas <br>%{y:.2f} enesetappu <br>100t/elaniku kohta"
          )
        )
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '15-24'), values = ~mean, labels = ~age, textinfo='label+percent', name="15-24 aastat") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '25-34'), values = ~mean, labels = ~age, textinfo='label+percent', name="25-34 aastat") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '35-54'), values = ~mean, labels = ~age, textinfo='label+percent', name="35-54 aastat") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '55-74'), values = ~mean, labels = ~age, textinfo='label+percent', name="55-74 aastat") #y=~mean, 
      trendine <- trendine %>% add_trace(data=filtered_trend_age() %>% filter(age == '75+'), values = ~mean, labels = ~age, textinfo='label+percent', name="75+ aastat") #y=~mean, 
      
      trendine <- trendine %>% layout(yaxis = list(title = 'Enesetappu 100 000 elaniku kohta'), xaxis = list(title = 'Aastad')) #%>% hide_legend()
      hide_colorbar(trendine)
      trendine <- trendine %>% add_trace(y = mean_all, type = "scatter", mode = "lines", name = " ", line = list(width = 1, dash = 'dash'),
                                         hovertemplate = paste("Keskmine %{y:.2f} kokku" )) %>% colorbar(title = "Keskmine")
    })
    

                #---------Vanus aastate ja riigi kaupa ----------#
    filtered_age_by <- reactive({
      dt <- data %>%
        select(country, year, age, suicides.100k.pop, suicides_no) %>%
            filter(country == input$country) %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            group_by(age) 
      if(input$tunnus_all=='suicides.100k.pop')
      {
        dt <- dt %>% summarize(mean = round(mean(.data[[input$tunnus_all]]),2))
        }
      else
      {
        dt <- dt %>% summarize(mean = round(sum(.data[[input$tunnus_all]])))
        }
    })

    
    output$age_pie <- renderPlotly({
      title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'}
      pie_age <- filtered_age_by() %>%
      plot_ly(type='pie', labels = ~age, values = ~mean, textinfo='label+percent', domain= list(row = 0, column=0),
              hovertemplate = paste(
                "%{label} aastat <br>%{value} inimest"
              ), name=" ")
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
        # bar_age <- bar_age %>% add_trace(data=filtered_age_by()%>% filter(age == '75+'), values = ~sum, labels = ~age, textinfo='label+percent', name="Estonia")
        bar_age <- bar_age %>% layout(yaxis = list(title = title), xaxis = list(title = 'Vanus'), barmode = 'stack')
    })

    
    #------------------------------- Polvkond / generation kokku --------------------------------#
    
    filtered_generation_all <- reactive({
        data %>%
            group_by(generation) %>%
            summarize(sum = round(mean(suicides.100k.pop),2))
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
      dt <- data %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            filter(country == input$country) %>%
            group_by(generation) 
      if(input$tunnus_all=='suicides.100k.pop')
      {dt <- dt %>% summarize(sum = round(mean(.data[[input$tunnus_all]]),2))}
      else
      {dt <- dt %>% summarize(sum = round(sum(.data[[input$tunnus_all]])))}
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
      generation_bar <- generation_bar %>% layout(yaxis = list(title = title), xaxis = list(title = 'Põlvkond'), barmode = 'stack')
        
    })
    
    filtered_trend_generation <- reactive({
      data %>%
        select(year, generation, suicides.100k.pop) %>%
        group_by(generation, year) %>%
        summarise(mean = round(mean(suicides.100k.pop),2)) 
    })
    
    
    output$trend_generation <- renderPlotly({
      trendine <-
        plot_ly(
          filtered_trend_generation() %>% filter(generation == 'Silent'),
          x = ~ year,
          y = ~ mean,
          type = 'scatter',
          mode = 'lines+markers', name="Generation X",
          hovertemplate = paste("%{x:.0f} aastal <br>%{y:.2f} enesetappu <br>100t/elaniku kohta" )
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

    
    #------------------------ Statistika sugu kaupa kokku-------------------------# 

    filtered_gender_all <- reactive({
        data %>% 
        group_by(sex) %>%
        summarize(sum = round(mean(suicides.100k.pop),2)) # suicides_no
    })
    
    output$gender_all <-  renderPlotly({
        gender_pie <- filtered_gender_all() %>% 
            plot_ly(type='pie', labels = ~sex, values = ~sum, textinfo='label+percent')
        gender_pie <- gender_pie %>% 
            layout(title = 'Enesetappu 100 000 elaniku kohta',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })  
    
    
      #---------- Sugu riikide ja aastate kaupa -----------#
    
    filtered_gender_by_country <- reactive({
      dt <- data %>%
            filter(year >= input$slider_years_gpaph[1] & year <= input$slider_years_gpaph[2]) %>%
            filter(country == input$country) %>%
            group_by(sex) 
      if(input$tunnus_all=='suicides.100k.pop')
      {
      dt <- dt %>% summarize(sum = round(mean(.data[[input$tunnus_all]]),2))
      }
      else
      {
        dt <- dt %>% summarize(sum = round(sum(.data[[input$tunnus_all]])))
        }
    })

    output$gender <-  renderPlotly({
        fig2 <- filtered_gender_by_country() %>% 
            plot_ly(type='pie', labels = ~sex, values = ~sum, textinfo='label+percent')
        
        fig2 <- fig2 %>% 
            layout(title = if(input$tunnus_all=='suicides.100k.pop'){'Enesetappu 100 000 elaniku kohta'} else {'Enesetapud kokku'},
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
