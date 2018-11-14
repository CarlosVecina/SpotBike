library(shinydashboard)
library(shiny)
library(ggplot2)
library(MASS)
library(plotly)
library(shinythemes)
library(shinyLP)
library(RJSONIO)
library(crayon)
library(markdown)
library(quantmod)
library(leaflet)
library(dplyr)
library(lubridate)
library(flexdashboard)
# Funcion slider de meses
monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}

ui <-
    
    dashboardPage(
        skin = "green",
        dashboardHeader(
            title = "SpotBike",
            # Menu de mensajes.
            dropdownMenu(
                type = "messages",
                badgeStatus = "primary",
                messageItem("Carlos Vecina",
                            "Welcome to my shiny app!",
                            time = "Now"),
                messageItem("Carlos Vecina",
                            "Any questions, contact me:",
                            time = "Now"),
                messageItem("Carlos Vecina",
                            "carlosvecinatebar@gmail.com",
                            time = "Now")
            )
        ),
        dashboardSidebar(bootstrapPage(
            sidebarMenu(
                # Sidebar con marquesina y paneles
                tags$div(h6(HTML(
                    "| Carlos Vecina Tebar "
                )),
                htmlOutput("marquesina")),
                
                menuItem("Introduccion", tabName = "i0"),
                menuItem(
                    "1. Explorar el mapa",
                    tabName = "i1",
                    icon = icon("play")
                ),
                menuItem(
                    "2. Analisis en profundidad",
                    tabName = "i2",
                    icon = icon("play")
                ),
                menuItem(
                    "3. Evolucion temporal",
                    tabName = "i3",
                    icon = icon("play"),
                    badgeLabel = "pronto",
                    badgeColor = "green"
                ),
                menuItem(
                    "4. Calcula tu ruta",
                    tabName = "i4",
                    icon = icon("eur"),
                    badgeLabel = "pronto",
                    badgeColor = "green"
                )
            )
            
        )),
        dashboardBody(fluidPage(
            HTML("<style>p.indent{ padding-left: 1.8em }</style>"),
            tabItems(
                tabItem(
                    # 0. Introduccion
                    tabName = "i0",
                    column(4),
                    column(8,
                           HTML("<h1><b>SpotBike</b></h1>")),
                    column(2),
                    column(10,
                           h3("Descubre las calles y cruces mas peligrosos de Madrid para ti como ciclista")),
                    column(12,
                           h4("")),
                    column(4, 
                           HTML("<h3><center><b>Mapa de Madrid</h3></center></b>"),
                           tags$img(src="1.Mapa_Madrid.PNG",width="110%", height="110%")),
                    column(4,
                           HTML("<h3><center><b>Evolucion temporal</h3></center></b>"),
                           tags$img(src="2.Analisis_Profundidad.PNG",width="110%", height="110%")),
                    column(4,
                           HTML("<h3><center><b>Ruta alternativa</h3></center></b>"),
                           tags$img(src="4.Ruta_Alternativa.PNG",width="110%", height="110%"))
                ),
                tabItem(
                    # 1. Mapa
                    tabName = "i1",
                    column(12,
                           HTML("<h1><center>Accidentes de bicicleta en Madrid</h1></center>"),
                           tags$head(tags$style(
                               HTML(
                                   "
                                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                    
                                    h1 {
                                    font-family: 'Lobster', cursive !important;
                                    font-weight: 500;
                                    line-height: 1.5;
                                    color: #00a65a;
                                    }
                                    
                                    "
                               )
                           ))
                    ),
                    column(
                        3,
                        sliderInput(
                            "slider1",
                            "Periodo",
                            min = as.Date("2018-01-01"),
                            max = as.Date("2018-10-01"),
                            value = c(as.Date("2018-01-01"), as.Date("2018-05-01")),
                            timeFormat = "%b %Y",
                            animate = animationOptions(interval = 300)
                        )
                    ),
                    column(2,
                           selectInput("centrarMapa",label = "Centrar el mapa", choices = 
                                           c("Madrid" = "Madrid",
                                             "Chamberi" = "Chamberi",
                                             "Latina" = "Latina",
                                             "Plaza Castilla" = "Plaza Castilla"))),
                    column(1),
                    column(
                        6
                        
                    ),
                    column(12,
                           h5("")),
                    
                    
                    column(12,
                           leafletOutput("mymap", height = 500)),
                    column(
                        12,
                        conditionalPanel(condition = "input.cryptocurrency == 'IOT' || input.cryptocurrency == 'XLM'",
                                         h6(
                                             HTML("XLM (in Eur) and IOT are not available in Kraken platform.")
                                         )),
                        plotOutput('grafica1', click = "clickGrafica")
                        
                    )
                    
                ),
                tabItem(
                    # 2. Analisis
                    tabName = "i2",
                    column(12,
                           
                           sliderInput(
                               "slider2",
                               "Periodo",
                               min = as.Date("2018-01-01"),
                               max = as.Date("2018-11-01"),
                               value = c(as.Date("2018-01-01"), as.Date("2018-05-01")),
                               timeFormat = "%b %Y",
                               animate = animationOptions(interval = 300)
                           )                        
                    ),
                    tags$div(
                        class = "col-md-4",
                        tags$div(
                            class = "box box-widget widget-user",
                            ## Header ##
                            tags$div(
                                class = "widget-user-header bg-green-active ",
                                tags$h3(class = "widget-user-username", "ACCIDENTES SEGUN GRAVEDAD")
                            ),
                            ## Image ##
                            tags$div(
                                class = "widget-user-image",
                                tags$img(class = "img-circle", src = "https://www.metoffice.gov.uk/binaries/content/gallery/mohippo/images/barometer/features/warning-icons.jpg/warning-icons.jpg/mohippo%3Abarometerheroimage", alt = "User Avatar")
                            ),
                            ## Footer ##
                            tags$div(class = "box-footer",
                                     tags$div(
                                         class = "row",
                                         # Primera columna
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("Ilesos")),
                                                 tags$span(class = "description-text", textOutput("PorcIlesos")),
                                                 tags$span(class = "description-text", "ILESOS")
                                             )
                                         ),
                                         # Segunda columna
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("Leves")),
                                                 tags$span(class = "description-text", textOutput("PorcLeves")),
                                                 tags$span(class = "description-text", "HERIDO LEVE")
                                             )
                                         ),
                                         # Tercera columna
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("Graves")),
                                                 tags$span(class = "description-text", textOutput("PorcGraves")),
                                                 tags$span(class = "description-text", "HERIDO GRAVE")
                                             )
                                         )
                                     ))
                        )
                    ),
                    tags$div(
                        class = "col-md-4",
                        tags$div(
                            class = "box box-widget widget-user",
                            tags$div(
                                class = "widget-user-header bg-aqua-active",
                                tags$h3(class = "widget-user-username", "ACCIDENTES SEGUN SEXO")
                            ),
                            tags$div(
                                class = "widget-user-image",
                                tags$img(class = "img-circle", src = "https://www.actuall.com/wp-content/uploads/2016/09/hombre-mujer-696x499.jpg", alt = "User Avatar")
                            ),
                            tags$div(class = "box-footer",
                                     tags$div(
                                         class = "row",
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("Mujer")),
                                                 tags$span(class = "description-text", textOutput("PorcMujer")),
                                                 
                                                 tags$span(class = "description-text", "MUJERES")
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", ""),
                                                 tags$span(class = "description-text", "")
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("Hombre")),
                                                 tags$span(class = "description-text", textOutput("PorcHombre")),
                                                 
                                                 tags$span(class = "description-text", "HOMBRES")
                                             )
                                         )
                                         
                                     ))
                        )
                    ),
                    tags$div(
                        class = "col-md-4",
                        tags$div(
                            class = "box box-widget widget-user",
                            tags$div(
                                class = "widget-user-header bg-green-active",
                                tags$h3(class = "widget-user-username", "ACCIDENTES SEGUN EDAD")
                            ),
                            tags$div(
                                class = "widget-user-image",
                                tags$img(class = "img-circle", src = "http://static.t13.cl/images/sizes/1200x675/1534362910-harold-0.jpg", alt = "User Avatar")
                            ),
                            tags$div(class = "box-footer",
                                     tags$div(
                                         class = "row",
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("acc15")),
                                                 tags$span(class = "description-text", textOutput("PorcAcc15")),
                                                 
                                                 tags$span(class = "description-text", "15 - 29  ANOS")
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("acc30")),
                                                 tags$span(class = "description-text", textOutput("PorcAcc30")),
                                                 
                                                 tags$span(class = "description-text", "30 - 49  ANOS")
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("acc50")),
                                                 tags$span(class = "description-text", textOutput("PorcAcc50")),
                                                 
                                                 tags$span(class = "description-text", "50 <  ANOS")
                                             )
                                         )
                                         
                                     ))
                        )
                    ),
                    tags$div(
                        class = "col-md-4",
                        tags$div(
                            class = "box box-widget widget-user",
                            tags$div(
                                class = "widget-user-header bg-green-active",
                                tags$h3(class = "widget-user-username", "ACCIDENTES MOMENTOS DIA")
                            ),
                            tags$div(
                                class = "widget-user-image",
                                tags$img(class = "img-circle", src = "https://image.freepik.com/vector-gratis/cuatro-escenas-naturaleza-diferentes-momentos-dia_1308-4429.jpg", alt = "User Avatar")
                            ),
                            tags$div(class = "box-footer",
                                     tags$div(
                                         class = "row",
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("momento1")),
                                                 tags$span(class = "description-text", textOutput("PorcMomento1")),
                                                 
                                                 tags$span(class = "description-text", "Manana[06,14]")
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("momento2")),
                                                 tags$span(class = "description-text", textOutput("PorcMomento2")),
                                                 
                                                 tags$span(class = "description-text", "Tarde[15,22]")
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("momento3")),
                                                 tags$span(class = "description-text", textOutput("PorcMomento3")),
                                                 
                                                 tags$span(class = "description-text", "Noche[23,05]")
                                             )
                                         )
                                         
                                     ))
                        )
                    ),
                    tags$div(
                        class = "col-md-4",
                        tags$div(
                            class = "box box-widget widget-user",
                            tags$div(
                                class = "widget-user-header bg-aqua-active",
                                tags$h3(class = "widget-user-username", "ACCIDENTES DISTRITOS PELIGROSOS")
                            ),
                            tags$div(
                                class = "widget-user-image",
                                tags$img(class = "img-circle", src = "https://s.libertaddigital.com/2016/02/22/distritos-madrid.jpg", alt = "User Avatar")
                            ),
                            tags$div(class = "box-footer",
                                     tags$div(
                                         class = "row",
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("dist1")),
                                                 tags$span(class = "description-text", textOutput("PorcDist1")),
                                                 
                                                 tags$span(class = "description-text", textOutput("distNombre1"))
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("dist2")),
                                                 tags$span(class = "description-text", textOutput("PorcDist2")),
                                                 
                                                 tags$span(class = "description-text", textOutput("distNombre2"))
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("dist3")),
                                                 tags$span(class = "description-text", textOutput("PorcDist3")),
                                                 
                                                 tags$span(class = "description-text", textOutput("distNombre3"))
                                             )
                                         )
                                     ))
                            
                        )
                    ),
                    tags$div(
                        class = "col-md-4",
                        tags$div(
                            class = "box box-widget widget-user",
                            tags$div(
                                class = "widget-user-header bg-green-active",
                                tags$h3(class = "widget-user-username", "ACCIDENTES MAS FRECUENTES")
                            ),
                            tags$div(
                                class = "widget-user-image",
                                tags$img(class = "img-circle", src = "data:image/png;base64,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", alt = "User Avatar")
                            ),
                            tags$div(class = "box-footer",
                                     tags$div(
                                         class = "row",
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("tipoAccidente1")),
                                                 tags$span(class = "description-text", textOutput("PorcTipoAccidente1")),
                                                 
                                                 tags$span(class = "description-text", textOutput("tipoAccidenteNombre1"))
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("tipoAccidente2")),
                                                 tags$span(class = "description-text", textOutput("PorcTipoAccidente2")),
                                                 
                                                 tags$span(class = "description-text", textOutput("tipoAccidenteNombre2"))
                                             )
                                         ),
                                         tags$div(
                                             class = "col-sm-4 border-right",
                                             tags$div(
                                                 class = "description-block",
                                                 tags$h5(class = "description-header", textOutput("tipoAccidente3")),
                                                 tags$span(class = "description-text", textOutput("PorcTipoAccidente3")),
                                                 
                                                 tags$span(class = "description-text", textOutput("tipoAccidenteNombre3"))
                                             )
                                         )
                                     ))
                            
                        )
                    ),
                    h1(" "),
                    HTML("<h1><center>Accidentes segun dias de la semana</h2></center>"),
                    column(12,
                           plotOutput("plotPorDias")),
                    HTML("<h1><center>Accidentes segun meses</h2></center>"),
                    column(12,
                           plotOutput("plotPorMeses"))
                    
                    
                )
                
                
            )
            
            
            
        )
        ))




server <- function(input, output, session) {
    sliderMonth <- reactiveValues()
    observe({
        full.date <- as.POSIXct(input$slider1, tz = "GMT")
        
        sliderMonth$Month <-
            as.character(monthStart(full.date))
    })
    
    bikeData <- readRDS(file = "data/bikes.RDS") %>% 
        filter(LESIVIDAD != "NO ASIGNADA") %>% 
        filter(Tramo.Edad != "DESCONOCIDA")
    levels(bikeData$LESIVIDAD) <-
        c("Heridos Graves", "Heridos Leves", "Ilesos", "NA")
    
    # Panel 1    
    observeEvent(input$slider1, {
        bikeData1 <-
            bikeData %>% filter(between(
                as.Date(dmy_hm(FECHA)),
                as.Date(input$slider1[1]),
                as.Date(input$slider1[2])
            ))
        
        output$mymap <- renderLeaflet({
            #https://nominatim.openstreetmap.org/search?q=135+pilkington+avenue,+birmingham&format=json
            
            
            bikeData1 %<>%
                filter(!is.na(lon)) %>%
                droplevels() 
            bikeData1 %<>% mutate(
                Fecha = paste0(
                    as.Date(bikeData1$FECHA, "%d/%m/%Y"),
                    " ",
                    RANGO.HORARIO
                ),
                popup = paste0(
                    "<b>Dónde:</b>",
                    DIRECCION.COMPLETA,
                    "<b> Cuándo:</b>",
                    FECHA,
                    "<b> Qué pasó:</b>",
                    TIPO.ACCIDENTE
                )
            ) %>%
                dplyr::filter(between(lat, 40.3, 40.6) &
                                  between(lon, -3.8, -3.5))
            bikeData1.df <-
                bikeData1 %>% split(bikeData1$LESIVIDAD)
            
            l <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)  
            
            l <- l #%>% setView(lng = -3.704109,lat = 40.436495,zoom = 14) #%>% setView(lng = -3.65,lat = 40.4,zoom = 12)
            switch(input$centrarMapa,
                   "Madrid" = l <- l,
                   "Chamberi" = l <- l %>% setView(lng = -3.704109,lat = 40.436495,zoom = 13),
                   "Latina" = l <- l %>% setView(lng = -3.74129,lat = 40.40246,zoom = 13),
                   "Plaza Castilla" = l <- l %>% setView(lng = -3.6892,lat = 40.4669,zoom = 13)
            )
            names(bikeData1.df) %>%
                purrr::walk(function(df) {
                    l <<- l %>%
                        addCircleMarkers(
                            data = bikeData1.df[[df]],
                            lng =  ~ lon,
                            lat =  ~ lat,
                            popup =  ~ popup,
                            color = "red",
                            stroke = FALSE,
                            fillOpacity = 0.8,
                            group = df,
                            clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)
                        )
                })
            
            l %>%
                addLayersControl(
                    overlayGroups = names(bikeData1.df),
                    options = layersControlOptions(collapsed = FALSE)
                )
        })
    })
    
    
    # Panel 2
    sliderMonth2 <- reactiveValues()
    observe({
        full.date <- as.POSIXct(input$slider2, tz = "GMT")
        
        sliderMonth2$Month <-
            as.character(monthStart(full.date))
    })
    
    observeEvent(input$slider2,{
        bikeData2 <-
            bikeData %>% filter(between(
                as.Date(dmy_hm(FECHA)),
                as.Date(input$slider2[1]),
                as.Date(input$slider2[2])
            )) %>% 
            mutate(Color = if_else(DIA.SEMANA == "SABADO" |DIA.SEMANA == "DOMINGO", "FinSema", "Diario" ),
                   MomentoDia = if_else(between(Hora,6,14),"Dia",if_else(between(Hora,15,22),"Tarde","Noche")))
        accHombre <- table(bikeData2$SEXO)["HOMBRE"] %>% as.numeric()
        accMujer <- table(bikeData2$SEXO)["MUJER"] %>% as.numeric()
        accGraves <- table(bikeData2$LESIVIDAD)["Heridos Graves"] %>% as.numeric()
        accLeves <- table(bikeData2$LESIVIDAD)["Heridos Leves"] %>% as.numeric()
        accIlesos <- table(bikeData2$LESIVIDAD)["Ilesos"] %>% as.numeric()
        acc15 <- table(bikeData2$nuevosTramosEdad)["15-29"] %>% as.numeric()
        acc30 <- table(bikeData2$nuevosTramosEdad)["30-49"] %>% as.numeric()
        acc50 <- table(bikeData2$nuevosTramosEdad)["50+"] %>% as.numeric()
        
        output$Hombre <- renderText(accHombre)
        output$Mujer <- renderText(accMujer)
        output$Graves <- renderText(accGraves)
        output$Leves <- renderText(accLeves)
        output$Ilesos <- renderText(accIlesos)
        output$acc15 <- renderText(acc15)
        output$acc30 <- renderText(acc30)
        output$acc50 <- renderText(acc50)
        
        output$PorcHombre <- renderText(paste0(round(accHombre/(accHombre+accMujer),digits = 4)*100," %"))
        output$PorcMujer <- renderText(paste0(round(accMujer/(accHombre+accMujer),digits = 4)*100," %"))
        output$PorcGraves <- renderText(paste0(round(accGraves/(accGraves+accLeves+accIlesos),digits = 4)*100," %"))
        output$PorcLeves <- renderText(paste0(round(accLeves/(accGraves+accLeves+accIlesos),digits = 4)*100," %"))
        output$PorcIlesos <- renderText(paste0(round(accIlesos/(accGraves+accLeves+accIlesos),digits = 4)*100," %"))
        output$PorcAcc15 <- renderText(paste0(round(acc15/(acc15+acc30+acc50),digits = 4)*100," %"))
        output$PorcAcc30 <- renderText(paste0(round(acc30/(acc15+acc30+acc50),digits = 4)*100," %"))
        output$PorcAcc50 <- renderText(paste0(round(acc50/(acc15+acc30+acc50),digits = 4)*100," %"))
        
        # Fila 2
        distritosPeligrosos <- bikeData2 %>% group_by(DISTRITO) %>% summarise(Suma = n()) %>% arrange(desc(Suma)) %>% head(3)
        
        output$dist1 <- renderText({distritosPeligrosos$Suma[1]})
        output$dist2 <- renderText({distritosPeligrosos$Suma[2]})
        output$dist3 <- renderText({distritosPeligrosos$Suma[3]})
        
        output$distNombre1 <- renderText({distritosPeligrosos$DISTRITO[1] %>% as.character()})
        output$distNombre2 <- renderText({distritosPeligrosos$DISTRITO[2] %>% as.character()})
        output$distNombre3 <- renderText({distritosPeligrosos$DISTRITO[3] %>% as.character()})
        
        output$PorcDist1 <- renderText({paste0(round(distritosPeligrosos$Suma[1]/(nrow(bikeData2)),digits = 4)*100," %")})
        output$PorcDist2 <- renderText({paste0(round(distritosPeligrosos$Suma[2]/(nrow(bikeData2)),digits = 4)*100," %")})
        output$PorcDist3 <- renderText({paste0(round(distritosPeligrosos$Suma[3]/(nrow(bikeData2)),digits = 4)*100," %")})
        
        momentosDia <- bikeData2 %>% group_by(MomentoDia) %>% summarise(Conteo = n()) %>% arrange(MomentoDia)
        
        output$momento1 <- renderText({momentosDia$Conteo[1] %>% as.character()})
        output$momento2 <- renderText({momentosDia$Conteo[3] %>% as.character()})
        output$momento3 <- renderText({momentosDia$Conteo[2] %>% as.character()})
        
        output$PorcMomento1 <- renderText({paste0(round(momentosDia$Conteo[1]/(nrow(bikeData2)),digits = 4)*100," %")})
        output$PorcMomento2 <- renderText({paste0(round(momentosDia$Conteo[3]/(nrow(bikeData2)),digits = 4)*100," %")})
        output$PorcMomento3 <- renderText({paste0(round(momentosDia$Conteo[2]/(nrow(bikeData2)),digits = 4)*100," %")})
        
        tipoAccidente <- bikeData2 %>% group_by(TIPO.ACCIDENTE) %>% summarise(Cuenta = n()) %>% arrange(desc(Cuenta))
        
        output$tipoAccidente1 <- renderText({tipoAccidente$Cuenta[1]})
        output$tipoAccidente2 <- renderText({tipoAccidente$Cuenta[2]})
        output$tipoAccidente3 <- renderText({tipoAccidente$Cuenta[3]})
        
        output$tipoAccidenteNombre1 <- renderText({tipoAccidente$TIPO.ACCIDENTE[1] %>% as.character()})
        output$tipoAccidenteNombre2 <- renderText({tipoAccidente$TIPO.ACCIDENTE[2] %>% as.character()})
        output$tipoAccidenteNombre3 <- renderText({tipoAccidente$TIPO.ACCIDENTE[3] %>% as.character()})
        
        output$PorcTipoAccidente1 <- renderText({paste0(round(tipoAccidente$Cuenta[1]/(nrow(bikeData2)),digits = 4)*100," %")})
        output$PorcTipoAccidente2 <- renderText({paste0(round(tipoAccidente$Cuenta[2]/(nrow(bikeData2)),digits = 4)*100," %")})
        output$PorcTipoAccidente3 <- renderText({paste0(round(tipoAccidente$Cuenta[3]/(nrow(bikeData2)),digits = 4)*100," %")})
        
        # Fila 3
        output$plotPorDias <- renderPlot({
            bikeData2$DIA.SEMANA <- factor(bikeData2$DIA.SEMANA,levels(bikeData2$DIA.SEMANA)[c(3,4,5,2,7,6,1)])
            ggplot(bikeData2, aes(x=DIA.SEMANA)) +
                geom_bar(aes(fill=Color)) +
                labs(x = "Dia", y = "Accidentes")
            
        })
        
        meses <- factor(c("Enero","Febrero","Marzo","Abril",
                          "Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"),levels=c("Enero","Febrero","Marzo","Abril",
                                                                                                                   "Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
        output$plotPorMeses <- renderPlot({
            ggplot(bikeData2, aes(x=meses[month(FECHA)])) +
                geom_bar(aes(fill=Color), position = "dodge") +
                labs(x = "MES", y = "Accidentes")
            
        })
        
    })
    
    
    
    
    # Marquesina
    
    output$marquesina <- renderText({
        firstDate <- bikeData$FECHA %>% head()
        lastDate <-  bikeData$FECHA %>% tail()
        firstDateFormat <-
            dmy_hm(as.character(firstDate[[1]]))
        lastDateFormat <-
            dmy_hm(as.character(lastDate[[length(lastDate)]]))
        
        paste0(
            "<MARQUEE> ",
            "Analizando datos desde: ",
            firstDateFormat,
            " hasta ",
            lastDateFormat,
            " </MARQUEE>"
        )
    })
}


shinyApp(ui, server)
