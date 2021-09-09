library(shiny)
library(shiny.router)
library(geojsonio)
library(highcharter)
library(shinyjs)
library(tidyverse)
library(shinycssloaders)
library(shinyBS)
library(xts)
library(readr)
library(DT)
library(data.table)
library(shinyWidgets)



t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

data_url_server <- "https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/data/"
data_url <- "./data/"
data_source <- "server"

get_path <- function(file_path) {
  if (data_source == "server") {
    return(url(paste0(data_url_server, file_path)))
  } else {
    return(paste0(data_url, file_path))
  }
}

#carga de datos de españa
data.spain <- read.csv(get_path("spain_covid_dataset.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
data.spain$Comunidad <- as.character(data.spain$Comunidad)
data.spain$Fecha <- as.Date(data.spain$Fecha, format = "%Y-%m-%d")

#carga de datos en las ucis de españa
data.uci.spain <- read.csv(get_path("ucispain.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")

#datos de vacunación
data.spain.vac <- read.csv(get_path("vacspain.csv"), stringsAsFactors = FALSE)

#mapa de españa
spain.map <- geojson_read("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/data/simple_spain.geojson")

#datos mundiales
#whodata <- read.csv("./data/whodata.csv", stringsAsFactors = FALSE)
whodata <- read.csv(get_path("whodata.csv"), stringsAsFactors=FALSE, fileEncoding="latin1",skipNul = TRUE)
whodata["Date_reported"] <- lapply(whodata["Date_reported"], as.Date)
popcsv <- read_csv(get_path("population.csv"))

spainpopulation <- (popcsv %>% dplyr::filter(Country == "Spain"))$PopTotal

#cambio de nombres
cambio_nombres <- function(df){
  #dataframe con columna Country
  
  df[df$Country == "Bahamas",]$Country <- "The Bahamas"
  df[df$Country == "Bolivia (Plurinational State of)", ]$Country <- "Bolivia"
  df[df$Country == "Brunei Darussalam", ]$Country <- "Brueni"
  df[df$Country == "Cabo Verde", ]$Country <- "Cape Verde"
  df[df$Country == "Congo", ]$Country <- "Republic of the Congo"
  df[df$Country == "Côte d’Ivoire", ]$Country <- "Ivory Coast"
  df[df$Country == "Czechia", ]$Country <- "Czech Republic"
  df[df$Country == "Democratic People's Republic of Korea", ]$Country <- "North Korea"
  df[df$Country == "Guinea-Bissau", ]$Country <- "Guinea Bissau"
  df[df$Country == "Holy See", ]$Country <- "Vatican"
  df[df$Country == "Iran (Islamic Republic of)", ]$Country <- "Iran"
  df[df$Country == "Kosovo[1]", ]$Country <- "Kosovo"
  df[df$Country == "Lao People's Democratic Republic", ]$Country <- "Laos"
  df[df$Country == "Micronesia (Federated States of)", ]$Country <- "Federated States of Micronesia"
  df[df$Country == "North Macedonia", ]$Country <- "Macedonia"
  df[df$Country == "Northern Mariana Islands (Commonwealth of the)", ]$Country <- "Northern Mariana Islands"
  df[df$Country == "Republic of Korea", ]$Country <- "South Korea"
  df[df$Country == "Republic of Moldova", ]$Country <- "Moldova"
  df[df$Country == "Russian Federation", ]$Country <- "Russia"
  df[df$Country == "Serbia", ]$Country <- "Republic of Serbia"
  df[df$Country == "Syrian Arab Republic", ]$Country <- "Syria"
  df[df$Country == "The United Kingdom", ]$Country <- "United Kingdom"
  df[df$Country == "Venezuela (Bolivarian Republic of)", ]$Country <- "Venezuela"
  df[df$Country == "Viet Nam", ]$Country <- "Vietnam"  
  
  return(df)
} 

#whodata <- cambio_nombres(whodata)
popcsv <- cambio_nombres(popcsv)

#poblacion por comunidades
pobcoms <- data.frame(Comunidad = c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha" , "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "Ceuta", "Melilla"), stringsAsFactors = FALSE, Poblacion = c(8464411, 1329391, 1018784, 1171543, 2175952, 582905, 2394918, 2045221, 7780479, 5057353, 1063987, 2701819, 319914, 6779888, 1511251, 661197, 2220504, 84202, 87076))

#mapa mundial
#load("worldgeojson.RData")
data(worldgeojson, package = "highcharter")

#añadimos nombre en español
nombres_esp <- read.csv(get_path("paises.csv"), stringsAsFactors = FALSE, encoding = "UTF-8") %>% dplyr::select(nombre, iso2)
whodata <- left_join(whodata, nombres_esp, by = c("Country_code" = "iso2"))
whodata[which(whodata$Country == "Kosovo"),]$nombre <- "Kosovo"


#carga del modelo de prediccion
githubURL <- "https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/data/modelogauss.RData"
download.file(githubURL,"modelogauss.RData")
load("modelogauss.RData")
ccaas <- read_csv(get_path("ccaas.csv"))

timeoutSeconds <- 300

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)



#definimos la función de botón

valueBox <- function(value, subtitle, color) {
  div(class = "col-lg-12 col-md-12",
      div(
          style = "color:white;margin-bottom:20px",
          div(class = "panel-heading", style = paste0("background-color:", color, ";border-radius:6px"),
              div(class = "row",
                  div(class = ("text-right"),
                      style = "font-size:16px; padding-right:5px; text-align:center;",
                          HTML(paste0('<b style="font-size:40px">',value, '</b> ', subtitle))
                  )
              )
          )
      )
  )
}


world_page <- div(class="container_text",
                  
                  div(style="margin-top:-40px",
                      fluidRow(
                              column(6,
                                     valueBox("COVID-19", "en todo el mundo", "#00E7E7"),
                                      ),
                              column(6,
                                      valueBox(format(max(whodata$Date_reported), "%d-%m-%Y"), "última actualización", "#4DB1FF"),
                                      )
                               
                               ),
                    tabsetPanel(
                      tabPanel("Información General",
                               fluidRow(
                                 
                                 column(8,
                                        div(align = "center",
                                            prettyRadioButtons(
                                              inputId = "radiomapamundial",
                                              label = "", 
                                              choiceNames = c("Incidencia Acumulada", "Porcentaje de Casos totales", "Porcentaje de Fallecidos totales"),
                                              choiceValues = c("ia", "pcd", "pfd"),
                                              icon = icon("check"), 
                                              bigger = TRUE,
                                              status = "info",
                                              animation = "jelly",
                                              inline = TRUE
                                            )
                                        ), 
                                        div(
                                          highchartOutput("highchartmapamundial", width="100%", height="650px") %>% withSpinner(type = 6, color = "#000CCC"),
                                          h6("*Se calcula la Incidencia Acumulada para 14 días como: Casos durante 14 días/Población sana al inicio del periodo por 100000 habitantes."),
                                          h6("**Porcentajes de casos y fallecidos respecto a la población total de cada país. Población obtenida para el año 2019 de la página de las Naciones Unidas.")
                                        )
                                 ),
                                 column(4, 
                                        highchartOutput("highchartnuevoscasos", width = "100%", height = "350px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        highchartOutput("highchartnuevosfallecidos", width = "100%", height = "350px") %>% withSpinner(type = 6, color = "#000CCC")
                                 ),
                               )
                        
                      ),
                      tabPanel("Tabla",
                               div(style="margin-bottom:10px;"),
                               DTOutput("datatableworld")
                      )
                    )


                  )
)

spain_page <- div(class="container_text",
                  div(style="margin-top:-40px",
                      fluidRow(
                        column(6,
                               valueBox("COVID-19", "en España", "#00E7E7"),
                        ),
                        column(6,
                               valueBox(format(as.Date(max(data.spain$Fecha), format = "%Y-%m-%d"), "%d-%m-%Y"), "última actualización", "#4DB1FF"),
                        )
                        
                      ),

                    tabsetPanel(
                      tabPanel("Información General",
                               fluidRow(
                                 div(style="margin-bottom:10px;"),

                                 column(8,
                                        highchartOutput("highchartincidenciaaucumulada", width="100%", height="700px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        h6("*Se calcula la Incidencia Acumulada para 14 días como: Casos durante 14 días/Población sana al inicio del periodo por 100000 habitantes."),
                                        h6("**Cálculo con respecto a la población por Comunidad Autónoma de acuerdo a los datos del INE en 2020.")
                                 ),
                                 column(4,
                                        highchartOutput("highchartnuevoscasosspain", width = "100%", height = "240px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        highchartOutput("highchartnuevosfallecidosspain", width = "100%", height = "240px") %>% withSpinner(type = 6, color = "#000CCC"),
                                        fluidRow(
                                          column(6,
                                                 highchartOutput("highchartucispain", width = "100%", height = "220px") %>% withSpinner(type = 6, color = "#000CCC"),
                                                 ),
                                          column(6,
                                                 highchartOutput("highcharthopitalspain", width = "100%", height = "220px") %>% withSpinner(type = 6, color = "#000CCC"),
                                                 )
                                        )
                                        
                                 ),
                               )
                               
                               
                      ),
                      tabPanel("Análisis por Hitos",
                               div(style="margin-bottom:10px;"),
                               selectInput("ccaa", "Elige la CCAA", choices = unique((data.spain %>% dplyr::arrange(Comunidad))$Comunidad), selected = "España"),
                               h3("Efemíredes:"),
                               div(
                                 column(3, 
                                        checkboxInput("checkboxhosteleria", label = HTML('<p style="color:orange">Hosteleria (nacional)</p>'), value = FALSE),
                                 ),
                                 column(2, 
                                        checkboxInput("checkboxfestivos", label = HTML('<p style="color:red">Festivos</p>'), value = FALSE),
                                 ),
                                 column(2,
                                        checkboxInput("checkboxsalidas", label = HTML('<p style="color:green">Salidas</p>'), value = FALSE),
                                 ),
                                 column(3,
                                        checkboxInput("checkboxolas", label = HTML('<p style="color:blue">Estado de Alarma/Cepas</p>'), value = FALSE),
                                 ),
                                 column(2,
                                        checkboxInput("checkboxmis", label = HTML('<p style="color:purple">Misceláneo</p>'), value = FALSE),
                                 )
                                 
                               ),
                               div(style="margin-top:50px;",
                                 tabsetPanel(
                                   tabPanel("Incidencia acumulada",
                                            highchartOutput("highchartincidenciadiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                                   ),
                                   tabPanel("Casos",
                                            highchartOutput("highchartcasosdiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                                   ),
                                   tabPanel("Fallecidos",
                                            highchartOutput("highchartfallecidosdiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                                   )                               
                                 )
                               )
                      ),
                      tabPanel("Comparación entre CCAA",
                               fluidRow(
                                 div(
                                   style="margin-top:20px;",
                                   fluidRow(
                                     column(4, 
                                            selectInput("ccaa_1", HTML('Seleccione la comunidad en <b style="color:green">verde</b>:'), choices = unique((data.spain %>% dplyr::filter(Comunidad != "España") %>% dplyr::arrange(Comunidad))$Comunidad), selected = "Comunidad Valenciana")
                                            ),
                                     column(4, 
                                            selectInput("ccaa_2", HTML('Seleccione la comunidad en <b style="color:blue">azul</b>:'), choices = unique((data.spain %>% dplyr::filter(Comunidad != "España") %>%  dplyr::arrange(Comunidad))$Comunidad), selected = "Comunidad de Madrid")
                                            ),
                                     column(4, 
                                            checkboxInput("checkboxesp", label = HTML('¿Deseas incluir datos <b style="color:red">nacionales</b>?'), value = FALSE)
                                            )
                                   ),
                                   div(),
                                   fluidRow(
                                     column(4,
                                            highchartOutput("highchartcomparacion1", width = "100%", height = "300px") %>% withSpinner(type = 6, color = "#000CCC"),
                                            ),
                                     column(4,
                                            highchartOutput("highchartcomparacion2", width = "100%", height = "300px") %>% withSpinner(type = 6, color = "#000CCC"),
                                     ),
                                     column(4,
                                            highchartOutput("highchartcomparacion3", width = "100%", height = "300px") %>% withSpinner(type = 6, color = "#000CCC"),
                                     ),
                                   ),
                                   fluidRow(
                                     column(4,
                                            highchartOutput("highchartcomparacion4", width = "100%", height = "300px") %>% withSpinner(type = 6, color = "#000CCC"),
                                     ),
                                     column(4,
                                            highchartOutput("highchartcomparacion5", width = "100%", height = "300px") %>% withSpinner(type = 6, color = "#000CCC"),
                                     ),
                                     column(4,
                                            highchartOutput("highchartcomparacion6", width = "100%", height = "300px") %>% withSpinner(type = 6, color = "#000CCC"),
                                     ),
                                   )
                                 )
                               )
                      ),
                      tabPanel("Vacunación",
                               fluidRow(
                                 div(

                                          
                                   column(8,
                                          highchartOutput("highchartvacspain", height = "800px") %>% withSpinner(type = 6, color = "#000CCC")
                                    ),
                                   column(4, 
                                          highchartOutput("highchartvacunaciondosis", width = "100%", height = "450px") %>% withSpinner(type = 6, color = "#000CCC"),
                                          highchartOutput("highchartpercvacspain", width = "100%", height = "350px") %>% withSpinner(type = 6, color = "#000CCC"),
                                          
                                   ),
                                  )
                               )
                      ),
                      tabPanel("Tabla",
                               div(style="margin-bottom:10px;"),
                               DTOutput("datatablespain")
                      )
                    )
                    
                    
                  )
                )

prediction_spain_page <- div(class="container_text",
                  div(
                    style="margin-top:-40px;text-align:justify",
                  ),
                  div(
                    highchartOutput("highchartprediccion", width="100%", height="700px") %>% withSpinner(type = 6, color = "#000CCC"),
                  ),
                  div(style="text-align:justify",
                      h5("Modelo de predicción de la Incidencia Acumulada a 14 días por cada 100000 habitantes en España. Se ha desarrollado un modelo de regresión generalizada para realizar una estimación de la incidencia acumulada para el día siguiente por medio de la incidencia acumulada en días previos, el ritmo de vacunación y la festividad 14 días previos, entre otros factores, empleando una función de distribución normal. El modelo presenta un RMSE de 18.45068, siendo el menor de todos los modelos probados.")
                      )
)

spain_page_server <- function(input, output, session) {
  
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  

  
  output$highchartcasosdiarios <- renderHighchart({
    if (input$checkboxhosteleria == TRUE){
      color_host <- "orange"
    } else {
      color_host <- t_col("orange", perc = 100)
    }
    
    if (input$checkboxfestivos == TRUE){
      color_fest <- "red"
    } else {
      color_fest <- t_col("red", perc = 100)
    }
    
    if (input$checkboxolas == TRUE){
      color_olas <- "blue"
    } else {
      color_olas <- t_col("blue", perc = 100)
    }
    
    if (input$checkboxsalidas == TRUE){
      color_salidas <- "green"
    } else {
      color_salidas <- t_col("green", perc = 100)
    }
    
    if (input$checkboxmis == TRUE){
      color_misc <- "purple"
    } else {
      color_misc <- t_col("purple", perc = 100)
    }
    
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$CasosDiarios, order.by = as.Date(data$Fecha))
    highchart(type = "stock") %>%
      hc_add_series(name = "Casos", series1, color = "#00FFAA") %>% 
      hc_xAxis(plotLines = list(
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Se acaba el stock de papel higiénico</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Correción de casos en Cataluña</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_salidas, '">Niños menores de 14 años pueden salir</p>')),
          color = color_salidas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-04-27", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_salidas, '">Se permite salir a pasear/hacer deporte</p>')),
          color = color_salidas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura de los primeros bares</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Fin del primer estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-07-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-08-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-07-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-08-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio de la Semana Santa</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin de la Semana Santa</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-04-05", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Puente de la Constitución</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Puente del Pilar</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-12", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">"No fiestas" de agosto</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-08-15", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Halloween</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Fin del segundo estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-08", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Mascarillas dejan de ser obligatorias exteriores</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Adelanto horario del cierre hosteleria</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-11-15", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Navidad</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-24", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Navidad</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-01-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Cominezo de la segunda ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin segunda ola y comienzo tercera</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-09", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la tercera ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-12", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Comienzo de la cuarta ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-04-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la cuarta ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-24", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la primera ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Se aprueba la primera vacuna contra el COVID-19</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Primer caso cepa británica en España</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Primer caso cepa india en España</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-03", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura de los primeros restaurantes en España</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-25", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura del ocio nocturno</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-08", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Segundo estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-25", tz = "UTC"))
        )
      )
      )
  })
  
  output$highchartincidenciadiarios <- renderHighchart({
    if (input$checkboxhosteleria == TRUE){
      color_host <- "orange"
    } else {
      color_host <- t_col("orange", perc = 100)
    }
    
    if (input$checkboxfestivos == TRUE){
      color_fest <- "red"
    } else {
      color_fest <- t_col("red", perc = 100)
    }
    
    if (input$checkboxolas == TRUE){
      color_olas <- "blue"
    } else {
      color_olas <- t_col("blue", perc = 100)
    }
    
    if (input$checkboxsalidas == TRUE){
      color_salidas <- "green"
    } else {
      color_salidas <- t_col("green", perc = 100)
    }
    
    if (input$checkboxmis == TRUE){
      color_misc <- "purple"
    } else {
      color_misc <- t_col("purple", perc = 100)
    }
    
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$IA14, order.by = as.Date(data$Fecha, format = "%Y-%m-%d"))
    highchart(type = "stock") %>%
      hc_add_series(name = "Incidencia acumulada", series1, color = "#00FFAA") %>% 
      hc_xAxis(plotLines = list(
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Se acaba el stock de papel higiénico</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_salidas, '">Niños menores de 14 años pueden salir</p>')),
          color = color_salidas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-04-27", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_salidas, '">Se permite salir a pasear/hacer deporte</p>')),
          color = color_salidas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura de los primeros bares</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Fin del primer estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-07-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-08-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-07-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-08-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio de la Semana Santa</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin de la Semana Santa</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-04-05", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Puente de la Constitución</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Puente del Pilar</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-12", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">"No fiestas" de agosto</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-08-15", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Halloween</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Fin del segundo estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-08", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Mascarillas dejan de ser obligatorias exteriores</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Adelanto horario del cierre hosteleria</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-11-15", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Navidad</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-24", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Navidad</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-01-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Cominezo de la segunda ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin segunda ola y comienzo tercera</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-09", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la tercera ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-12", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Comienzo de la cuarta ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-04-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la cuarta ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-24", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la primera ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Se aprueba la primera vacuna contra el COVID-19</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Primer caso cepa británica en España</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Primer caso cepa india en España</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-03", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura de los primeros restaurantes en España</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-25", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura del ocio nocturno</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-08", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Segundo estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-25", tz = "UTC"))
        )
      )
      )
  })
  
  output$highchartfallecidosdiarios <- renderHighchart({
    if (input$checkboxhosteleria == TRUE){
      color_host <- "orange"
    } else {
      color_host <- t_col("orange", perc = 100)
    }
    
    if (input$checkboxfestivos == TRUE){
      color_fest <- "red"
    } else {
      color_fest <- t_col("red", perc = 100)
    }
    
    if (input$checkboxolas == TRUE){
      color_olas <- "blue"
    } else {
      color_olas <- t_col("blue", perc = 100)
    }
    
    if (input$checkboxsalidas == TRUE){
      color_salidas <- "green"
    } else {
      color_salidas <- t_col("green", perc = 100)
    }
    
    if (input$checkboxmis == TRUE){
      color_misc <- "purple"
    } else {
      color_misc <- t_col("purple", perc = 100)
    }
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$FallecidosDiarios, order.by = as.Date(data$Fecha, format = "%Y-%m-%d"))
    highchart(type = "stock") %>%
      hc_add_series(name = "Fallecidos", series1, color = "#00FFAA") %>% 
      hc_xAxis(plotLines = list(
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Se acaba el stock de papel higiénico</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-03-20", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_salidas, '">Niños menores de 14 años pueden salir</p>')),
          color = color_salidas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-04-27", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_salidas, '">Se permite salir a pasear/hacer deporte</p>')),
          color = color_salidas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura de los primeros bares</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-11", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Fin del primer estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-07-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-08-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-07-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Verano</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-08-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio de la Semana Santa</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin de la Semana Santa</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-04-05", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Puente de la Constitución</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Puente del Pilar</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-12", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">"No fiestas" de agosto</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-08-15", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Halloween</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Fin del segundo estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-08", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_misc, '">Mascarillas dejan de ser obligatorias exteriores</p>')),
          color = color_misc,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-06-21", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Adelanto horario del cierre hosteleria</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-11-15", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Inicio del periodo de Navidad</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-24", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_fest, '">Fin del periodo de Navidad</p>')),
          color = color_fest,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-01-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Cominezo de la segunda ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin segunda ola y comienzo tercera</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-09", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la tercera ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-03-12", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Comienzo de la cuarta ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-04-01", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la cuarta ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-24", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:fuchsia">Fin de la primera ola de la pandemia</p>')),
          color = "fuchsia",
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-31", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Se aprueba la primera vacuna contra el COVID-19</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-02", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Primer caso cepa británica en España</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-12-23", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Primer caso cepa india en España</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2021-05-03", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura de los primeros restaurantes en España</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-05-25", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_host, '">Apertura del ocio nocturno</p>')),
          color = color_host,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-06-08", tz = "UTC"))
        ),
        list(
          label = list(text = paste0('<p style="color:', color_olas, '">Segundo estado de alarma</p>')),
          color = color_olas,
          width = 1,
          value = datetime_to_timestamp(as.Date("2020-10-25", tz = "UTC"))
        )
      )
      )
  })
  
  output$highchartincidenciaaucumulada <- renderHighchart({
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClickedspain', event.point.Comunidad);}") 
    data <- data.spain %>% dplyr::filter(Fecha == max(unique(data.spain$Fecha)))
    data$tasacasos <- round(100*(data$Casos/spainpopulation), 2)
    data <- left_join(data, pobcoms, by = "Comunidad")
    data$tasamuertos <- round(100*(data$Fallecidos/data$Poblacion), 2)
    data <- data %>% dplyr::filter(Comunidad != "España")
    
    mapa <- highchart(type="map") %>%
      hc_add_series_map(spain.map, 
                        data,
                        "IA14", c("NAME", "Comunidad"), 
                        nullColor="#F0F0F0", borderColor="#fff",
                        borderWidth=1,
                        dataLabels = list(style = list(fontSize = '12'),enabled = TRUE, format = '{point.name}: {point.value}', color = "white", backgroundColor = 'rgba(0,0,0,0.5)', borderRadius = 7, padding = 5)) %>%
      hc_title(text = "Incidencia Acumulada a 14 días") %>%
      hc_credits(enabled = TRUE, text = 'Fuente: Ministerio de Sanidad', href = 'https://www.mscbs.gob.es/',  target = '_blank') %>%
      hc_tooltip(headerFormat="<b>{point.point.Comunidad}</b><br>",
                 pointFormat="Incidencia acumulada: <b>{point.value}*</b> <br> Número de casos: <b>{point.Casos}</b> <br> Porcentaje de casos: <b>{point.tasacasos}</b>%** <br> Número de fallecidos: <b>{point.Fallecidos}</b> <br> <br> Porcentaje de fallecidos: <b>{point.tasamuertos}</b>%**") %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer"))
    hc_colorAxis(mapa, minColor = "#CC99FF", maxColor = "#6400C7")
    hc_colorAxis(mapa, min = 0, max = round(max(na.omit(data)$IA14)+50, -2), minColor = "#00CCFF", maxColor = "#0F00FF")  
    
  })

  data.spain.filtrado <- reactive({
    if(!is.null(input$hcClickedspain)){
      data.spain %>% dplyr::filter(Comunidad == input$hcClickedspain) %>% dplyr::select(Comunidad, Fecha, CasosDiarios, FallecidosDiarios)
    } else{
      data.spain %>% dplyr::filter(Comunidad == "España") %>% dplyr::select(Fecha, CasosDiarios, FallecidosDiarios)
    }
  })
  
  output$highchartnuevoscasosspain <- renderHighchart({
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Casos diarios en ", unique(data.spain.filtrado()$Comunidad))
    } else {
      titulo <- "Casos diarios en toda España"
    }
    data.spain.filtrado() %>%
      hchart("line", hcaes(x = Fecha, y = CasosDiarios), name = "Casos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartnuevosfallecidosspain <- renderHighchart({
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Fallecidos diarios en ", unique(data.spain.filtrado()$Comunidad))
    } else {
      titulo <- "Fallecidos diarios en toda España"
    }
    data.spain.filtrado() %>%
      hchart("line", hcaes(x = Fecha, y = FallecidosDiarios), name = "Muertos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartucispain <- renderHighchart({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Situación en las UCIS en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Situación en las UCIS en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    col_stops <- data.frame(
      q = c(0, 0.4, 0.7, 0.9),
      c = c('#55FF33', '#A3DF0D', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        center = list('50%', '70%'),
        size = '100%',
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = data$PerCamasUCI,
        dataLabels = list(
          y = -40,
          borderWidth = 0,
          useHTML = TRUE,
          format = paste('<p style="font-size:22px;text-align:center;margin-bottom:0px;"> {point.y}% </p><p style="font-size:10px;margin-top:0px;margin-bottom:-10px">Camas UCI ocupadas</p>')#<p style="text-align:center;font-size:10px;margin-top:0px">', fecha, '</p>')
        )
      ) %>% 
      hc_size(height = 300)
  })
  
  output$highcharthopitalspain <- renderHighchart({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Situación en las UCIS en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Situación en las UCIS en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    col_stops <- data.frame(
      q = c(0, 0.4, 0.7, 0.9),
      c = c('#55FF33', '#A3DF0D', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        center = list('50%', '70%'),
        size = '100%',
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = data$PercCamasCovid,
        dataLabels = list(
          y = -40,
          borderWidth = 0,
          useHTML = TRUE,
          format = paste('<p style="font-size:22px;text-align:center;margin-bottom:0px;"> {point.y}% </p><p style="font-size:10px;margin-top:0px;margin-bottom:-10px">Camas de hospital ocupadas</p><br>')
        )
      ) %>% 
      hc_size(height = 300)
  })
  
  output$highchartvacspain <- renderHighchart({
    
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClickedspainvac', event.point.Comunidad);}") 
    
    data.spain.covid.filter <- data.spain.vac %>% dplyr::filter(fecha == max(data.spain.vac$fecha))
    data.spain.covid.filter$PercPfizer <- round(100*data.spain.covid.filter$Pfizer/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$PercModerna <- round(100*data.spain.covid.filter$Moderna/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$PercAstraZeneca <- round(100*data.spain.covid.filter$AstraZeneca/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$PercJanssen <- round(100*data.spain.covid.filter$Janssen/data.spain.covid.filter$Entregadas, 2)
    data.spain.covid.filter$percvacunados <- round(data.spain.covid.filter$percvacunados ,2)
    
    data.spain.covid.filter <- data.spain.covid.filter %>% dplyr::filter(Comunidad != "España")
    
    mapa <- highchart(type="map") %>%
      hc_add_series_map(spain.map, 
                        data.spain.covid.filter,
                        "percvacunados", c("NAME", "Comunidad"), 
                        nullColor="#F0F0F0", borderColor="#fff",
                        borderWidth=1,
                        dataLabels = list(style = list(fontSize = '12'),enabled = TRUE, format = '{point.name}: {point.value}%', color = "white", backgroundColor = 'rgba(0,0,0,0.5)', borderRadius = 7, padding = 5)) %>%
      hc_title(text = "Porcentaje de la población vacunada") %>%
      hc_credits(enabled = TRUE, text = 'Fuente: Ministerio de Sanidad', href = 'https://www.mscbs.gob.es/',  target = '_blank') %>%
      hc_tooltip(
                 headerFormat="<b>{point.point.Comunidad}</b><br>",
                 pointFormat='Porcentaje de personas vacunadas (pauta completa): <b>{point.value}</b>% <br> Porcentaje de dosis administradas: <b>{point.PercAdministradas}</b>%<br> Dosis entregadas totales: <b>{point.Entregadas}</b> <br> Dosis entregadas de <span style="color:blue">Pfizer</span>: <b>{point.PercPfizer}</b>% <br> Dosis entregadas de <span style="color:green">Moderna</span>: <b>{point.PercModerna}</b>% <br> Dosis entregadas de <span style="color:purple">AstraZeneca</span>: <b>{point.PercAstraZeneca}</b>% <br> Dosis entregadas de <span style="color:#FFE400">Janssen</span>: <b>{point.PercJanssen}</b>%') %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer"))
    hc_colorAxis(mapa, min = min(data.spain.covid.filter$percvacunados), max = max(data.spain.covid.filter$percvacunados), minColor = "#00CCFF", maxColor = "#0F00FF")  
    
  })
  
  output$highchartvacunaciondosis <- renderHighchart({
    data.spain.covid.filter <- data.spain.vac %>% dplyr::filter(fecha == max(data.spain.vac$fecha))
    
    if(!is.null(input$hcClickedspainvac)){
      data.spain.covid.filter <- data.spain.covid.filter %>% dplyr::filter(Comunidad == input$hcClickedspainvac)
      titulo <- paste0("Dosis entregadas en ", unique(data.spain.covid.filter$Comunidad))
      
    } else {
      data.spain.covid.filter <- data.spain.covid.filter %>% dplyr::filter(Comunidad == "España")
      titulo <- "Dosis entregadas en toda España"
    }
    
    df <- tibble(labels = c("Pfizer", "Moderna", "AstraZeneca", "Janssen"), values = c(data.spain.covid.filter$Pfizer, data.spain.covid.filter$Moderna, data.spain.covid.filter$AstraZeneca, data.spain.covid.filter$Janssen))
    df %>% 
      hchart(type = "pie", hcaes(name = labels, y = values, color = c("Pfizer" = "#0231C5", "Moderna" = "#40C502", "AstraZeneca" = "#7000FC", 
                                                                      "Janssen" = "#EAE305")), innserSize = "50%") %>%
      hc_plotOptions(series = list(innerSize = "50%")) %>%
      hc_title(text = titulo) %>%
      hc_tooltip(headerFormat="", pointFormat = paste('Vacuna: <b>{point.name}</b><br/>Cantidad entregadas: <b>{point.y}</b><br>Porcentaje entregadas: <b>{point.percentage:.1f}%</b>'), cursor="pointer")
  })
  
  output$highchartpercvacspain <- renderHighchart({
    fecha <- max(data.spain.vac$fecha)
    data.spain.covid.filter <- data.spain.vac %>% dplyr::filter(fecha == max(data.spain.vac$fecha))
    if(!is.null(input$hcClickedspainvac)){
      data <- data.spain.covid.filter %>% dplyr::filter(Comunidad == input$hcClickedspainvac)
      titulo <- paste0("Porcentaje con la pauta completa en ", unique(data.spain.covid.filter$Comunidad))
      
    } else {
      data <- tibble(Personas2Dosis = sum(data.spain.covid.filter$Personas2Dosis), Poblacion = sum(data.spain.covid.filter$Poblacion))
      titulo <- "Porcentaje con la pauta completa en toda España"
    }

    col_stops <- data.frame(
      q = c(0.15, 0.25, 0.5, 0.7),
      c = c('#DF5353', '#DDDF0D','#A3DF0D','#55FF33'),
      stringsAsFactors = FALSE
    )
    
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        center = list('50%', '70%'),
        size = '110%',
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = round(100*data$Personas2Dosis/data$Poblacion, 2),
        dataLabels = list(
          y = -60,
          borderWidth = 0,
          useHTML = TRUE,
          format = paste('<p style="font-size:28px;text-align:center;margin-bottom:0px;"> {point.y}% </p><p style="font-size:12px;margin-top:0px;margin-bottom:-10px">Porcentaje vacunados</p><br><p style="text-align:center;font-size:12px;margin-top:0px">', fecha, '</p>')
        )
      ) %>% 
      hc_size(height = 200)
  })
  
  data.spain.comparacion <- reactive({
    data <- setDT(data.spain)
    if(input$checkboxesp){
      data[which(Comunidad %in% c(input$ccaa_1, input$ccaa_2, "España"))]
    } else {
      data[which(Comunidad %in% c(input$ccaa_1, input$ccaa_2))]
    }
    })
  
  comparacion.color <- reactive({
    if(input$checkboxesp){
      c("blue", "green", "red")
    } else {
      c("blue", "green")
    }
  })
  
  output$highchartcomparacion1 <- renderHighchart({
    data.spain.comparacion() %>% 
      hchart("line", hcaes(x = Fecha, y = CasosDiarios, group = Comunidad), color = comparacion.color()) %>%
      hc_title(text = "Casos Diarios") %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(split = FALSE, shared = TRUE,
                 headerFormat='<b>{point.Fecha}</b><br>',
                 pointFormat='<span style="color:{point.color}">\u25CF</span>  <b>{point.Comunidad}</b>: <b>{point.y}</b><br>')
  })
  output$highchartcomparacion2 <- renderHighchart({
    data.spain.comparacion() %>% 
      hchart("line", hcaes(x = Fecha, y = FallecidosDiarios, group = Comunidad), color = comparacion.color()) %>%
      hc_title(text = "Fallecidos Diarios") %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(split = FALSE, shared = TRUE,
                 headerFormat='<b>{point.Fecha}</b><br>',
                 pointFormat='<span style="color:{point.color}">\u25CF</span>  <b>{point.Comunidad}</b>: <b>{point.y}</b><br>')
  })
  output$highchartcomparacion3 <- renderHighchart({
    data.spain.comparacion() %>% 
      hchart("line", hcaes(x = Fecha, y = IA14, group = Comunidad), color = comparacion.color()) %>%
      hc_title(text = "Incidencia Acumulada Diaria") %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(split = FALSE, shared = TRUE,
                 headerFormat='<b>{point.Fecha}</b><br>',
                 pointFormat='<span style="color:{point.color}">\u25CF</span>  <b>{point.Comunidad}</b>: <b>{point.y}</b><br>')
  })
  
  data.spain.comparacion.hosp <- reactive({
    data <- setDT(data.uci.spain)
    if(input$checkboxesp){
      data[which(CCAA %in% c(input$ccaa_1, input$ccaa_2, "España"))]
    } else {
      data[which(CCAA %in% c(input$ccaa_1, input$ccaa_2))]
    }
  })
  
  output$highchartcomparacion4 <- renderHighchart({
    na.omit(data.spain.comparacion.hosp()) %>% 
      hchart("line", hcaes(x = fecha, y = PercCamasCovid, group = CCAA), color = comparacion.color()) %>%
      hc_title(text = "Porcentaje de camas ocupadas en hospitales") %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(split = FALSE, shared = TRUE,
                 headerFormat='<b>{point.point.fecha}</b><br>',
                 pointFormat='<span style="color:{point.color}">\u25CF</span>  <b>{point.CCAA}</b>: <b>{point.y}</b>%<br>')
  })
  output$highchartcomparacion5 <- renderHighchart({
    data.spain.comparacion.hosp() %>% 
      hchart("line", hcaes(x = fecha, y = PerCamasUCI, group = CCAA), color = comparacion.color()) %>%
      hc_title(text = "Porcentaje de camas ocupadas en UCI") %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(split = FALSE, shared = TRUE,
                 headerFormat='<b>{point.point.fecha}</b><br>',
                 pointFormat='<span style="color:{point.color}">\u25CF</span>  <b>{point.CCAA}</b>: <b>{point.y}</b>%<br>')
  })
  
  output$highchartcomparacion6 <- renderHighchart({
    data <- data.spain.vac %>% dplyr::select(Comunidad, fecha, percvacunados)
    
    if(input$checkboxesp){
      data <- na.omit(setDT(data))[which(Comunidad %in% c(input$ccaa_1, input$ccaa_2, "España"))]
    } else {
      data <- na.omit(setDT(data))[which(Comunidad %in% c(input$ccaa_1, input$ccaa_2))]
    }
   data %>%
      hchart("line", hcaes(x = fecha, y = percvacunados, group = Comunidad), color = comparacion.color()) %>%
      hc_title(text = "Porcentaje de Vacunados") %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_tooltip(split = FALSE, shared = TRUE,
                 headerFormat='<b>{point.point.fecha}</b><br>',
                 pointFormat='<span style="color:{point.color}">\u25CF</span>  <b>{point.Comunidad}</b>: <b>{point.y}</b>%<br>')
  })
  
  
  output$highchartprediccion <- renderHighchart({
    week_num <- as.numeric(strftime(max(as.Date(data.spain$Fecha, format = "%Y-%m-%d")), format = "%V"))
    fechas <- as.Date(unique(data.spain$Fecha), format = "%Y-%m-%d")
    data.covid.spain.1 <- data.spain %>% dplyr::filter(Fecha == fechas[length(fechas)])
    data.covid.spain.1 <- data.covid.spain.1[!(data.covid.spain.1$Comunidad == "España"),] %>% dplyr::select(Comunidad, IA14)
    names(data.covid.spain.1) <- c("Comunidad", "IA14DiaAnterior")
    
    data.covid.spain.2 <- data.spain %>% dplyr::filter(Fecha == (fechas[length(fechas)-1]))
    data.covid.spain.2 <- data.covid.spain.2[!(data.covid.spain.2$Comunidad == "España"),] %>% dplyr::select(Comunidad, IA14)
    names(data.covid.spain.2) <- c("Comunidad", "IA14DosDiasAnterior")
    
    vac <- left_join(data.spain.vac %>% dplyr::filter(fecha == max(data.spain.vac$fecha)), ccaas, by = c("Comunidad" = "ccaa")) %>% dplyr::select(Comunidad, percvacunados, number)
    
    prim <- format(seq(as.Date("2020-03-20", format = "%Y-%m-%d"), as.Date("2020-06-21", format = "%Y-%m-%d"),by= "days"), "%m-%d")
    vera <- format(seq(as.Date("2020-06-22", format = "%Y-%m-%d"), as.Date("2020-09-22", format = "%Y-%m-%d"),by= "days"), "%m-%d")
    ot <- format(seq(as.Date("2020-09-23", format = "%Y-%m-%d"), as.Date("2020-12-21", format = "%Y-%m-%d"),by= "days"), "%m-%d")
    
    get_season <- function(x) {
      y <- format(x, "%m-%d")
      if (y %in% prim) {
        z <- 2
      } else if (y %in% vera) {
        z <- 3
      } else if (y %in% ot) {
        z <- 4
      } else {
        z <- 1
      }
      return(z)
    }
    
    df.modelo <- data.frame(CCAA = 1:19, festivo = 0, estacion = get_season(as.Date(max(data.spain$Fecha), format = "%Y-%m-%d")), week = sin((2*pi/53)*week_num))
    df.modelo <- left_join(df.modelo, vac, by = c("CCAA" = "number"))
    df.modelo <- left_join(df.modelo, data.covid.spain.1, by = c("Comunidad" = "Comunidad"))
    df.modelo <- left_join(df.modelo, data.covid.spain.2, by = c("Comunidad" = "Comunidad"))
    
    df.modelo$Comunidad <- NULL
    names(df.modelo)[1] <- "Comunidad"
    
    df.modelo[c(1,2,3)] <- lapply(df.modelo[c(1,2,3)] , as.factor)
    df.modelo$IA14 <- round(predict(modelo_gaus, newdata=df.modelo, type="response"), 2)
    
    df.modelo$Comunidad <- as.numeric(df.modelo$Comunidad)
    
    df.modelo <- left_join(df.modelo, ccaas, by = c("Comunidad" = "number")) %>% dplyr::select(ccaa, IA14)
    names(df.modelo) <- c("Comunidad", "IA14Predicha")
    
    week_day <- as.Date(paste0("2021-",as.character(week_num),"-01"), format = "%Y-%W-%w")
    
    mapa <- highchart(type="map") %>%
      hc_add_series_map(spain.map, 
                        df.modelo,
                        "IA14Predicha", c("NAME", "Comunidad"), 
                        nullColor="#F0F0F0", borderColor="#fff",
                        borderWidth=1,
                        dataLabels = list(enabled = TRUE, format = '{point.name}: {point.value}', color = "white", backgroundColor = 'rgba(0,0,0,0.5)', borderRadius = 7, padding = 5)) %>%
      hc_title(text = paste0("Predicción de la Incidencia Acumulada para el ", max(as.Date(data.spain$Fecha, format = "%Y-%m-%d")) + 1)) %>%
      hc_tooltip(headerFormat="<b>{point.point.Comunidad}</b><br>",
                 pointFormat="Incidencia acumulada: <b>{point.value}</b>") %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))))
    hc_colorAxis(mapa, min = 0, max = round(max(na.omit(df.modelo)$IA14Predicha)+50, -2), minColor = "#00CCFF", maxColor = "#0F00FF")  
    
  })
  
  
  output$highchartmapamundial <- renderHighchart({
    req(input$radiomapamundial)
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClicked', event.point.name);}")    
    data <- whodata
    data_max <- max(whodata$Date_reported)
    data_max_1 <- data_max - 1
    data_max_15 <- data_max - 15
    
    casos14dias <- data[data$Date_reported == data_max_1,]$Cumulative_cases - data[data$Date_reported == data_max_15,]$Cumulative_cases 
    data <- data %>% dplyr::filter(Date_reported == data_max_1)
    data <- left_join(data, popcsv, by = c("Country" = "Country"))
    data$ia <- round(casos14dias/(data$PopTotal - casos14dias) *100000, 2)
    
    data$tasamuertos <- round(100*(data$Cumulative_deaths/data$PopTotal), 2)
    data$tasacasos <- round(100*(data$Cumulative_cases/data$PopTotal), 2)
    
    if (input$radiomapamundial == "ia"){
      hc_mapa <- highchart() %>%
        hc_add_series_map(worldgeojson, data, value = "ia", joinBy = c("name", "Country"), name = "Incidencia acumulada") %>%
        hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer")) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_title(text = "Incidencia Acumulada a 14 días*") %>%
        hc_credits(enabled = TRUE, text = '', mapText = '<a target="_blank" rel="noopener noreferrer" href="https://covid19.who.int/">Fuente: OMS</a>') %>%
        hc_tooltip(headerFormat="<b>{point.point.nombre}</b><br>",
                   pointFormat="<b>Incidencia acumulada</b>: <b>{point.value}</b>* <br> Número de casos totales: <b>{point.Cumulative_cases}</b> <br> Porcentaje de casos totales: <b>{point.tasacasos}</b>%** <br> <br> Número de fallecidos totales: <b>{point.Cumulative_deaths}</b> <br> <br> Porcentaje de fallecidos totales: <b>{point.tasamuertos}</b>%**")
      hc_colorAxis(hc_mapa, min = 0, max = round(max(na.omit(data)$ia)+500, -3), minColor = "#00CCFF", maxColor = "#0F00FF")  
    } else if (input$radiomapamundial == "pcd") {
      hc_mapa <- highchart() %>%
        hc_add_series_map(worldgeojson, data, value = "tasacasos", joinBy = c("name", "Country"), name = "Porcentaje de casos totales") %>%
        hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer")) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_title(text = "Porcentaje de casos totales") %>%
        hc_credits(enabled = TRUE, text = '', mapText = '<a target="_blank" rel="noopener noreferrer" href="https://covid19.who.int/">Fuente: OMS</a>') %>%
        hc_tooltip(headerFormat="<b>{point.point.nombre}</b><br>",
                   pointFormat="Incidencia acumulada: <b>{point.ia}</b>* <br> Número de casos totales: <b>{point.Cumulative_cases}</b> <br> <b>Porcentaje de casos totales</b>: <b>{point.value}</b>%** <br> <br> Número de fallecidos totales: <b>{point.Cumulative_deaths}</b> <br> <br> Porcentaje de fallecidos totales: <b>{point.tasamuertos}</b>%**")
      hc_colorAxis(hc_mapa, min = 0, max = 100, minColor = "#00CCFF", maxColor = "#0F00FF")  
    } else if (input$radiomapamundial == "pfd") {
      hc_mapa <- highchart() %>%
        hc_add_series_map(worldgeojson, data, value = "tasamuertos", joinBy = c("name", "Country"), name = "Porcentaje de muertes de la población") %>%
        hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer")) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_title(text = "Porcentaje de fallecidos totales") %>%
        hc_credits(enabled = TRUE, text = '', mapText = '<a target="_blank" rel="noopener noreferrer" href="https://covid19.who.int/">Fuente: OMS</a>') %>%
        hc_tooltip(headerFormat="<b>{point.point.nombre}</b><br>",
                   pointFormat="Incidencia acumulada: <b>{point.ia}</b>* <br> Número de casos totales: <b>{point.Cumulative_cases}</b> <br> Porcentaje de casos totales: <b>{point.tasacasos}</b>%** <br> <br> Número de fallecidos totales: <b>{point.Cumulative_deaths}</b> <br> <br> <b>Porcentaje de fallecidos totales: </b> <b>{point.value}</b>%**")
      hc_colorAxis(hc_mapa, min = 0, max = 2, minColor = "#00CCFF", maxColor = "#0F00FF")  
    }
    

  })
  
  world.filtrado <- reactive({
    if(!is.null(input$hcClicked)){
      whodata %>% dplyr::filter(Country == input$hcClicked) %>% dplyr::select(Date_reported, New_cases, Cumulative_cases, New_deaths, Cumulative_deaths, nombre)
    } else{
      data <- whodata %>% dplyr::select(Date_reported, New_cases, Cumulative_cases, New_deaths, Cumulative_deaths) %>% group_by(Date_reported)
      aggregate(. ~ Date_reported, data, sum)
    }
  })
  
  output$highchartnuevoscasos <- renderHighchart({
    if(!is.null(input$hcClicked)){
      titulo <- paste0("Casos diarios en ", unique(world.filtrado()$nombre))
    } else {
      titulo <- "Casos diarios a nivel mundial"
    }
    world.filtrado() %>%
      hchart("line", hcaes(x = Date_reported, y = New_cases), name = "Casos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartnuevosfallecidos <- renderHighchart({
    if(!is.null(input$hcClicked)){
      titulo <- paste0("Fallecidos diarios en ", unique(world.filtrado()$nombre))
    } else {
      titulo <- "Fallecidos diarios a nivel mundial"
    }
    world.filtrado() %>%
      hchart("line", hcaes(x = Date_reported, y = New_deaths), name = "Muertos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })

  output$datatableworld <- renderDT({
    data <- whodata
    data_max <- max(whodata$Date_reported)
    data_max_1 <- data_max - 1
    data_max_15 <- data_max - 15
    casos14dias <- data[data$Date_reported == data_max_1,]$Cumulative_cases - data[data$Date_reported == data_max_15,]$Cumulative_cases 
    data <- data %>% dplyr::filter(Date_reported == data_max_1)
    data <- left_join(data, popcsv, by = c("Country" = "Country"))
    data$ia <- round(casos14dias/(data$PopTotal - casos14dias) *100000, 2)
  
    data$tasamuertos <- round(100*(data$Cumulative_deaths/data$PopTotal), 2)
    data$tasacasos <- round(100*(data$Cumulative_cases/data$PopTotal), 2)
    
    data <- data %>% dplyr::arrange(desc(Cumulative_cases))
    data <- data %>% dplyr::select("nombre", "Cumulative_cases", "New_cases", "tasacasos", "Cumulative_deaths", "New_deaths", "tasamuertos", "ia")
    names(data) <- c("País", "Casos acumulados", "Nuevos casos en 24h", "Porcentaje de casos", "Fallecidos acumulados", "Nuevos fallecidos en 24h", "Porcentaje de fallecidos", "IA 14 días")
    data <- na.omit(data)
    datatable(data, rownames = FALSE,  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
  })
  
  output$datatablespain <- renderDT({
    data <- data.spain
    data_vac <- data.spain.vac %>% dplyr::filter(fecha == max(unique(data.spain.vac$fecha))) %>% dplyr::select(Comunidad, percvacunados)
    data_uci <- data.uci.spain %>% dplyr::filter(fecha == max(unique(data.uci.spain$fecha))) %>% dplyr::select(CCAA, PercCamasCovid)
    data <- data %>% dplyr::filter(Fecha == max(unique(data$Fecha)))
    data <- data %>% dplyr::select(-c(Fecha, number_file)) %>% dplyr::arrange(desc(Casos))
    data <- left_join(data, data_vac, by = c("Comunidad"))
    data <- left_join(data, data_uci, by = c("Comunidad" = "CCAA"))
    data <- data %>% dplyr::select(Comunidad, Casos, CasosDiarios, Fallecidos, FallecidosDiarios, IA14, percvacunados, PercCamasCovid)
    names(data) <- c("Comunidad", "Casos", "Nuevos casos en 24h", "Fallecidos", "Nuevos fallecidos en 24h", "IA 14 días", "Porcentaje de vacunados", "Porcentaje de ocupación en UCI por COVID-19")
    datatable(data, rownames = FALSE,  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
  })


  
}
prediction_spain_page_server <- function(input, output, session) {}

router <- make_router(
  route("world", world_page),
  route("spain", spain_page, spain_page_server),
  route("prediction_spain", prediction_spain_page, prediction_spain_page_server)
)

menu_button <- function(link = "world", ...) {
  a(class="button-guay",
    href=route_link(link),
    span(class="button-text", ...)
  )
}

ui <- fluidPage(
  tags$script(inactivity),
  shinyjs::useShinyjs(),
  title = "Dashboard COVID-19",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"),
    HTML('<script src="https://kit.fontawesome.com/260fafe623.js" crossorigin="anonymous"></script>')
  ),
  div(class="big-container",
      div(id="dashboard-header",
          div(class="mini-header",
              menu_button(link="world", "Mundial"),
              menu_button(link="spain", "España"),
              menu_button(link="prediction_spain", "Predicción en España")
          )
      ),
      router$ui
  )
)
server <- function(input, output, session) {
  router$server(input, output, session)
}
shinyApp(ui, server)
