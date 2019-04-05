#
library(shiny)
library(tidyverse)

# definir que campos se guardan en los archivos que van al directorio
fieldsAll <- c("nombre", "fecha_naci", "favourite_pkg", "used_shiny", "r_num_years", "os_type")

# definir que campos son mandatorios para registrar
fieldsMandatory <- c("nombre", "favourite_pkg")

# agregar asterisco rojo a los campos mandatorios
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#rangos de fechas para filtro de descargas del administrador
timezone_pty <- c("EST")
min_date <- c(as.Date("1930-01-01"))
max_date <- c(Sys.Date())

# colocar tiempo actual para cada registro
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# formatear el tiempo de la respuesta a un formato mas legible
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# guardar las respuestas a un file .csv
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# cargar todas las respuestas previas en un dataframe. Combina todos los archivos .csv que encuentre en el directorio
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# nombre del folder directorio donde se almacenaran las respuestas. Debe ser creado localmente. En la terminal de Ubuntu por ejemplo
responsesDir <- file.path("responses")

# CSS del app estetica
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
.nav-tabs>li.active>a, .nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus { border-top: 2px solid orange; border-bottom: 0; font-weight: bold; }
.nav-tabs { font-size: 1.4em; }
.nav-tabs>li { margin-left: 7px; }
.nav-tabs>li>a { background-color: #f8f8f8; border: 1px solid #dddddd; padding: 10px 30px; margin-left: 5px; }
.nav>li>a:hover, .nav>li>a:focus { background-color: #dddddd; }
.tab-content { padding-top: 15px; }
"

# nombres de usuario que son administradores, si se corriera el app en servidor Pro
adminUsers <- c("admin", "prof")

# informacion para compartir este app
share <- list(
  title = "Trust Forms",
  url = "https://www.datanautas.com",
  image = "https://www.datanautas.com/wp-content/uploads/2019/02/icon_datanautas-e1549591621239.png",
  description = "Un shiny app tipo formulario que almacena informacion",
  twitter_user = "Czor_Salad"
)

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Trust Forms - Datanautas",
    tags$head(
      tags$link(rel = "shortcut icon", type="image/x-icon", href="https://www.datanautas.com/wp-content/uploads/2019/02/icon_datanautas-e1549591621239.png"),
      tags$link(href = "app.css", rel = "stylesheet"),
      
      # Facebook
      tags$meta(property = "og:title", content = share$title),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = share$url),
      tags$meta(property = "og:image", content = share$image),
      tags$meta(property = "og:description", content = share$description),
      
      # Twitter
      tags$meta(name = "twitter:card", content = "summary"),
      tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
      tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
      tags$meta(name = "twitter:title", content = share$title),
      tags$meta(name = "twitter:description", content = share$description),
      tags$meta(name = "twitter:image", content = share$image)
    ),
    tags$a(
      href="https://github.com/CzorSalad/shiny-server/tree/master/",
      tags$img(style="position: absolute; top: 0; right: 0; border: 0;",
               src="github-green-right.png",
               alt="Fork me on GitHub")
    ),
    div(id = "header",
        h1("Trust-Forms"),
        h4("App Demo desarrollado por ",
           a(href = "https://www.datanautas.com",
             "Datanautas")
        ),
        strong( 
          span("Creado por "),
          a("Datanautas", href = "https://www.datanautas.com"),
          HTML("&bull;"),
          span("Codigo disponible"),
          a("en GitHub", href = "https://github.com/CzorSalad/shiny-server/tree/master/trust-forms"),
          HTML("&bull;"),
          a("Otros apps", href = "https://www.datanautas.com"), "por Datanautas"),
        br(),br()
    ),
    
    fluidRow(
      column(12,
            tabsetPanel(id = "mainNav",
             tabPanel("Registrar",
                      value = "registrar",
             
             div(
               id = "form",
               h4("Formulario de Registro"),
               textInput("nombre", labelMandatory("Nombre del cliente"), ""),
               textInput("favourite_pkg", labelMandatory("# de Cedula")),
               dateInput("fecha_naci", labelMandatory("Fecha de Nacimiento"),
                         value = "1990-01-01",
                         min = "1935-01-01",
                         format = "dd-mm-yyyy",
                         startview = "decade"),
               checkboxInput("used_shiny", "Es una Persona Politicamente Expuesta (PEP)?", FALSE),
               sliderInput("r_num_years", "Duracion (a) del financiamiento:", 0, 12, 1, ticks = FALSE),
               selectInput("os_type", "Estado Civil",
                           c("",  "Soltero", "Casado", "No especificado")),
               passwordInput("password_agente","ID de Agente:", placeholder = "Coloque su ID para poder registrar"),
               
               actionButton("submit", "Registrar", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Registrando..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("El financiamiento ha sido registrado con exito!"),
                 actionLink("submit_another", "Registrar otro financiamiento")
               )
             )
          ),
      tabPanel("Financiamientos",
               value = "financiamientos",
               
                column(6,
                       div(
                         id = "adminvalidation",
                         h4("ID de Administrador"),
                         passwordInput("password","Contrasena:", placeholder = "Coloque su ID para accesar")
                       ),

                       uiOutput("adminPanelContainer")
                )
      )
      )
    )
  )
  ),
  server = function(input, output, session) {
    
    # Habilitar el boton de Registrar cuando se llenan todos los campos mandatorios
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(c(all(mandatoryFilled), input$password_agente == password_list_agente))
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
   
    # Administracion de contrasenas para habilitar funciones como descargar y ver tablas
    # Lista de contrasenas de admins
    password_list_admin <- c("lenovo179447")
    password_list_agente <- c("RP-ALB-7919")
    
    # Habilitar descargar si el password es correcto 
    observe({
      valid_password_admin <- input$password %in% password_list_admin
      
      shinyjs::toggleState(id = "downloadBtn", condition = valid_password_admin)
    })
    
    
    # Combinar todos los inputs y agregar tiempo
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })    
    
    # Cuando se clickea el boton de Registrar, registrar la respuesta
    observeEvent(input$submit, {
      
      # Experiencia de usuario
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Guardar la data (ensenar mensaje de error si hay alguno)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # Registrar otra respuesta
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    # Panel de Administrador
    output$adminPanelContainer <- renderUI({
      if (!isAdmin()) return()
      
      div(
        conditionalPanel("input.password == 'lenovo179447' | input.password == 'artec7919'", 
        id = "adminPanel",
        h2("Lista de financiamientos registrados"),
        downloadButton("downloadBtn", "Descargar registros"),
        br(),
        dateRangeInput(inputId = "rango_fechas",
                       label = "Ver financimientos desde:",
                       min = min_date,
                       max = max_date,
                       startview = "decade",
                       start = "1990-01-01",
                       end = max_date,
                       language = "es",
                       separator = " - "),
        DT::dataTableOutput("responsesTable"), br(),
        "* Se debe considerar si borrar los registros al ser descargados o si permitir seleccionar rango de descarga."
        )
      )
    })
    
    # determinar si el usuario es administrador
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })    
    
    # ensenar respuestas en la tabla de administrador
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      data$fecha_naci <- as.Date(data$fecha_naci, origin="1970-01-01")
      data <- data %>%
        filter(fecha_naci >= as.Date(input$rango_fechas[1], origin = "1970-01-01") &
                 fecha_naci <= as.Date(input$rango_fechas[2], origin = "1970-01-01"))
      DT::datatable(
        data,
        colnames = c("Nombre", "Fecha de Nacimiento", "Cedula", "PEP?", "Anos de Finan.", "Estado civil", "Timestamp"),
        rownames = FALSE,
        options = list(searching = TRUE, lengthChange = FALSE)
      )
    })
    
    # Permitir a usuarios descargar los registros
    output$downloadBtn <- downloadHandler(
      filename = function() {
        sprintf("trust_forms_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )    
  }
)
