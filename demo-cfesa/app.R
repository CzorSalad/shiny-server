#
library(shiny)
library(tidyverse)

# definir que campos se guardan en los archivos que van al directorio
fieldsAll <- c("password_agente", "auto_valor", "auto_anos_uso", "finan_cuotas", "finan_calculo",
               "auto_marca", "auto_modelo", "auto_estado", "auto_seguro", "auto_mantenimiento",
               "finan_pct_abono", "finan_abono", "finan_cashback", "finan_refinan", "finan_desc_dir",
               "finan_tipo", "finan_credito_tipo", "finan_inversion_tipo", "finan_frecuencia", "cliente_nombre", 
               "apellido_paterno", "cliente_sexo", "cliente_fecha_naci", "cliente_edad", "cliente_estado_civil",
               "cliente_educacion", "cliente_dependientes", "cliente_altura", "cliente_peso", "cliente_garante",
               "cliente_nombre2", "apellido_materno", "identificacion_tipo", "identificacion_numero", "cliente_auto",
               "cliente_licencia", "cliente_vivienda", "cliente_salario", "cliente_presion_min", "cliente_presion_max",
               "cliente_mancomunado", "direccion_provincia", "direccion_distrito", "direccion_corregimiento",
               "direccion_barrio", "direccion_zona", "direccion_calle", "direccion_casa", "direccion_tel",
               "direccion_tel_trab", "direccion_cel", "direccion_correo", "laboral_empresa_tipo", "laboral_lugar",
               "laboral_fecha_ingreso", "laboral_ant_lugar", "laboral_ant_fecha_ingreso", "laboral_antiguedad",
               "laboral_ocupacion", "laboral_ingresos", "laboral_ant_fecha_egreso", "laboral_ant_antiguedad",
               "laboral_ingresos_otros", "laboral_ingresos_tipo", "laboral_ingresos_otros_tipo", "laboral_ant_periodo",
               "laboral_profesion", "laboral_situacion", "laboral_actividad", "conyuge_identificacion_tipo",
               "conyugue_nombre", "conyugue_apellido_paterno", "conyugue_identificacion", "conyugue_sexo",
               "conyugue_nombre2", "conyugue_apellido_materno", "conyugue_estado_civil", "conyugue_fecha_naci",
               "conyugue_edad", "conyugue_profesion", "conyugue_trabajo", "conyugue_ingreso", "conyugue_antiguedad",
               "conyugue_ingresos", "conyugue_ingresos_otros")

# definir que campos son mandatorios para registrar NO INCLUIR CAMPOS FECHA 
fieldsMandatory <- c("cliente_nombre", "apellido_paterno")

marcas_autos <- sort(c("", "Toyota", "HINO", "Lexus"))

# Definir modelos de autos
modelos_autos <- sort(c("", "Agya", "Yaris", "Rush", "Corolla",
                        "Rav4", "Fortuner", "Prado", "Landcruiser",
                        "Camion1", "Camion2", "Camion3",
                        "Xtrail", "Pathfinder", "Qashqai"))

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
responsesDir <- file.path("databaseCFESA")


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
    title = "Trust Forms - Datanautas",
    tags$head(
      tags$link(rel = "shortcut icon", type="image/x-icon", href="https://www.datanautas.com/wp-content/uploads/2019/02/icon_datanautas-e1549591621239.png"),
      
      # CSS para estetica del app
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
            tabsetPanel(id = "mainNav",
             tabPanel("Registrar",
                      value = "registrar",
             column(12,
                    div(id = "authentication",
                      style="display: inline-block;vertical-align: top;",
                      passwordInput("password_agente","ID de Agente:", placeholder = "Colocar ID para poder registrar"),
                      
                      shinyjs::hidden(
                        span(id = "submit_msg", "Registrando..."),
                        div(id = "error",
                            div(br(), tags$b("Error: "), span(id = "error_msg"))
                        )
                      )
                    ),
                    div(id = "boton_registrar",
                        style="display: inline-block;vertical-align: top;padding: 10px",
                        br(),br(),
                        actionButton("submit", "Registrar", class = "btn-primary")), 
                    br(),
                  column(6,
                    div(id = "titulo_form1", h3("Datos del Auto & Financiamiento")),
                    column(6,
                    # Form parte 1.1
                      div(
                        id = "form1",
                        numericInput("auto_valor", labelMandatory("Valor del Auto"), value = 20000, step = 10000, min = 0),
                        numericInput("auto_anos_uso", labelMandatory("Años de uso"), value = 0, step = 1, min = 0),
                        numericInput("finan_cuotas", labelMandatory("Número de cuotas"), value = 60, step = 12, min = 0),
                        radioButtons("finan_calculo", "Cálculo", 
                                   c("Porcentaje", "Opción 2", "Opción 3"), 
                                   selected = "Porcentaje", 
                                   inline = TRUE),
                        selectInput("auto_marca", "Marca del auto", marcas_autos),
                        selectInput("auto_modelo", "Modelo del auto", modelos_autos),
                        radioButtons("auto_estado", "Estado del auto",
                           c("Nuevo", "Usado"), inline = TRUE),
                        checkboxInput("auto_seguro", "Seguro de auto?", FALSE),
                        checkboxInput("auto_mantenimiento", "Incluir mantenimiento?", FALSE),
                        br(), br(), br(), br()
                      )
                    ),
              # Form parte 1.2
                    column(5,
                      div(
                        id = "form2",
                        numericInput("finan_pct_abono", labelMandatory("% de Abono"), value = 20, step = 5, min = 0),
                        numericInput("finan_abono", labelMandatory("$ Abono"), value = 0, step = 1000, min = 0),
                        numericInput("finan_cashback", labelMandatory("Cashback"), value = 0, step = 1000, min = 0),
                        checkboxInput("finan_refinan", "Es refinanciado?", FALSE),
                        checkboxInput("finan_desc_dir", "Descuento directo?", FALSE),
                        selectInput("finan_tipo", "Tipo de financiación", c("", "Crédito", "Opción 2", "Opción 3")),
                        selectInput("finan_credito_tipo", "Tipo de crédito", c("", "Fideicomiso", "Opción 2",
                                                                               "Opcion 3")),
                        selectInput("finan_inversion_tipo", "Tipo de inversión", c("", "Comercial", "Opción 2",
                                                                                   "Opción 3")),
                        radioButtons("finan_frecuencia", "Frecuencia", c("Quincenal", "Mensual", "Trimestral"),
                                     selected = "Quincenal", inline = TRUE)
                )
              )
             ),
             
             #div de prueba
             column(6,
                    div(id = "titulo_form2", h3("Datos del Cliente")),
                    column(6,
                           # Form parte 2.1
                           div(
                             id = "form3",
                             textInput("cliente_nombre", labelMandatory("Primer nombre"), ""),
                             textInput("apellido_paterno", labelMandatory("Apellido paterno"), ""),
                             radioButtons("cliente_sexo", labelMandatory("Sexo"), c("Masculino", "Femenino"),
                                          inline = TRUE, selected = character(0)),
                             dateInput("cliente_fecha_naci", labelMandatory("Fecha de Nacimiento"),
                                       value = "1980-01-01",
                                       min = min_date,
                                       startview = "decade",
                                       format = "dd-mm-yyyy"),
                             numericInput("cliente_edad", "Edad", value = 20 , step = 1, min = 18),
                             selectInput("cliente_estado_civil", "Estado Civil",
                                         c("",  "Soltero", "Casado", "No especificado")),
                             selectInput("cliente_educacion", "Nivel de educación", c("","Ninguno", "Primaria", "Secundaria", "Universitaria")),
                             numericInput("cliente_dependientes", "Número de dependientes", value = 0 , step = 1, min = 0),
                             numericInput("cliente_altura", "Altura (cm)", value = 160, step = 10, min = 0),
                             numericInput("cliente_peso", "Peso (lbs)", value = 100, step = 10, min = 0),
                             checkboxInput("cliente_garante", "Tiene garante?", FALSE),
                             br(), br(), br(), br()
                           )
                    ),
                    # Form parte 2.2
                    column(6,
                           div(
                             id = "form4",
                             textInput("cliente_nombre2", "Segundo nombre", ""),
                             textInput("apellido_materno", labelMandatory("Apellido materno"), ""),
                             radioButtons("identificacion_tipo", labelMandatory("Tipo de identificación"),
                                          c("Cédula", "Pasaporte", "Opción 3"), inline = TRUE),
                             textInput("identificacion_numero", labelMandatory("No. serie de identificacion"), ""),
                             checkboxInput("cliente_auto", "Tiene auto?", FALSE),
                             selectInput("cliente_licencia", "Tipo de licencia", c("", "Particular", "Comercial", "Otro")),
                             selectInput("cliente_vivienda", "Tipo de vivienda",
                                         c("",  "Propietario", "Arrendamiento", "Opción 3")),
                             numericInput("cliente_salario", "Valor/Pago mensual", value = 1000 , step = 500, min = 500),
                             numericInput("cliente_presion_min", "Presión mínima", value = 80, step = 10, min = 0),
                             numericInput("cliente_presion_max", "Presión máxima", value = 120, step = 10, min = 0),
                             checkboxInput("cliente_mancomunado", "Mancomunado?", FALSE)
                           )
                    )
             ),
            
            # Formulario de dirección
             div(style="padding-left: 10px;",
                id = "form6",
                h3("Dirección"),
                div(style="display: inline-block;vertical-align: top;padding-left: 25px;",
                    selectInput("direccion_provincia", "Provincia", sort(c("", "Panamá", "Panamá Oeste", "Chiriquí",
                                                                      "Los Santos", "Coclé", "Colón", "Veraguas", "Daríen",
                                                                      "Guna Yala", "Herrera", "Bocas del Toro"))),
                    selectInput("direccion_distrito", "Distrito",
                                c("",  "Opción 1", "Opción 2", "Opción 3")),
                    textInput("direccion_corregimiento", "Corregimiento", "")
                    ),
                div(style="display: inline-block;vertical-align: top;padding-left: 75px;",
                    textInput("direccion_barrio", "Barrio", ""),
                    textInput("direccion_zona", "Zona", ""),
                    textInput("direccion_calle", "Calle/Etapa/Sector", "")
                    ),
                div(style="display: inline-block;vertical-align: top;padding-left: 75px;",
                    textInput("direccion_casa", "Casa/Edificio", ""),
                    textInput("direccion_tel", "Teléfono residencial", "")
                    ),
                div(style="display: inline-block;vertical-align: top;padding-left: 75px;",
                    textInput("direccion_tel_trab", "Teléfono trabajo",""),
                    textInput("direccion_cel", "Celular", ""),
                    textInput("direccion_correo", "Correo electrónico", "")
                    )
                ),
            
            # Formulario de información de ingresos del cliente
            div(
                id = "form7",
                h3("Información Laboral y de Ingresos"),
                div(style="display: inline-block;vertical-align: top;padding-left: 25px;",
                    textInput("laboral_empresa_tipo", "Empresa", ""),
                    textInput("laboral_lugar", "Lugar de trabajo", ""),
                    dateInput("laboral_fecha_ingreso", "Fecha de ingreso",
                              startview = "year",
                              language = "es",
                              format = "dd-mm-yyyy"),
                    br(),
                    h4("Datos de Trabajo Anterior"),
                    textInput("laboral_ant_lugar", "Lugar de trabajo anterior", ""),
                    dateInput("laboral_ant_fecha_ingreso", "Fecha de ingreso",
                              startview = "year",
                              language = "es",
                              format = "dd-mm-yyyy")
                ),
                
                div(style="display: inline-block;vertical-align: top;padding-left: 75px;",
                    numericInput("laboral_antiguedad", "Antiguedad laboral (meses)", min = 0, value = 0,  step = 1),
                    textInput("laboral_ocupacion", "Ocupacion", ""),
                    numericInput("laboral_ingresos", "Ingresos (anual)", min = 0, value = 10000, step = 500),
                    br(),
                    br(),
                    dateInput("laboral_ant_fecha_egreso", "Fecha de egreso",
                              startview = "year",
                              language = "es",
                              format = "dd-mm-yyyy"),
                    numericInput("laboral_ant_antiguedad", "Antiguedad (meses):", min = 0, value = 0, step = 1)
                ),
                
                div(style="display: inline-block;vertical-align: top;padding-left: 75px;",
                    numericInput("laboral_ingresos_otros", "Otros ingresos (anual)", min = 0, value = 0, step = 500),
                    selectInput("laboral_ingresos_tipo", "Tipo de ingresos", c("", "Estados Financieros", "Opción 2",
                                                                               "Opción 3")),
                    selectInput("laboral_ingresos_otros_tipo", "Tipo de otros ingresos", c("", "Estados Financieros", "Opción 2",
                                                                               "Opción 3")),
                    br(),
                    br(),
                    numericInput("laboral_ant_periodo", "Perido entre anterior y actual (meses)", min = 0, value = 0, step = 1)
                ),
                
                div(style="display: inline-block;vertical-align: top;padding-left: 75px;",
                    textInput("laboral_profesion", "Profesión", ""),
                    selectInput("laboral_situacion", "Situación laboral", c("", "Independiente no profesional", "Opción 2",
                                                                                           "Opción 3")),
                    selectInput("laboral_actividad", "Actividad", c("", "Personal - Particular", "Opción 2",
                                                                                           "Opción 3"))
                )
            ),
            
            # Formulario para llenar en caso de estado civil es casado
            div(
              conditionalPanel("input.cliente_estado_civil == 'Casado'", 
                               id = "form5",
                               h3("Información del Conyugue"),
                               column(3,
                                      radioButtons("conyuge_identificacion_tipo", labelMandatory("Tipo de identificación"),
                                                   c("Cédula", "Pasaporte", "Opción 3"), inline = TRUE),
                                      textInput("conyugue_nombre", "Primer nombre", ""),
                                      textInput("conyugue_apellido_paterno", "Apellido paterno", ""),
                                      textInput("conyugue_identificacion", "No. serie de identificación", ""),
                                      br(),br(),br()
                               ),
                               
                               column(3,
                                      radioButtons("conyugue_sexo", labelMandatory("Sexo"), c("Masculino", "Femenino"),
                                                   inline = TRUE, selected = character(0)),
                                      textInput("conyugue_nombre2", "Segundo nombre", ""),
                                      textInput("conyugue_apellido_materno", "Apellido materno", ""),
                                      selectInput("conyugue_estado_civil", "Estado Civil",
                                                  c("",  "Soltero", "Casado", "No especificado"))
                               ),
                               
                               column(3,
                                      dateInput("conyugue_fecha_naci", "Fecha de nacimiento",
                                                startview = "decade",
                                                language = "es",
                                                value = "1980-01-01",
                                                format = "dd-mm-yyyy",
                                                min = min_date,
                                                max = max_date),
                                      numericInput("conyugue_edad", "Edad", min = 0, value = 0, step = 1),
                                      textInput("conyugue_profesion", "Profesión", ""),
                                      textInput("conyugue_trabajo", "Lugar de trabajo", "")
                               ),
                               
                              column(3,
                                     dateInput("conyugue_ingreso", "Fecha de ingreso",
                                              startview = "decade",
                                              language = "es",
                                              value = max_date,
                                              format = "dd-mm-yyyy",
                                              min = min_date,
                                              max = max_date),
                                     numericInput("conyugue_antiguedad", "Antiguedad laboral (meses)", value = 0, min = 0, step = 1),
                                     numericInput("conyugue_ingresos", "Ingresos", value = 1000, min = 0, step = 500),
                                     numericInput("conyugue_ingresos_otros", "otros ingresos", value = 0, min = 0, step = 500)
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
            )
          ),
      tabPanel("Financiamientos",
               value = "financiamientos",
               
                column(12,
                       div(
                         id = "adminvalidation",
                         h4("ID de Administrador"),
                         passwordInput("password","Contrasena:", placeholder = "Coloque su ID para accesar")
                       ),

                       uiOutput("adminPanelContainer")
                )
      ),
      tabPanel("Seguimiento",
               value = "seguimiento"),
      
      tabPanel("Documentacion",
               value = "documentacion"),
      
      tabPanel("Dashboard",
               value = "dashboard"),
      
      tabPanel("Ayuda",
               value = "ayuda")
    # Parentesis del cierre de Panel de Tabs
    )
  # Parentesis del cierre de Fluid Page
    )
 # Parentesis del cierre de Shiny App
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
      mandatoryFilled <- all(c(all(mandatoryFilled), input$password_agente %in% password_list_agente))
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
   
    # Administracion de contrasenas para habilitar funciones como descargar y ver tablas
    # Lista de contrasenas de admins
    password_list_admin <- c("lenovo179447")
    password_list_agente <- c("RP-ALB-7919", "RP-PDE-7919")
    
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
      
      # Guardar la data (ensenar mensaje de error si hay alguno) y resetea y esconde los formularios
      tryCatch({
        saveData(formData())
        shinyjs::reset("form1")
        shinyjs::reset("form2")
        shinyjs::reset("form3")
        shinyjs::reset("form4")
        shinyjs::reset("form5")
        shinyjs::reset("form6")
        shinyjs::reset("form7")
        shinyjs::hide("titulo_form1")
        shinyjs::hide("titulo_form2")
        shinyjs::hide("form1")
        shinyjs::hide("form2")
        shinyjs::hide("form3")
        shinyjs::hide("form4")
        shinyjs::hide("form5")
        shinyjs::hide("form6")
        shinyjs::hide("form7")
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
      shinyjs::show("titulo_form1")
      shinyjs::show("titulo_form2")
      shinyjs::show("form1")
      shinyjs::show("form2")
      shinyjs::show("form3")
      shinyjs::show("form4")
      shinyjs::show("form5")
      shinyjs::show("form6")
      shinyjs::show("form7")
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
        dateInput(inputId = "rango_fechas",
                       label = "Ver financimientos desde:",
                       min = min_date,
                       max = max_date,
                       startview = "year",
                       value = "2019-01-01",
                       language = "es"),
        
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
      data$cliente_fecha_naci <- as.Date(data$cliente_fecha_naci, origin="1970-01-01")
      data <- data %>%
        filter(timestamp >= as.POSIXct(input$rango_fechas, origin = "1970-01-01"))
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = TRUE, 
                       lengthChange = FALSE,
                       scrollX = TRUE)
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