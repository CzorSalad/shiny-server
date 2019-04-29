
## - DEPENDENCIAS (LOAD)
####################################
library(shiny)
library(tidyverse)
library(shinyalert)
library(mongolite)
#####################

## - GENERALES DEL FORMULARIO (campos)
#####################################

# Definir variables/campos del formulario que seran guardadas
fieldsAll <- c("auto_valor", "auto_anos_uso", "finan_cuotas", "finan_calculo",
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



# Variables de la seccion de MODIFICAR REGISTROS
# Definir variables/campos de texto para la seccion de modificar registros
texto_select_fields <- c("Modelo del auto" = "auto_modelo", "Calculo" = "finan_calculo",
                         "Marca del auto" = "auto_marca", "Estado del auto" = "auto_estado",
                         "Tipo de financiacion"  = "finan_tipo", "Tipo de credito" = "finan_credito_tipo",
                         "Tipo de inversion" = "finan_inversion_tipo", "Frecuencia" = "finan_frecuencia",
                         "Primer nombre" = "cliente_nombre", "Segundo nombre" = "cliente_nombre2",
                         "Apellido paterno" = "apellido_paterno", "Apellido materno" = "apellido_materno",
                         "Sexo" = "cliente_sexo", "Identificacion tipo" = "identificacion_tipo",
                         "Identificacion No. Serie" = "identificacion_numero", "Estado civil" = "cliente_estado_civil",
                         "Tipo de licencia" = "cliente_licencia", "Tipo de vivienda" = "cliente_vivienda",
                         "Nivel de educacion" = "cliente_educacion", "Provincia"  = "direccion_provincia",
                         "Distrito" = "direccion_distrito", "Corregimiento"  = "direccion_corregimiento",
                         "Barrio" = "direccion_barrio", "Zona" = "direccion_zona",
                         "Calle/Etapa/Sector" = "direccion_calle","Casa/Edificio" = "direccion_casa",
                         "Telefono residencial" = "direccion_tel", "Telefono trabajo" = "direccion_tel_trab",
                         "Celular" = "direccion_cel", "Correo electronico" = "direccion_correo",
                         "Empresa" = "laboral_empresa_tipo", "Lugar de trabajo" = "laboral_lugar",
                         "Lugar de trabajo anterior" = "laboral_ant_lugar",
                         "Ocupacion" = "laboral_ocupacion", "Tipo de ingresos" = "laboral_ingresos_tipo",
                         "Tipo de ingresos (otros)" = "laboral_ingresos_otros_tipo", "Profesion" = "laboral_profesion",
                         "Situacion laboral" = "laboral_situacion", "Actividad laboral" = "laboral_actividad",
                         "Conyugue tipo de ID" = "conyuge_identificacion_tipo", "Conyugue primer nombre" = "conyugue_nombre",
                         "Conyugue apellido paterno" = "conyugue_apellido_paterno", "Conyugue No. de ID" = "conyugue_identificacion",
                         "Conyugue sexo" = "conyugue_sexo", "Conyugue segundo nombre" = "conyugue_nombre2",
                         "Conyugue apellido materno" = "conyugue_apellido_materno", " Conyugue estado civil" = "conyugue_estado_civil",
                         "Conyugue profesion" = "conyugue_profesion", "Conyugue lugar de trabajo" = "conyugue_trabajo")

# Definir variables/campos numericos para la seccion de modificar registros
numeric_select_fields <- c("Valor del auto" = "auto_valor", "Auto anos de uso" = "auto_anos_uso",
                           "Numero de cuotas" = "finan_cuotas",
                           "Abono (porcentaje)" = "finan_pct_abono", "Abono (monto)" = "finan_abono",
                           "Cashback (monto)" = "finan_cashback", "Cliente edad" = "cliente_edad",
                           "No de dependientes" = "cliente_dependientes", "Altura"="cliente_altura",
                           "Peso" = "cliente_peso","Vivienda pago (monto)" = "cliente_salario",
                           "Presion minima" = "cliente_presion_min", "Presion maxima" = "cliente_presion_max",
                           "Cliente antiguedad laboral" = "laboral_antiguedad", "Cliente ingresos" = "laboral_ingresos", 
                           "Antiguedad trabajo anterior" = "laboral_ant_antiguedad", "Cliente otros ingresos" = "laboral_ingresos_otros",
                           "Periodo entre trabajos" = "laboral_ant_periodo", "Conyugue edad" = "conyugue_edad",
                           "Conyugue antiguedad laboral" = "conyugue_antiguedad", "Conyugue ingresos" = "conyugue_ingresos", 
                           "Conyugue otros ingresos" = "conyugue_ingresos_otros")

# Definir variables/campos fecha
fecha_select_fields <- c("Cliente fecha de nacimiento" = "cliente_fecha_naci", "Cliente ingreso trabajo" = "laboral_fecha_ingreso",
                         "Cliente ingreso trabajo anterior" = "laboral_ant_fecha_ingreso", "Cliente egreso trabajo" = "laboral_ant_fecha_egreso",
                         "Conyugue fecha de nacimiento" = "conyugue_fecha_naci", "Conyugue ingreso trabajo" = "conyugue_ingreso")

# Definir variables/campos logicos (CheckBox)
logical_select_fields <- c("Seguro de auto" = "auto_seguro", "Incluir mantenimiento" = "auto_mantenimiento",
                           "Refinanciado" = "finan_refinan", "Descuento directo" = "finan_desc_dir",
                           "Tiene garante" = "cliente_garante", "Tiene auto" = "cliente_auto",
                           "Es mancomunado" = "cliente_mancomunado")

cotizar_fields <- c("cotizar_nombre", "cotizar_nper", "cotizar_monto_total", "cotizar_abono", "cotizar_modelo", "cotizar_tipo_finan")
#####################


## - VARIABLES PARA FUNCIONES
#####################################

## MongoDB ##
options(mongodb = list(
  "host" = "datanautas0",
  "username" = "datanautas",
  "password" = "dunlopdataman69420"
))
databaseName <- "trust_forms_testing"
collectionName <- "financiamientos"
## ##

# Campos para buscar cliente y load (mongo)
searchID_fields <- c("search_identif", "search_transnum")

# Campos para buscar cliente y load (mongo)
searchID_fields_mongo <- c("identificacion_numero", "IDcounter")

# campos del buscador de registros (mongo)
serialID_fields <- c("identificacion_numero")

# Definir que campos son mandatorios para registrar NO INCLUIR CAMPOS FECHA 
fieldsMandatory <- c("cliente_nombre", "apellido_paterno", "identificacion_numero")

# Marcas de carros
marcas_autos <- sort(c("", "Toyota", "HINO", "Lexus"))

# Definir modelos de autos
modelos_autos <- sort(c("", "Agya", "Yaris", "Rush", "Corolla",
                        "Rav4", "Fortuner", "Prado", "Landcruiser",
                        "Camion1", "Camion2", "Camion3",
                        "NX300h", "Hilux", "Tundra"))

# Rangos de fechas para filtro de descargas del administrador
timezone_pty <- c("EST")
min_date <- c(as.Date("1930-01-01"))
max_date <- c(Sys.Date())

# Nombre del folder directorio donde se almacenaran las respuestas. Debe ser creado localmente. En la terminal de Ubuntu por ejemplo
responsesDir <- file.path("databaseCFESA")

# Nombres de usuario que son administradores, si se corriera el app en servidor Pro
adminUsers <- c("admin", "prof")
######################


## - FUNCIONES
#####################################
# agregar asterisco rojo a los campos mandatorios
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Printear valores en porcentaje
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
# Printear valores en dolares
dolares <- function(x, digits = 2, format = "f", ...) {
  paste0("$ ",formatC(x, format = format, digits = digits, big.mark = ",", ...))
}

# Colocar tiempo actual "timestamp" para cada registro
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# Formatear el tiempo de la respuesta a un formato mas legible
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Contador de repeticiones del ID
counterID <- function(input) {
  database <- loadDataMongo()
  id_numbers <- database$identificacion_numero
  id_numbers <- id_numbers[!is.na(id_numbers)]
  repeticiones <- sum(id_numbers == input) + 1
  repeticiones
}

### MONGO DB MIGRATION FUNCTIONS ###
saveDataMongo <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName, db = databaseName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s-vhd4e.mongodb.net/test?retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host
              )
  )
  
  # Insert the data into the mongo collection as a data.frame
  db$insert(data)
}

loadDataMongo <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName, db = databaseName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s-vhd4e.mongodb.net/test?retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host
              )
  )
  # Read all the entries
  data <- db$find()
  data
}

deleteDataMongo <- function(search_id) {
  # Conectarse a la base de datos
  db <- mongo(collection = collectionName, db = databaseName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s-vhd4e.mongodb.net/test?retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host
              )
  )
  # buscar cliente segun ID input
  data <- db$remove(query = sprintf('{"%s" : "%s", "%s" : "%s"}', searchID_fields_mongo [[1]], search_id[[1]],
                                  searchID_fields_mongo[[2]], search_id[[2]]),
                    just_one = TRUE)
}

editDataMongo <- function(search_id, col_to_edit, x) {
  # Conectarse a la base de datos
  db <- mongo(collection = collectionName, db = databaseName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s-vhd4e.mongodb.net/test?retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host
              )
  )
  # buscar cliente segun ID input
  db$update(query = sprintf('{"%s" : "%s", "%s" : "%s"}', searchID_fields_mongo [[1]], search_id[[1]],
                                             searchID_fields_mongo[[2]], search_id[[2]]),
            update = sprintf('{"$set":{"%s" : "%s"}}', col_to_edit, x)
  )
}

# Search en MongoDB - Funcion para loadear un file especifico usar funcion searchDataMongo(searchClientMongo())
searchDataMongo <- function(search_id) {
  # Conectarse a la base de datos
  db <- mongo(collection = collectionName, db = databaseName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s-vhd4e.mongodb.net/test?retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host
              )
  )
  # buscar cliente segun ID input
  data <- db$find(query = sprintf('{"%s" : "%s", "%s" : "%s"}', searchID_fields_mongo [[1]], search_id[[1]],
                                  searchID_fields_mongo[[2]], search_id[[2]]),
                  limit = 1)
  data
}


# Asociar IDs con usuarios NO ESTA EN USO - CONSTRUCCION
usernames_valid <- function() {
  names(usernames_ID) <- NULL
  names(usernames_ID) <- password_list_agente
  usernames_ID
}

# Loadear parametros de los bancos NO ESTA EN USO - CONSTRUCCION
bancos_params_load <- function(){
  paramsDir <- c("parametrosBancos")
  files <- list.files(file.path(paramsDir), full.names = TRUE)
  params_bancos <- read_csv(files)
  params_bancos
}
#######################

## - FUNCIONES PARA COTIZAR SEGUN BANCO
#####################################
##
#### Formato para tabla ####

cotizacion_format <- function(resultado){
  cotizacion <- as.data.frame(t(as.data.frame(resultado)))
  result <- cotizacion %>%
    mutate(
      "Tasa" = percent(V1),
      "Plazo" = round(V2,0),
      "Monto a financiar" = dolares(V3),
      "Letra mensual" = dolares(V4),
      "Letra quincenal" = dolares(V5)
    )%>%
    select(6:10)
  result
}

#### GLOBALBANK ####
# utilizar de la siguiente manera cotizacion_format(prestamoGlobalBank(reactiveCotizar()))
prestamoGlobalBank <- function(user_inputs){
  cotizar_info <- user_inputs
  pct_abonado <- as.double(cotizar_info[[4]])/as.double(cotizar_info[[3]])
  tasa <- globalTasa(cotizar_info, pct_abonado)
  nperiodos <- globalNper(cotizar_info, pct_abonado)
  globalResult(tasa, nperiodos, cotizar_info)
}

globalNper <- function(cotizar_info, pct_abonado){
  nper <- as.double(cotizar_info[[2]])
  max_nper <- c(84,84,90)
  if(is.null(nper)) {
    if (pct_abonado >= 0.30){
      nper <- max_nper[[3]]
      nper
    } else if (pct_abonado >= 0.20){
      nper <- max_nper[[2]]
      nper
    } else if (pct_abonado >= 0.10){
      nper <- max_nper[[1]]
      nper
    } else {
      return()
    }
  } else {
    nper
  }
}

globalTasa <- function(cotizar_info, pct_abonado){
  tasas <- c(0.0650, 0.0625, 0.0575)
  if (pct_abonado >= 0.30){
    tasa <- tasas [[3]]
    tasa
  } else if (pct_abonado >= 0.20){
    tasa <- tasas[[2]]
    tasa
  } else if (pct_abonado >= 0.10){
    tasa <- tasas[[1]]
    tasa
  } else {
    return()
  }
}

globalResult <- function(tasa, nperiodos, user_inputs) {
  agya <- c("tasa" = 0.0650,"nper" = 84)
  camioneta <- c("tasa" = 0.0575, "nper" = 90)
  result <- vector("double", 5)
  if(user_inputs[[5]] == "Camioneta" & is.null(user_inputs[[2]])){
    result[[1]] <- camioneta[[1]]
    result[[2]] <- camioneta[[2]]
    result[[3]] <- (as.numeric(user_inputs[[3]]) - as.numeric(user_inputs[[4]]))
    result[[4]] <- (result[[3]] + (result[[3]] * result[[1]]))/result[[2]]
    result[[5]] <- result[[4]]/2
    result
  }
  else if(user_inputs[[5]] == "Camioneta"){
    result[[1]] <- camioneta[[1]]
    result[[2]] <- nperiodos
    result[[3]] <- (as.double(user_inputs[[3]]) - as.double(user_inputs[[4]]))
    result[[4]] <- (result[[3]] + (result[[3]] * result[[1]]))/result[[2]]
    result[[5]] <- result[[4]]/2
    result
  } 
  else if(user_inputs[[5]] == "Sedan"){
    result[[1]] <- tasa
    result[[2]] <- nperiodos
    result[[3]] <- (as.numeric(user_inputs[[3]]) - as.numeric(user_inputs[[4]]))
    result[[4]] <- (result[[3]] + (result[[3]] * result[[1]]))/result[[2]]
    result[[5]] <- result[[4]]/2
    result
  }
  else if(user_inputs[[5]] == "Agya" & is.null(user_inputs[[2]])) {
    result[[1]] <- agya[[1]]
    result[[2]] <- agya[[2]]
    result[[3]] <- (as.numeric(user_inputs[[3]]) - as.numeric(user_inputs[[4]]))
    result[[4]] <- (result[[3]] + (result[[3]] * result[[1]]))/result[[2]]
    result[[5]] <- result[[4]]/2
    result
  } 
  else if(user_inputs[[5]] == "Agya"){
    result[[1]] <- agya[[1]]
    result[[2]] <- nperiodos
    result[[3]] <- (as.numeric(user_inputs[[3]]) - as.numeric(user_inputs[[4]]))
    result[[4]] <- (result[[3]] + (result[[3]] * result[[1]]))/result[[2]]
    result[[5]] <- result[[4]]/2
    result
  }
}


#### BAC ####
prestamoBAC <- function(user_inputs){
  cotizar_info <- user_inputs
  pct_abonado <- as.double(cotizar_info[[4]])/as.double(cotizar_info[[3]])
  tasa <- bacTasa(cotizar_info, pct_abonado)
  nperiodos <- bacNper(cotizar_info)
  bacResult(tasa, nperiodos, cotizar_info)
}

bacTasa <- function(cotizar_info, pct_abonado){
  tasas <- c(0.0680, 0.0650, 0.0625, 0.0600, 0.0575)
  if (pct_abonado >= 0.40){
    tasa <- tasas [[5]]
    tasa
  } else if (pct_abonado >= 0.30){
    tasa <- tasas[[4]]
    tasa
  } else if (pct_abonado >= 0.20){
    tasa <- tasas[[3]]
    tasa
  } else if (pct_abonado >= 0.15){
    tasa <- tasas[[2]]
    tasa
  } else if (pct_abonado >= 0.10){
    tasa <- tasas[[1]]
    tasa
  }
  else {
    return()
  }
}

bacNper <- function(cotizar_info){
  nper <- as.double(cotizar_info[[2]])
  max_nper <- c(84)
  if(is.null(nper) | nper > 84) {
      nper <- max_nper[[1]]
     
  } else {
    nper
  }
}

bacResult <- function(tasa, nperiodos, user_inputs) {
  result <- vector("double", 5)
    result[[1]] <- tasa
    result[[2]] <- nperiodos
    result[[3]] <- (as.numeric(user_inputs[[3]]) - as.numeric(user_inputs[[4]]))
    result[[4]] <- (result[[3]] + (result[[3]] * result[[1]]))/result[[2]]
    result[[5]] <- result[[4]]/2
    result
  
}
###################

## - SHARING INFO PARA SNIPPET
#####################################
share <- list(
  title = "Trust Forms",
  url = "https://www.datanautas.com",
  image = "https://www.datanautas.com/wp-content/uploads/2019/02/icon_datanautas-e1549591621239.png",
  description = "Un shiny app tipo formulario que almacena informacion",
  twitter_user = "Czor_Salad"
)
#######################

####
###
##
# USER INTERFACE ##
##
###
####

  ## HTML TAGS, HTML INFO, HEADERS ##
  #####################################
shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    title = "Trust Forms - Datanautas",
    tags$head(
      tags$link(rel = "shortcut icon", type="image/x-icon", 
                href="https://www.datanautas.com/wp-content/uploads/2019/02/icon_datanautas-e1549591621239.png"),
      
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
      href="http://www.gcp.com.pa/cfesa.asp",
      tags$img(style="position: absolute; top: 0; right: 0; border: 0;",
               src="cfesa_logo.png",
               alt="Visitar pagina de CFESA")
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
        br(),br(),
        actionButton("log_in_btn", "Iniciar Sesion", class = "btn-primary", icon = icon("sign-in-alt")),
        br()
    ),
  ############################
  
  ## MENU DE NAVEGACION ##      
    fluidRow(
      div(id = "loading-content", h1("Cargando... ", icon("spinner", class="fa-spin")),
          p("Bienvenido a TrustForms!")),
            
      tabsetPanel(id = "mainNav",
    ## COTIZAR PANEL
    ##################################
            tabPanel("Cotizar",
                value = "cotizar",
                sidebarLayout(
                  sidebarPanel(
                       div(style = "padding: 1% 0% 0% 2%;",
                        downloadButton("cotizacion_report_btn", "Generar cotizacion"),
                        textInput("cotizar_nombre", "Nombre del cliente", ""),
                        selectInput("cotizar_tipo_finan", "Tipo de financiamiento", choices = c("Fideicomiso", "Otro", "Otro 2")),
                        numericInput("cotizar_monto_total", "Valor del Auto", value = 0, step = 1000),
                        numericInput("cotizar_abono", "Abono ($)", value = 0, step = 1000),
                        sliderInput("cotizar_nper", "Numero de Periodos", value = 84, min = 0, max = 90, step = 6),
                        selectInput("cotizar_modelo", "Tipo de Auto", choices = c("Camioneta", "Sedan", "Agya"), selected = "Camioneta")
                       )
                  ),
                  mainPanel()
                )
              ),
    ###########################

    ## SEGUIMIENTO PANEL
    ##################################
              tabPanel("Seguimiento",
                       value = "seguimiento"),
    ###########################

    ## SOLICITUDES PANEL
    ##################################
              tabPanel("Solicitudes",
                      value = "registrar",
             column(12,
                    column(2,
                           uiOutput("sesionVerificada")),
                    
                    shinyjs::hidden(
                      span(id = "submit_msg", "Registrando..."),
                      div(id = "error",
                          div(br(), tags$b("Error: "), span(id = "error_msg"))
                      )
                    ),

                    column(1, div(id = "boton_registrar",
                        style="display: inline-block;vertical-align: top;padding: 10px; padding-left: 10px;",
                        actionButton("submit", "Registrar", class = "btn-success", icon = icon("user-plus")), br(),br(),
                        actionButton("modificar_reg_btn", "Modificar", class = "btn-secondary", icon = icon("pencil-alt"))
                        )),
                    column(9, 
                           div(id = "load_client",
                             h4("Buscar financiamiento"),
                             column(4, textInput("search_identif", "No. de ID", ""),
                                    p(tags$em("Para modificar una solicitud, hacer doble click en el campo que desea editar."))),
                             column(4, numericInput("search_transnum", "# de Transaccion", value = 1, step = 1)),
                             column(4, div(style="padding-top: 12%;", 
                                           actionButton("delete_client_btn", "Eliminar", class = "btn-danger", icon = icon("trash-alt")))
                                    )
                          ),
                          div(id = "load_client_table",
                                 uiOutput("clienteLoadContainer")
                          )
                      )
                ),
                    br(),
                  column(6,
                    div(id = "titulo_form1", h3("Datos del Auto & Financiamiento")),
                    column(6,
                    # Form parte 1.1
                      div(
                        id = "form1",
                        numericInput("auto_valor", labelMandatory("Valor del Auto"),value = 0, step = 10000, min = 0),
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
             ),
              shinyjs::hidden(
                div(
                  id = "modificar_msg",
                  h3("Se modificó el financiamiento exitosamente!"),
                  actionLink("regresar_registrar", "Regresar")
                )
              )
            ),
    ###########################

    ## FINANCIAMIENTOS (DATABASE) PANEL
    ##################################
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
    ###########################

    ## DOCUMENTACION (DOCS GENERATION) PANEL
    ##################################
      tabPanel("Documentacion",
               value = "documentacion"),
    ###########################

    ## DASHBOARD KPIs PANEL
    ##################################
      tabPanel("Dashboard",
               value = "dashboard"),
    ###########################

    ## AYUDA PANEL
    ##################################
      tabPanel("Ayuda",
               value = "ayuda")

    ###########################
    # Parentesis del cierre de Panel de Tabs
    )
  # Parentesis del cierre de Fluid Page
    )
 # Parentesis del cierre de UI
  ),


####
### 
##
#  SERVER ##
## 
###
####

  server = function(input, output, session) {
    
    ## OBSERVE
    #################################
    
    # Habilitar boton de Registrar cuando se llenan todos los campos mandatorios y password es correcto
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(c(all(mandatoryFilled), !is.null(input$shinyalert) && esAgente()))
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Habilitar boton de Modificar si el password es correcto 
    observe({
      mandatoryModificar <- all(c(esAgente()))
      shinyjs::toggleState(id = "modificar_reg_btn", condition = mandatoryModificar)
    })
    
    # Habilitar boton de eliminar si el password es de admin y si hay input.
    observe({
      req(input$search_identif)
      mandatoryEliminar <- all(c(esAgente(), searchEnable()))
      shinyjs::toggleState(id = "delete_client_btn", condition = mandatoryEliminar)
    })
    
    # Habilitar boton de Descargar si el password es correcto 
    observe({
      valid_password_admin <- input$password %in% password_list_admin
      
      shinyjs::toggleState(id = "downloadBtn", condition = valid_password_admin)
    })
    ###########################
    
    ## ESTADO INICIAL
    #################################
    
    # Loading de la aplicacion
    shinyjs::hide("loading-content", anim = TRUE, animType = "fade")
    
    # Ensenar panel de modificar registros cuando clickean el boton Modificar
    shinyjs::hide("load_client")
    shinyjs::onclick("modificar_reg_btn",
                     shinyjs::toggle(id = "load_client", anim = TRUE))
    
    # Esconder panel de modificar
    shinyjs::hide("cliente_info_table")
    
    #Deshabilitar boton de Modificar
    shinyjs::disable("modificar_reg_btn")
    shinyjs::disable("delete_client_btn")
    
    # Administrador de Usuarios y Contrasenas de agente
    password_list_admin <- c("lenovo179447")
    password_list_agente <- c("RP-ALB-7919", "RP-PDE-7919")
    usernames_ID <- c("Luis Repetto", "Gina Gomez")
    names(usernames_ID) <- password_list_agente
    
    # Definir proxy para reactividad del datatable de "Modificar"
    proxy <- DT::dataTableProxy("clienteLoadTable")
    
    ###########################
    
    ## VALORES REACTIVOS
    #################################
    # Combinar todos los inputs y agregar tiempo. Unlist para mongoDB
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- unlist(data)
      data <- c(data, timestamp = epochTime(), IDcounter = counterID(input$identificacion_numero), 
                IDagente = input$shinyalert, estatus = "Pendiente")
      data <- as.data.frame(t(data))
      data
    })
    
    # Data de registro unico a modificar. Usada en el data table de "Modificar"
    d5 <- reactive({searchDataMongo(searchClientMongo())
    })
    
    # Inputs variables para calcular cotizaciones
    reactiveCotizar <- reactive({
      cotizar_inputs <- sapply(cotizar_fields, function(x) input[[x]])
      cotizar_inputs
    })
    
    # Combinar todos los inputs para el serial - Se utiliza en mongoDB
    serialID <- reactive({
      req(input$identificacion_numero)
      serialID_inf <- sapply(serialID_fields, function(x) input[[x]])
      serialID_inf <- paste(serialID_inf, collapse = "-")
      serialID_inf <- paste(serialID_inf, counterID(input$identificacion_numero), sep = "-")
      serialID_inf
    })
    
    # Buscar un 1 cliente en el mongoDB para mostrar en el DT y ser editado
    searchClientMongo <- reactive({
      req(input$search_identif)
      searchID_inf <- sapply(searchID_fields, function(x) input[[x]])
      searchID_inf
    })
    
    # Campos de cliente search lleno
    searchEnable <- reactive({
      req(input$search_identif)
      test_valid <- nchar(input$search_identif) >= 5 & !is.null(input$search_identif)
      test_valid
    })
    
    #User authentication
    # Determinar si el usuario es administrador
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })
    
    # Determinar si el usuario es agente
    esAgente <- reactive({
      !is.null(input$shinyalert) && input$shinyalert%in% password_list_agente
    })
    ###########################
    
    ## INTERACTIVIDAD JS Observe Events
    #################################
    # Cuando se clickea el boton de Registrar, registrar la respuesta
    observeEvent(input$submit, {
      
      # Experiencia de usuario
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      shinyjs::hide("modificar_registros")
      
      # Guardar la data (ensenar mensaje de error si hay alguno) y resetea y esconde los formularios
      tryCatch({
        saveDataMongo(formData())
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
    
    # Regresar despues de modificar un registro
    observeEvent(input$regresar_registrar, {
      shinyjs::reset("load_client")
      shinyjs::reset("modificar_registros")
      shinyjs::hide("modificar_registros")
      shinyjs::hide("load_client")
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
    
    # Borrar un cliente de la base de datos
    observeEvent(input$delete_client_btn, {
      shinyjs::reset("load_client")
      deleteDataMongo(searchClientMongo())
    })
    
    # PopUp de iniciar de sesion
    observeEvent(input$log_in_btn, {
      #ensenar popup para log in
      shinyalert(title = "Iniciar sesion",
                 text = "Ingrese sus credenciales para acceder a Trust Forms",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = FALSE,
                 type = "input",
                 showConfirmButton = TRUE,
                 showCancelButton = TRUE,
                 confirmButtonText = "Iniciar Sesion",
                 cancelButtonText = "Cancelar",
                 confirmButtonCol = "#316FEE",
                 timer = 0,
                 html = TRUE,
                 animation = "slide-from-top",
                 inputPlaceholder = "Coloque su ID",
                 inputType = "password"
      )
    })
    
    # Modificar registro cuando se edita una celda en el datatable
    observeEvent(input$clienteLoadTable_cell_edit, {
      info <- input$clienteLoadTable_cell_edit
      str(info)
      i <- info$row
      j <- info$col
      v <- info$value
      col_to_edit <- colnames(d5())[j + 1]
      col_to_edit
      DT::replaceData(proxy, d5(), resetPaging = FALSE)  # important
      editDataMongo(searchClientMongo(), col_to_edit, v)
      shinyjs::reset("load_client")
      shinyjs::disable("delete_client_btn")
      
    })
    ##########################
    
    ## OUTPUTS
    #################################
    # Panel de con base datos descargable para administradores
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
        
        DT::dataTableOutput("responsesTable"), br()
        )
      )
    })
    
    # Panel con tabla con info de cliente una vez es colocada su info
    output$clienteLoadContainer <- renderUI({
      if (!esAgente()) return()
      
      div(id = "cliente_info_table",
          DT::dataTableOutput("clienteLoadTable"), br()
        )
    })
    
    # Panel con mensaje de verificacion de sesion
    output$sesionVerificada <- renderUI({
      if (!esAgente()) return()
      
      div( 
          id = "authentication",
          style="display: inline-block;vertical-align: top;",
          h5("Sesion activa. Puede registrar y modificar financiamientos.")
      )
    })
    
    # Ensenar registros en la tabla de administrador
    output$responsesTable <- DT::renderDataTable({
      data <- loadDataMongo()
      data[data == ""] = NA
      data$cliente_fecha_naci <- as.Date(as.numeric(data$cliente_fecha_naci), origin="1970-01-01")
      data$timestamp <- as.POSIXct(as.numeric(data$timestamp), origin="1970-01-01")
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
    
    # Tabla con info del cliente
    output$clienteLoadTable <- DT::renderDataTable({
      cliente_data <- searchDataMongo(searchClientMongo())
      cliente_data[cliente_data == ""] = NA
      cliente_data$timestamp <- as.POSIXct(as.numeric(cliente_data$timestamp), origin="1970-01-01")
      cliente_data$cliente_fecha_naci <- as.Date(as.numeric(cliente_data$cliente_fecha_naci), origin="1970-01-01")
      DT::datatable(
        cliente_data,
        editable = TRUE,
        rownames = FALSE,
        options = list(lengthChange = FALSE,
                       scrollX = TRUE,
                       searching = FALSE)
      ) 
    })
    
    # Permitir a usuarios descargar los registros
    output$downloadBtn <- downloadHandler(
      filename = function() {
        sprintf("trust_forms_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadDataMongo(), file, row.names = FALSE)
      }
    )
    
    # Cotizacion RMD html para descargar
    output$cotizacion_report_btn <- downloadHandler(
      filename = function() {sprintf("cotizacion_%s.html", humanTime())
        },
      content = function(file) {
        # Copiar el reporte a un directorio temporal
        cotizacion_temporal <- file.path(tempdir(), "cotizacion_template.Rmd")
        file.copy("cotizacion_template.Rmd", cotizacion_temporal, overwrite = TRUE)
        
        # Parametros que pasaran a la cotizacion Rmd
        params <- list(nombre = input$cotizar_nombre,
                       tipo_finan = input$cotizar_tipo_finan,
                       monto_total = input$cotizar_monto_total,
                       abono = input$cotizar_abono,
                       nper = input$cotizar_nper,
                       agente = usernames_ID[input$shinyalert],
                       cotizacion_global = cotizacion_format(prestamoGlobalBank(reactiveCotizar())),
                       cotizacion_bac = cotizacion_format(prestamoBAC(reactiveCotizar()))
                       )
        
        rmarkdown::render(cotizacion_temporal, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    ###########################
  }
)