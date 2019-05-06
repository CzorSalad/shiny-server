
# Campos del buscador de registros
search_fields <- c("search_nombre", "search_apellido", "search_fecha_naci", "search_id")

# Campos con nuevos valores
values_fields <- c("text_input", "date_input", "numeric_input", "logical_input")

# Campos del seleccionador para cambio de campos
select_fields <- c("search_nombre", "search_apellido", "search_fecha_naci", "search_id")


# Values Reactivos de nuevo input para la modificacion de un registro
formModValues <- reactive({
  responses_mod_values <- sapply(values_fields, function(x) input[[x]])
  with(data.frame(responses_mod_values), replace(responses_mod_values, responses_mod_values == 0, NA))
  with(data.frame(responses_mod_values), replace(responses_mod_values, responses_mod_values == "No modificar", NA))
  responses_mod_values <- responses_mod_values[!is.na(responses_mod_values) & !is.null(responses_mod_values)]
  responses_mod_values
})

# Variables Reactivas de campos seleccionados para la modificacion de un registro
formModVariables <- reactive({
  responses_mod_variables <- sapply(select_fields, function(x) input[[x]])
  responses_mod_variables <- responses_mod_variables[!is.na(responses_mod_values)]
  responses_mod_variables
}) 

# Values reactivos para la busqueda de los registros de un cliente
searchClient <- reactive({
  search_id <- sapply(search_fields, function(x) input[[x]])
  search_id <- t(search_id)
  search_id
}) 

# Funcion para loadear un file especifico usar funcion cliente_load(searchClient())
cliente_load <- function(search_id) {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  files_filter <- str_detect(files, digest::digest(search_id),ignore_case = FALSE)
  files_extract <- files[files_filter]
  cliente_data <- lapply(files_extract, read.csv, stringsAsFactors = FALSE)
  cliente_data
}



# Funciones para MODIFICAR
# Determinar file a overwrite con nueva info de cliente
# Usar dentro de cliente_modificar()
file_overwrite <- function(search_id) {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  files_filter <- str_detect(files, digest::digest(search_id),ignore_case = FALSE)
  files_extract <- files[files_filter]
  files_extract
}

# Encontrar el index number de cada variable que cambio y modificar los values de un registro ya hecho
# y guardar los nuevos valores usar funcion cliente_modificar(formModVariables(),formModValues())
cliente_modificar <- function(variablesMod, valuesMod) {
indexReal <- vector("double", length(variablesMod))
for (i in seq_along(variablesMod)) {
    indexReal[[i]] <- which(variblesMod[[i]] == fieldsAll)
  }
      indexReal
      data_original <- cliente_load(searchClient())
      
for (i in indexReal) {
    data_original[[i]] <- valuesMod[[i]]
  }
      data_to_save <- data_original
      write.csv(x = data_to_save, file = file.path(responsesDir, file_overwrite()),
          row.names = FALSE, quote = TRUE)
}

# Funcion para DELETE un cliente usar con cliente_delete(searchClient())
financiamiento_delete <- function(search_ID) {
  #Definir nombre de file a borrar
  cliente_delete <- list.files(file.path(responsesDir), full.names = TRUE)
  files_filter <- str_detect(cliente_delete, search_ID)
  cliente_fn <- cliente_delete[files_filter]
  # Validar su existencia
  if (file.exists(cliente_fn)) 
    # Borrarlo si existe
    file.remove(cliente_fn)
}




# Campos totales a registrar en la base de datos en este orden
fieldsAll <- c("shinyalert", "auto_valor", "auto_anos_uso", "finan_cuotas", "finan_calculo",
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

# Campos de texto 
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


# Campos de fecha
fecha_select_fields <- c("Cliente fecha de nacimiento" = "cliente_fecha_naci", "Cliente ingreso trabajo" = "laboral_fecha_ingreso",
                         "Cliente ingreso trabajo anterior" = "laboral_ant_fecha_ingreso", "Cliente egreso trabajo" = "laboral_ant_fecha_egreso",
                         "Conyugue fecha de nacimiento" = "conyugue_fecha_naci", "Conyugue ingreso trabajo" = "conyugue_ingreso")

# Campos de CheckBox
logical_select_fields <- c("Seguro de auto" = "auto_seguro", "Incluir mantenimiento" = "auto_mantenimiento",
                            "Refinanciado" = "finan_refinan", "Descuento directo" = "finan_desc_dir",
                            "Tiene garante" = "cliente_garante", "Tiene auto" = "cliente_auto",
                            "Es mancomunado" = "cliente_mancomunado")

# Campos numericos
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


#
#
# EXPERIMENTAL
#
#



digest_fields <- t("Luis", "Repetto", 8158, "8-585-2294")
pruebadig <- digest::digest(digest_fields)


search_id <- c("Isabella", "Repetto", 8158, "8-777-5555")
digest_inf <- c(search_id)
digest_inf <- t(digest_inf)
digest_inf

filenames <- digest::digest(digest_inf)
filenames


cliente_load <- function(search_id) {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  files_filter <- str_detect(files, digest::digest(search_id))
  files_extract <- files[files_filter]
  cliente_data <- lapply(files_extract, read.csv, stringsAsFactors = FALSE)
  cliente_data <- do.call(rbind, cliente_data)
  cliente_data
}

responsesDir <- file.path("databaseCFESA")



files <- list.files(path = file.path(responsesDir), full.names = TRUE)
files_filter <- str_detect(files, digest::digest(search_id))
files_extract <- files[files_filter]
cliente_data <- lapply(files_extract, read.csv, stringsAsFactors = FALSE)
cliente_data <- do.call(rbind, cliente_data)
cliente_data




pruebastub <- substr(digest_inf, 1, 3)
serialIDwee <- paste(pruebastub, collapse = "-")
serialIDwee


# Reactivos para MODIFICAR
# Values Reactivos de nuevo input para la modificacion de un registro
formModValues <- reactive({
  responses_mod_values <- sapply(values_fields, function(x) input[[x]])
  with(data.frame(responses_mod_values), replace(responses_mod_values, responses_mod_values == 0, NA))
  with(data.frame(responses_mod_values), replace(responses_mod_values, responses_mod_values == "No modificar", NA))
  responses_mod_values <- responses_mod_values[!is.na(responses_mod_values) & !is.null(responses_mod_values)]
  responses_mod_values
})

# Variables Reactivas de campos seleccionados para la modificacion de un registro
formModVariables <- reactive({
  responses_mod_variables <- sapply(select_fields, function(x) input[[x]])
  responses_mod_variables <- responses_mod_variables[!is.na(responses_mod_values)]
  responses_mod_variables
}) 



pruebadereplace <- c("No hay cambio", "Fecha de Nacimiento", NA, "Luis", "Repetto")

pruebaderepe <- pruebadereplace[!is.na(pruebadereplace)]

pruebaderepedos <- pruebaderepe[pruebaderepe != "No hay cambio"]


vectortest <- vector("character", 60)


testvectortime <- c(1:15)
indexreplacement <- c(1,5,8)
valuereplacement <- c(6, 6, 6)

vectortest[indexreplacement] <- valuereplacement


#suma para contar

ggnames <- c("Ingir", "Isa", NA, "Luis", "Luis", NA, "Luis", "Maru", "Maru")
prueb <- !is.na(ggnames)
pruebita <- ggnames == "Luis"
ggnames <- ggnames[!is.na(ggnames)]


contadornames <- sum(ggnames == "Luis") + 1

ggnamesprueb <- ggnames[ggnames == "Luis"]

paveqpasa <- paste (c("hola"), c("luis"), collapse = "-")

# CONTADOR
counterID <- function(input) {
  database <- loadData()
  id_numbers <- database$identificacion_numero
  repeticiones <- sum(id_numbers == input) + 1
  repeticiones
}

# Funcion para cargar todas las respuestas previas en un dataframe. Combina todos los archivos .csv que encuentre en el directorio
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}


# Combinar todos los inputs para el serial
serialID <- function () {
  serialID_inf <- sapply(serialID_fields, function(x) input[[x]])
  serialID_inf <- paste(serialID_inf, collapse = "-")
  serialID_inf <- paste(serialID_inf, counterID(ggnames), sep = "-")
  serialID_inf
}

paverpue <- counterID("8-666-6666")

datapaver <- loadData()
paverbien <- datapaver$identificacion_numero
pavermejor <- paverbien[paverbien == "8-666-6666"]
pavermejor <- length(pavermejor[!is.na(pavermejor)])



formModValues <- reactive({
  responses_mod_values <- sapply(values_fields, function(x) input[[x]])
  responses_mod_values <- responses_mod_values[!is.na(responses_mod_values)]
  responses_mod_values <- responses_mod_values[responses_mod_values != "No hay cambio"]
  responses_mod_values
})

