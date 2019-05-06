library(mongolite)
library(jsonlite)

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

options(mongodb = list(
  "host" = "datanautas0",
  "username" = "datanautas",
  "password" = "dunlopdataman69420"
))
databaseName <- "trust_forms_testing"
collectionName <- "financiamientos"

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
  data <- as.data.frame(t(data))
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
  data[data == ""] = NA
  data
}

deleteDataMongo <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName, db = databaseName,
              url = "mongodb+srv://datanautas:dunlopdataman69420@datanautas0-vhd4e.mongodb.net/test?retryWrites=true")
  
  # Delete data
  db$remove("{}", just_one = TRUE)
}

queryDataMongo <- function() {
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
  data <- db$find(query = '{"cliente_nombre" : "Luis"}',
                  limit = 1)
  data
}

cliente1 <- c("name" = "Kanye West", "age" = 45, "fav_food" = "Mexican", "profession" = "Music Producer", "comments" = "Famous rapper, disliked by many for his egotistical behaviour.")
cliente2 <- c("name" = "Tom Sawyer", "age" = 68, "fav_sport" = "Football", "fav_team" = "Manchester United", "fav_player" = "Lukaku")
cliente3 <- c("name" = "Napoleon Bonaparte", "fav_team" = "Paris Saint-Germain", "purpose" = "Conquer Europe", "comments" = "Loved by the people of France despite of his stature", "age" = 55)
view(cliente1)
view(datacsvtry)

datacsvtry <- read_csv("databaseCFESA/20190422-100617_8-666-6666-7.csv")
# probar funcipnes

queryLuis <- queryDataMongo()



deleteDataMongo()
deleteDataMongo()

testmongodb <- loadDataMongo()
is.data.frame(testmongodb)
testmongodb[testmongodb == ""] = NA

testmongodb

counterID <- function(input) {
  database <- loadDataMongo()
  id_numbers <- database$identificacion_numero
  id_numbers <- id_numbers[!is.na(id_numbers)]
  repeticiones <- sum(id_numbers == input) + 1
  repeticiones
}

counterID("8-858-2294")
view(testmongodb)


typeof(testmongodb)
testmongodb <- unlist(testmongodb)
testmongodb <- as.data.frame(testmongodb)


data$cliente_fecha_naci <- as.Date(data$cliente_fecha_naci, origin="1970-01-01")
data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")

query = '{"cliente_nombre" : "Luis"}'


data <- db$find(query = sprintf("'{'%s' : '%s'}'", searchID_fields, search_id),
                limit = 1)

searchID_fields <- c("identificacion_numero")
search_id <- c("8-858-2294")
holi <- sprintf('{"%s" : "%s"}', searchID_fields, search_id)

holi

querydosDataMongo <- function() {
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
  data <- db$find(query = sprintf('{"%s" : "%s"}', searchID_fields, search_id),
                  limit = 1)
  data
}

timetoprov <- querydosDataMongo()
timetoprov

