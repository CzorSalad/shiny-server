library(tidyverse)
library(stringr)

## Crear primera lista de parametros
######################
paramsDir <- c("parametrosBancos")
files <- list.files(file.path(paramsDir), full.names = TRUE)
params_bancos <- read_csv(files)

tasas_bancos <- c(0.0450, 0.0500, 0.0410, 0.0415, 0.0510, 0.0600)
tasas_36meses <- tasas_bancos + 0.013
tasas_buen_abono <- tasas_bancos - 0.013

params_bancos <- tibble(tasas_bancos, tasas_36meses, tasas_buen_abono)
params_bancos <- t(params_bancos)

colnames(params_bancos) <- names(bancos)
params_bancos <- as.data.frame(params_bancos)
params_bancos

write_csv(params_bancos, files, col_names = TRUE)
########################


# VALORES
totalidad <- 40000
abono <- 10000
nper <- 60
tipoplazo <- c(2)
bancos <- c( "BAC" = 1, "Global Bank"= 2, "Scotia Bank" = 3, "Banco General" = 4,
             "Multibank" = 5, "Banistmo" = 6)

### Funcion para porcentaje y dolares
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

dolares <- function(x, digits = 2, format = "f", ...) {
  paste0("$ ",formatC(x, format = format, digits = digits, big.mark = ",", ...))
}

# DIRECTORIO
paramsDir <- c("parametrosBancos")

# FUNCION
loadParamsBanco <- function() {
files <- list.files(file.path(paramsDir), full.names = TRUE)
params_bancos <- read_csv(files)
params_bancos
}

paramsBancoData <- function(params, tipoplazo, bancos, monto_total, abono, nper) {
params_bancos <- t(params)
params_bancos <- as.data.frame(params_bancos)

params_table <- params_bancos %>%
  mutate(
    tasa_aplicable = params_bancos[ , tipoplazo],
    monto_total = abono + (abono * tasa_aplicable),
    letra_quincenal = round(monto_total / nper, 2),
    letra_mensual = dolares(letra_quincenal / 2),
    letra_quincenal = dolares(letra_quincenal),
    monto_total = dolares(monto_total),
    tasa_aplicable = percent(tasa_aplicable)) %>%
  select(-V1, -V2, -V3)

row.names(params_table) <- names(bancos)
colnames(params_table) <- c("Tasa aplicable", "Monto total", "Letra quincenal", "Letra mensual")

params_table
}

## Test run posiblemente un reactive value
###############

testdatafortable <- paramsBancoData(loadParamsBanco(), tipoplazo, bancos, totalidad, abono, nper)
testdatafortable


# REACTIVE VALUE PER BANK
###########################

### GLOBAL BANK ###
abono_pct <- c( 0.1, 0.2, 0.3)
tasas <- c(0.0650, 0.0625, 0.0575)
max_nper <- c(84,84,90)

agya_tasa <- c(0.0650)
agya_nper <- c(84)
camineta_tasa <- c(0.0575)
camioneta_nper <- c(90)


pruebainputs <- c("Luis", "90", "25000", "5000", "Camioneta")
paverq <- as.double(pruebainputs[[3]]) - as.double(pruebainputs[[4]])

## GLOBALBANK
##################
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

## FORMATO PARA PRESENTAR RESULTADOS DE COTIZACION EN TABLA
#usar cotizar_tableformat(prestamoGlobalBank(reactiveCotizar()))
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
#####################


### Funcion para porcentaje y dolares
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

dolares <- function(x, digits = 2, format = "f", ...) {
  paste0("$ ",formatC(x, format = format, digits = digits, big.mark = ",", ...))
}


