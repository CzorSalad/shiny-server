## Eaves-Dropper
# Shiny App para monitorear el sentiment del stock market
# utilizando info de Yahoo Finance

## - DEPENDENCIAS - CARGAR PAQUETES - ##
##################
## DEPENDENCIAS - CARGAR PAQUETES
library(shiny)
library(mongolite)
library(tm)
library(tidytext)
library(tm.plugin.webmining)
library(purrr)
library(XML)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
######

## - MongoDB INFO Y CREDENCIALES - ##
##################
options(mongodb = list(
  "host" = "datanautas0",
  "username" = "datanautas",
  "password" = "dunlopdataman69420"
))
databaseName <- "eaves_dropper"
collectionName <- "resultados_loughran"
######
 
## - FUNCIONES R - ##
##################

# Marcar un campo como mandatorio con el asterisco rojo
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Printear valores en porcentaje para usar en datatables
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Colocar tiempo actual "timestamp" para cada registro
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# Formatear el tiempo de la respuesta a un formato mas legible
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# EXTRAER NOTICIAS DE YAHOO FINANCE
YahooFinanceSourceRepetto <- function (symbol, ...) {
  fq <- sprintf("http://feeds.finance.yahoo.com/rss/2.0/headline?s=%s&region=US&lang=en-US", symbol)
  parser <- function(cr) {
    tree <- parse(cr, type = "XML", asText = TRUE)
    xpathSApply(tree, path = "//item")
  }
  ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, 
                  reader = readYahoo, postFUN = getLinkContent, retrieveFeedURL = TRUE, 
                  ...)
  ws
}

## EJECUTAR DOWNLOAD DE NOTICIAS Y FORMATEAR EN DATAFRAME
download_articles <- function(symbol) {
  WebCorpus(YahooFinanceSourceRepetto(symbol))
}

######

## - FUNCIONES MongoDB - ##
##################
# Funciones para Guardar/LoadTodos/Borrar/Editar/Load1 los registros de resultados Loughran
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
######

## - CAMPOS DE INPUTS (FORM) - ##
##################
company_fields <- c("company_1", "company_2", "company_3", "company_4", "company_5", "company_6")
symbols_fields <- c("symbols_1", "symbols_2", "symbols_3", "symbols_4", "symbols_5", "symbols_6")
######

## - SHARING INFO PARA SNIPPET - ##
##################
share <- list(
  title = "The Eaves-Dropper",
  url = "https://www.datanautas.com",
  image = "https://www.datanautas.com/wp-content/uploads/2019/02/icon_datanautas-e1549591621239.png",
  description = "Monitor del sentiment en el stock market. Desarrollado por Datanautas.",
  twitter_user = "Czor_Salad"
)
######

####
###
##
# USER INTERFACE ##
##
###
####

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  
  ## - HTML TAGS, HTML INFO, HEADERS - ##
  #################
  title = "Eaves-Dropper",
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
    href="https://www.datanautas.com",
    tags$img(style="position: absolute; top: 0; right: 0; border: 0;",
             src="datanautas_logo.png",
             alt="Ir a Datanautas.com")
  ),
  div(id = "header",
      h1("The Eaves-Dropper"),
      h4(tags$em("A stock market sentiment observatory")),
      br(),
      strong( 
        span("Desarrollado por Luis Repetto para"),
        a("Datanautas", href = "https://www.datanautas.com"),
        HTML("&bull;"),
        span("Código disponible"),
        a("en GitHub", href = "https://github.com/CzorSalad/shiny-server/tree/master/eavesdropper")
      ),
      br(),br()
  ),
  ############ 

  ## - INPUTS - ##
  ###################
   sidebarLayout(
      sidebarPanel(
        p(span("Ingresa los "), tags$em("symbols"), span("de la o las companias que quieras analizar y haz click en"),
        strong(span("EAVESDROP"))),
        br(),
        div(style="display: inline-block;vertical-align: top; padding-left: 10px;",
            actionButton(inputId = "eavesdrop_btn",
                     label = strong("EAVESDROP"),
                     icon = icon("comments-dollar"),
                     class = "btn-success"
                     )
            ),
        div(style="display: inline-block;vertical-align: top; padding-left: 10px;",
            downloadButton("download_btn",
                         label = strong("DESCARGAR"),
                         class = "btn-primary")
            ),
        br(),
        p(strong("Companías:"), style = "padding: 7% 0% 0% 2%;"),
        p(tags$em("eg. AAPL para Apple")),
        textInput("symbols_1",
                     label = "",
                     placeholder = "AAPL",
                      width = '50%'
                     ),
        textInput("symbols_2",
                    label = "",
                    width = '50%'
        ),
        textInput("symbols_3",
                    label = "",
                    width = '50%'
        ),
        textInput("symbols_4",
                    label = "",  
                    width = '50%'
        ),
        textInput("symbols_5",
                    label = "",  
                    width = '50%'
        ),
        textInput("symbols_6",
                    label = "",  
                    width = '50%'
        )
      ),
      
  ##########

  ## - UI OUTPUTS - ##
  ######################
      mainPanel(
        shinyjs::hidden(div(id = "palabras_loughran_table",
           uiOutput("wordsTableContainer")
          )
        ),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Buscando y analizando noticias... Por favor espera unos segundos"),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        ), 
        shinyjs::hidden(div(id = "palabras_loughran_plot",
                            h3("Ratio de Positividad por Empresa"),
                            plotOutput("positivityPlot"))),
        h4("Metodología"),        
        p("Positividad de los articulos evaluada contra el lexicon financiero 'Loughran'. El mismo permite
                  clasificar las palabras bajo un contexto financiero considerando las siguientes categorías 'Positive',
                  'Negative', 'Litigious', 'Uncertainty', 'Constraining' y 'Superflous'.
                  El cálculo consiste en evaluar la presencia de palabras positivas vs. negativas."),
        br(),
        span(p("El ratio de positividad es calculado así: "), tags$em("(positive - negative) / (positive + negative))"))
      )
   )
)
  ##########

####
### 
##
#  SERVER ##
## 
###
####

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Habilitar boton de EAVESDROP
  observe({
    mandatoryFilled <- nchar(input$symbols_1) >= 2
    shinyjs::toggleState(id = "eavesdrop_btn", condition = mandatoryFilled)
  })
  
  # Habilitar boton de Descargar
  observe({
    req(input$eavesdrop_btn)
    mandatoryFilled <- nchar(input$symbols_1) >= 2
    shinyjs::toggleState(id = "download_btn", condition = mandatoryFilled)
  })
  
  #cuando click el boton EAVESDROP
observeEvent(input$eavesdrop_btn, {
  shinyjs::show("submit_msg")

  tryCatch({
    shinyjs::show("palabras_loughran_table")
    shinyjs::show("words_table")
    shinyjs::show("palabras_loughran_plot")
  },
  error = function(err) {
    shinyjs::html("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::hide("submit_msg")
  })
})

shinyjs::disable("download_btn")  
  
## - REACTIVES - ##
##################

  # Buscar noticias y cargar como dataframe con corpus
  articulos_stocks <- reactive({
    stock_articulos <- tibble(symbol = symbols()) %>%
      mutate(corpus = map(symbol, download_articles))%>%
      unnest(map(corpus, tidy)) %>%
      unnest_tokens(word, text) %>%
      select(symbol, datetimestamp, word, id, heading)
    stock_articulos
  })
  
  # Vector con los symbols del user
  symbols <- reactive({
  req(input$symbols_1)
  symbols <- c(input$symbols_1, input$symbols_2, input$symbols_3,
               input$symbols_4, input$symbols_5, input$symbols_6)
  symbols
  })
  
  # 
  stock_sentiment_count <- reactive({
    stock_sentiment_count <- articulos_stocks() %>%
      inner_join(get_sentiments("loughran"), by = "word") %>%
      count(sentiment, symbol) %>%
      spread(sentiment, n, fill = 0)
    stock_sentiment_count
  })
  

#######
  
## - OUTPUTS SERVER - ##
#############   
  # Panel/Container de tabla de contador de palabras
  output$wordsTableContainer <- renderUI({
    div(id = "words_table",
        h3("Presencia de palabras según categoría del 'Loughran' lexicon"),
        DT::dataTableOutput("wordsTable"), br()
    )
  }) 
  
  # Tabla de contador de palabras
  output$wordsTable <- DT::renderDataTable({
    req(input$eavesdrop_btn)
    DT::datatable(
      stock_sentiment_count(),
      editable = FALSE,
      rownames = TRUE,
      options = list(lengthChange = FALSE,
                     scrollX = TRUE,
                     searching = FALSE)
    ) 
  })

  
  
  output$positivityPlot <- renderPlot({
     req(input$eavesdrop_btn)
     stock_sentiment_count() %>%
       mutate(score = (positive - negative) / (positive + negative)) %>%
       mutate(symbol = reorder(symbol, score)) %>%
       ggplot(aes(symbol, score, fill = score > 0)) +
       geom_col(show.legend = FALSE) +
       coord_flip() +
       labs(x = "Companía",
            y = "Ratio de Positividad entre los 20 articulos de noticias más recientes")
   })
  
  
  
  # Permitir a usuarios descargar la tabla
  output$downloadBtn <- downloadHandler(
    filename = function() {
      sprintf("eavesdropper_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(stock_sentiment_count(), file, row.names = FALSE)
    }
  )
#######   
}

# Run the application 
shinyApp(ui = ui, server = server)

