library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(corrplot)

"carros.csv es un set de datos que encontramos en github en la dirección:
https://gist.githubusercontent.com/ahcamachod/1595316a6b37bf39baac355b081d9c3b/raw/98bc94de744764cef0e67922ddfac2a226ad6a6f/car_prices.csv"
carros <- read.csv("D:/BajadoEnD/20231218Módulo05Estadística&ProgramaciónConR/carros/carros.csv")

poly_model <- lm(precio ~ poly(edad_del_modelo, 2) + poly(km_por_anio, 2), data = carros)

predict_car <- function(edad, km) {
  prediction <- predict(poly_model, newdata = data.frame(edad_del_modelo = edad, km_por_anio = km))
  return(prediction)
}

ui <- fluidPage(
  titlePanel("Predicción de venta de automóviles"),
  sidebarLayout(
    sidebarPanel(
      numericInput("edad", "Edad del modelo:", 1),
      numericInput("km", "Kilómetros por año:", 0),
      actionButton("submit", "Aceptar"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predicción", 
                 textOutput("prediction_text"), 
                 textOutput("prediction_text2")),
        tabPanel("Datos", 
                 DTOutput("carros_table")),
        tabPanel("Estadísticas", 
                 verbatimTextOutput("summary"), 
                 plotOutput("hist_precio")),
        tabPanel("Regresión", 
                 plotOutput("regression_plot")),
        tabPanel("Correlación", 
                 plotOutput("correlation_plot")),
        tabPanel("Equipo 21",
                 h4("Febrero 2023"),
                 h4("Proyecto Módulo V"),
                 h5("Christian Arturo Meza Álvarez kosa_inc@hotmail.com"),
                 h5("Enrique Narváez Rodriguez nihaonarvi@gmail.com "),
                 h5("Juan Gerardo Granados Gálvez ggranadosg@gmail.com"),
                 h5("Jorge Enrique Madariaga Puertas madpue6@gmail.com")
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    prediction <- predict_car(input$edad, input$km)
    output$prediction_text <- renderText({
      pred <- ifelse(prediction > 0.5, "Sí", "No")
      paste("Predicción de venta:", pred)
    })
    
    output$prediction_text2 <- renderText({
      pred <- ifelse(prediction > 0.5, "Sí", "No")
      paste("Precio estimado de venta:", round(prediction, 0))
    })
  })
  
  output$carros_table <- renderDT({
    datatable(carros, options = list(searching = TRUE), filter = 'top') %>%
      formatRound(1:ncol(carros), digits = 0)
  })
  
  output$summary <- renderPrint({
    summary(carros)
  })
  
  output$hist_precio <- renderPlot({
    ggplot(carros, aes(x = precio)) + 
      geom_histogram(binwidth = 500) + 
      labs(title = "Histograma de precios", x = "Precio", y = "Frecuencia")
  })
  
  output$regression_plot <- renderPlot({
    ggplot(carros, aes(x = km_por_anio, y = precio)) + 
      geom_point() + 
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) + 
      labs(title = "Regresión polinómica de grado 2", x = "Kilómetros por año", y = "Precio")
  })
  
  output$correlation_plot <- renderPlot({
    correlation_matrix <- cor(carros[, c("precio", "edad_del_modelo", "km_por_anio")])
    corrplot(correlation_matrix, method = "color")
  })
}

shinyApp(ui = ui, server = server)
