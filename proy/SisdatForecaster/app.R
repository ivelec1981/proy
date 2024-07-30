library(shiny)
library(readxl)
library(forecast)
library(keras)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)
library(patchwork)
library(dygraphs)

# Función para leer y preparar datos
prepare_data <- function(file_path) {
  list(
    tabla11 = read_excel(file_path, sheet = "Proyecciones", range = "B40:L50") %>%
      replace_na(list("2016" = 0, "2017" = 0, "2018" = 0, "2019" = 0, "2020" = 0, "2021" = 0, "2022" = 0, "2023" = 0, "Unidad" = "", "Concepto" = "", "Variable" = "")),
    energia_vendida = read_excel(file_path, sheet = "Proyecciones", range = "B22:L36") %>%
      replace_na(list("2016" = 0, "2017" = 0, "2018" = 0, "2019" = 0, "2020" = 0, "2021" = 0, "2022" = 0, "2023" = 0, "Unidad" = "", "Nivel Tensión" = "", "CatUso" = "")),
    energia_disponible = read_excel("C:/Users/LEGION/Maestria BigData/Visualizacion de Datos/Prueba/Bdd.xlsx", sheet = "EnergiaDisponible")
  )
}

# Función para calcular CAGR
calcular_CAGR <- function(V_inicial, V_final, n) {
  CAGR <- ((V_final / V_inicial)^(1/n)) - 1
  return(CAGR * 100)  # Convertimos a porcentaje
}

# Función para calcular variación anual
calcular_variacion_anual <- function(values) {
  variaciones <- numeric(length(values) - 1)
  for (i in 2:length(values)) {
    variaciones[i-1] <- ((values[i] - values[i-1]) / values[i-1]) * 100
  }
  return(variaciones)
}

# Convertir los datos a formato largo
convert_to_long_format <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = -c(Año, Mes), names_to = "Empresa", values_to = "Value")
  return(df_long)
}

ui <- dashboardPage(
  dashboardHeader(
    title = "SISDAT Forecaster",
    tags$li(class = "dropdown",
            tags$style(HTML("
                .navbar-custom-menu > .navbar-nav > li {
                  display: flex;
                  align-items: center;
                }
            ")),
            div(
              class = "user-info",
              icon("user"), "Ejecutivo", "|", actionLink("logout", "Logout", icon = icon("sign-out"))
            )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Históricos", tabName = "data", icon = icon("upload")),
      menuItem("Proyecciones", tabName = "projections", icon = icon("line-chart")),
      menuItem("Resultados", tabName = "comparisons", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "data",
              fileInput("file", "Subir archivo Excel", accept = c(".xlsm", ".xlsx", ".xls")),
              fluidRow(
                column(width = 6,
                       valueBoxOutput("cagr_energia_box"),
                       valueBoxOutput("cagr_potencia_box"),
                       leafletOutput("mapEcuador", width = "100%", height = "600px")),
                column(width = 6,
                       column(width = 12, plotlyOutput("plot_disponible")),
                       column(width = 12, plotlyOutput("plot_potencia_maxima"))
                )
              ),
              fluidRow(
                column(width = 12,
                       h3("Potencia y Energia"),
                       rHandsontableOutput("tabla11"),
                       h3("Energía Vendida"),
                       rHandsontableOutput("energia_vendida")
                )
              )
      ),
      tabItem(tabName = "projections",
              selectInput("empresa", "Seleccionar Empresa", choices = NULL),  # Filtro de empresa distribuidora
              selectInput("model", "Seleccionar Modelo", choices = c("ARIMA", "LSTM", "GRU", "Holt-Winters")),
              actionButton("run", "Correr Modelos"),
              dygraphOutput("dygraph_energia"),  # Gráfica para mostrar datos
              dataTableOutput("energia_disponible_table"),
              plotlyOutput("plot_energia"),
              plotlyOutput("plot_potencia")
              
              
      ),
      tabItem(tabName = "comparisons",
              plotOutput("comparePlot")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Leer y mostrar datos subidos
  data <- reactive({
    req(input$file)
    prepare_data(input$file$datapath)
  })
  
  energia_disponible_long <- reactive({
    req(data())
    convert_to_long_format(data()$energia_disponible)
  })
  
  observe({
    updateSelectInput(session, "empresa", choices = unique(energia_disponible_long()$Empresa))
  })
  
  
  output$tabla11 <- renderRHandsontable({
    req(data())
    rhandsontable(data()$tabla11) %>%
      hot_col(col = "2016", format = "0,0.00") %>%
      hot_col(col = "2017", format = "0,0.00") %>%
      hot_col(col = "2018", format = "0,0.00") %>%
      hot_col(col = "2019", format = "0,0.00") %>%
      hot_col(col = "2020", format = "0,0.00") %>%
      hot_col(col = "2021", format = "0,0.00") %>%
      hot_col(col = "2022", format = "0,0.00") %>%
      hot_col(col = "2023", format = "0,0.00") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          if (value >= 0 && value < 5000) {
            td.style.backgroundColor = '#ffffff';
            td.style.color = 'black';
            td.style.fontWeight = 'normal';
          } else if (value >= 5000 && value < 10000) {
            td.style.backgroundColor = '#f0f8ff';
            td.style.color = 'black';
            td.style.fontWeight = 'normal';
          } else if (value >= 10000 && value < 20000) {
            td.style.backgroundColor = '#b0c4de';
            td.style.color = 'black';
            td.style.fontWeight = 'bold';
          } else if (value >= 20000 && value < 50000) {
            td.style.backgroundColor = '#4682b4';
            td.style.color = 'white';
            td.style.fontWeight = 'bold';
          } else if (value >= 50000) {
            td.style.backgroundColor = '#2f4f4f';
            td.style.color = 'white';
            td.style.fontWeight = 'bold';
          }
        }
      ")
  })
  
  output$energia_vendida <- renderRHandsontable({
    req(data())
    rhandsontable(data()$energia_vendida,rowHeaders = NULL) %>%
      hot_col(col = "2016", format = "0,0.00") %>%
      hot_col(col = "2017", format = "0,0.00") %>%
      hot_col(col = "2018", format = "0,0.00") %>%
      hot_col(col = "2019", format = "0,0.00") %>%
      hot_col(col = "2020", format = "0,0.00") %>%
      hot_col(col = "2021", format = "0,0.00") %>%
      hot_col(col = "2022", format = "0,0.00") %>%
      hot_col(col = "2023", format = "0,0.00") %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          if (value >= 0 && value < 5000) {
            td.style.backgroundColor = '#ffffff';
            td.style.color = 'black';
            td.style.fontWeight = 'normal';
          } else if (value >= 5000 && value < 10000) {
            td.style.backgroundColor = '#f0f8ff';
            td.style.color = 'black';
            td.style.fontWeight = 'normal';
          } else if (value >= 10000 && value < 20000) {
            td.style.backgroundColor = '#b0c4de';
            td.style.color = 'black';
            td.style.fontWeight = 'bold';
          } else if (value >= 20000 && value < 50000) {
            td.style.backgroundColor = '#4682b4';
            td.style.color = 'white';
            td.style.fontWeight = 'bold';
          } else if (value >= 50000) {
            td.style.backgroundColor = '#2f4f4f';
            td.style.color = 'white';
            td.style.fontWeight = 'bold';
          }
        }
      ")
  })
  
  # # Mostrar datos en tabla para "EnergiaDisponible"
  # output$energia_disponible_table <- renderDataTable({
  #   req(data())
  #   df_long <- convert_to_long_format(data()$energia_disponible)
  #   datatable(df_long)
  # })
  # 
  # filtered_data <- reactive({
  #   req(input$empresa)
  #   energia_disponible_long() %>%
  #     filter(Empresa == input$empresa)
  # })
  # 
  # 
  # # Mostrar datos filtrados en tabla
  # output$filtered_data_table <- renderDataTable({
  #   req(filtered_data())
  #   datatable(filtered_data())
  # })
  
  
  output$energia_disponible_table <- renderDataTable({
    req(input$empresa)
    datatable(energia_disponible_long() %>% filter(Empresa == input$empresa))
  })
  
  
  filtered_data <- reactive({
    req(input$empresa)
    energia_disponible_long() %>%
      filter(Empresa == input$empresa)
  })
  
  
  output$dygraph_energia <- renderDygraph({
    req(filtered_data())
    data <- filtered_data()
    
    # Preparar los datos para dygraph
    data_ts <- ts(data$Value, frequency = 12, start = c(min(data$Año), 1))
    dygraph(data_ts, main = paste("Empresa:", input$empresa)) %>% 
      dyShading(from = "2020-1-1", to = "2020-12-1", color = "#FBAF6F")%>%
      dyEvent("2020-3-16", "Pandemia", labelLoc = "bottom")
  })
  
  observeEvent(input$run, {
    req(filtered_data())
    data <- filtered_data()
    
    if (nrow(data) < 12) {
      showModal(modalDialog(
        title = "Error",
        "La longitud de los datos no es adecuada para el modelo LSTM.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    data_ts <- ts(data$Value, frequency = 12, start = c(min(data$Año), 1))
    
    train <- head(data_ts, round(length(data_ts) * 0.8))
    test <- tail(data_ts, length(data_ts) - length(train))
    
    if (length(train) < 12 | length(test) < 12) {
      showModal(modalDialog(
        title = "Error",
        "La longitud de los datos de entrenamiento o prueba no es adecuada.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    model <- keras_model_sequential() %>%
      layer_lstm(units = 50, input_shape = c(12, 1)) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = "mse",
      optimizer = "adam"
    )
    
    train_matrix <- array_reshape(train, c(length(train) - 11, 12, 1))
    target <- array_reshape(tail(train, length(train) - 11), c(length(train) - 11, 1))
    
    model %>% fit(train_matrix, target, epochs = 50, batch_size = 1, verbose = 0)
    
    test_matrix <- array_reshape(test, c(length(test) - 11, 12, 1))
    predictions <- model %>% predict(test_matrix)
    
    predictions_ts <- ts(predictions, start = c(max(data$Año), 1), frequency = 12)
    
    output$plot_energia <- renderPlotly({
      plot_ly() %>%
        add_lines(x = time(test), y = test, name = "Actual") %>%
        add_lines(x = time(predictions_ts), y = predictions_ts, name = "Predicted") %>%
        layout(title = "Predicción de Energía Disponible")
    })
  })
  
  # Cálculo del CAGR y mostrar en las tarjetas
  output$cagr_energia_box <- renderValueBox({
    req(data())
    tabla <- data()$tabla11
    start_year <- 2016
    end_year <- 2023
    
    energia <- tabla %>%
      filter(Concepto == "Disponible (Distribución)")
    
    cagr_energia <- calcular_CAGR(energia[[as.character(start_year)]], energia[[as.character(end_year)]], end_year - start_year)
    
    valueBox(
      value = paste0(round(cagr_energia, 2), "%"),
      subtitle = tags$div(
        style = "font-size: 14px;",  # Ajusta el tamaño del texto
        "Tasa de crecimiento promedio energía"
      ),
      icon = icon("bolt"),
      color = "blue"
    )
  })
  
  # Similar ajuste para la otra tarjeta
  output$cagr_potencia_box <- renderValueBox({
    req(data())
    tabla <- data()$tabla11
    start_year <- 2016
    end_year <- 2023
    
    potencia <- tabla %>%
      filter(Concepto == "Máxima disponible (Distrib. + Clientes en Transm.)")
    
    cagr_potencia <- calcular_CAGR(potencia[[as.character(start_year)]], potencia[[as.character(end_year)]], end_year - start_year)
    
    valueBox(
      value = paste0(round(cagr_potencia, 2), "%"),
      subtitle = tags$div(
        style = "font-size: 14px;",  # Ajusta el tamaño del texto
        "Tasa de crecimiento promedio potencia"
      ),
      icon = icon("question-circle"),
      color = "yellow"
    )
  })
  
  
  output$plot_disponible <- renderPlotly({
    req(data())
    tabla <- data()$tabla11
    disponible <- tabla %>%
      filter(Concepto == "Disponible (Distribución)")
    
    years <- as.numeric(names(disponible)[4:ncol(disponible)])
    values <- as.numeric(disponible[4:ncol(disponible)]) / 1000  # Convertir a GWh
    
    plot_df <- data.frame(
      Year = years,
      Value = values
    )
    
    variacion_anual <- calcular_variacion_anual(values)
    variacion_df <- data.frame(
      Year = years[-1],
      Variacion = variacion_anual,
      Color = ifelse(years[-1] == max(years), "#6EA6C3", "lightgrey")
    )
    
    p1 <- plot_ly(plot_df, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines+markers',
                  marker = list(color = '#6EA6C3', size = 10, line = list(color = 'white', width = 2)),
                  line = list(color = '#6EA6C3', width = 2)) %>%
      add_text(text = ~format(round(Value), big.mark = ".", decimal.mark = ","), textposition = 'top center', showlegend = FALSE) %>%
      layout(title = "Energía Disponible GWh",
             xaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
             plot_bgcolor = '#ECF0F5',
             paper_bgcolor = '#ECF0F5',
             showlegend = FALSE)
    
    p2 <- plot_ly(variacion_df, x = ~Year, y = ~Variacion, type = 'bar', marker = list(color = ~Color)) %>%
      add_text(text = ~paste0(round(Variacion, 1), "%"), textposition = 'outside', showlegend = FALSE) %>%
      layout(title = "Energía Disponible GWh",
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
             plot_bgcolor = '#ECF0F5',
             paper_bgcolor = '#ECF0F5',
             showlegend = FALSE)
    
    subplot(p1, p2, nrows = 2, shareX = FALSE) %>%
      layout(xaxis = list(showticklabels = TRUE, title = FALSE), xaxis2 = list(showticklabels = FALSE, title = FALSE))
  })
  
  
  output$plot_potencia_maxima <- renderPlotly({
    req(data())
    tabla <- data()$tabla11
    potencia_maxima <- tabla %>%
      filter(Concepto == "Máxima disponible (Distrib. + Clientes en Transm.)")
    
    years <- as.numeric(names(potencia_maxima)[4:ncol(potencia_maxima)])
    values <- as.numeric(potencia_maxima[4:ncol(potencia_maxima)])
    
    plot_df <- data.frame(
      Year = years,
      Value = values
    )
    
    variacion_anual <- calcular_variacion_anual(values)
    variacion_df <- data.frame(
      Year = years[-1],
      Variacion = variacion_anual,
      Color = ifelse(years[-1] == max(years), "orange", "lightgrey")
    )
    
    p1 <- plot_ly(plot_df, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines+markers',
                  marker = list(color = 'orange', size = 10, line = list(color = 'white', width = 2)),
                  line = list(color = 'orange', width = 2)) %>%
      add_text(text = ~format(round(Value), big.mark = ".", decimal.mark = ","), textposition = 'top center', showlegend = FALSE) %>%
      layout(title = "Potencia Máxima disponible MW",
             xaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
             plot_bgcolor = '#ECF0F5',
             paper_bgcolor = '#ECF0F5',
             showlegend = FALSE)
    
    p2 <- plot_ly(variacion_df, x = ~Year, y = ~Variacion, type = 'bar', marker = list(color = ~Color)) %>%
      add_text(text = ~paste0(round(Variacion, 1), "%"), textposition = 'outside', showlegend = FALSE) %>%
      layout(title = "Potencia Máxima disponible MW",
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
             plot_bgcolor = '#ECF0F5',
             paper_bgcolor = '#ECF0F5',
             showlegend = FALSE)
    
    subplot(p1, p2, nrows = 2, shareX = FALSE) %>%
      layout(xaxis = list(showticklabels = TRUE, title = FALSE)) # Mostrar el eje X solo una vez
  })
  
  
  
  output$mapEcuador <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -78.4678, lat = -1.8312, zoom = 7)
  })
  
  # Add more reactive outputs as needed for plots and other data displays
}

shinyApp(ui, server)
