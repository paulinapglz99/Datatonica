install.packages("shiny")
install.packages("shinydashboard")

# Cargar paquetes
library(shiny)
library(shinydashboard)

# Definir el UI (interfaz de usuario)
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Básico"), # Encabezado
  
  dashboardSidebar( # Barra lateral
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-bar")),
      menuItem("Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  
  dashboardBody( # Contenido principal
    tabItems(
      # Pestaña Inicio
      tabItem(tabName = "inicio",
              h2("Bienvenido al Dashboard Básico"),
              p("Este es un ejemplo de Shiny Dashboard.")
      ),
      
      # Pestaña Gráficos
      tabItem(tabName = "graficos",
              h2("Gráfico de ejemplo"),
              plotOutput("grafico_ejemplo") # Espacio para el gráfico
      ),
      
      # Pestaña Datos
      tabItem(tabName = "datos",
              h2("Tabla de datos"),
              tableOutput("tabla_ejemplo") # Espacio para la tabla
      )
    )
  )
)

# Definir el servidor
server <- function(input, output) {
  # Generar un gráfico básico
  output$grafico_ejemplo <- renderPlot({
    x <- rnorm(100)
    hist(x, col = "skyblue", border = "white", main = "Histograma")
  })
  
  # Mostrar una tabla de datos
  output$tabla_ejemplo <- renderTable({
    data.frame(
      Nombre = c("Juan", "Ana", "Luis"),
      Edad = c(25, 30, 35),
      Ciudad = c("Madrid", "Barcelona", "Valencia")
    )
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
