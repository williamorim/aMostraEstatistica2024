library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Situação das escolas brasileiras"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "metrica",
        label = "Selecione a métrica",
        choices = c(
          "Água potável" = "agua_potavel",
          "Energia elétrica" = "energia_rede_publica",
          "Internet para alunos" = "internet_alunos"
        )
      )
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output, session) {

  con <- DBI::dbConnect(
    bigrquery::bigquery(),
    project = "basedosdados",
    dataset = "br_inep_censo_escolar",
    billing = "basedosdados-436013"
  )

  output$grafico <- renderPlot({
    dplyr::tbl(con, "escola") |> 
      dplyr::select(metrica = dplyr::any_of(input$metrica), ano) |> 
      dplyr::group_by(ano) |> 
      dplyr::summarise(prop = mean(metrica, na.rm = TRUE)) |> 
      dplyr::collect() |> 
      ggplot() +
      geom_line(aes(x = ano, y = prop)) +
      geom_point(aes(x = ano, y = prop)) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()
  })
  
}

shinyApp(ui, server)