library(shiny)
library(plotly)
library(readxl)
library(FactoMineR)
# library(factoextra)
# library(shinythemes)

ui <- fluidPage(

  h2("Select data points by dragging the mouse to see more insights"),

  plotlyOutput(outputId = "mdsMap"),
  plotOutput(outputId = "caMap"),
  verbatimTextOutput(outputId = "selectedData")

)

server <- function(input, output, session) {
  output$mdsMap <- renderPlotly({

    positionData <- read_excel("/Users/tharindu/downloads/projects/research/R/positionData.xlsx")
    pos <- subset(positionData, select = c("latitude", "longitude"))
    rownames(pos) <- positionData$empName
    View(pos)

    distMat <- dist(pos, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

    loc <- cmdscale(distMat)
    colnames(loc) <- c("dim_1", "dim_2")
    loc <- as.data.frame(loc)
    View(loc)
    key <- row.names(loc)
    plot_ly(loc, x = ~dim_1, y = ~dim_2, type = "scatter", color = rownames(loc),
    marker = list(size = 10), key = ~key) %>%
        layout(title = "MDS Map",
               xaxis = list(title = "Dimension 1"),
               yaxis = list(title = "Dimension 2"),
               dragmode =  "select",
               plot_bgcolor = "6A446F")
  })

  output$caMap <- renderPlot({

    # Get subset based on selection
    event.data <- event_data("plotly_selected")

    # If NULL dont do anything
    # if(is.null(event.data) == T) "return(NULL)" else event.data$key

    caData <- read_excel("/Users/tharindu/downloads/projects/research/R/caData.xlsx")
    data <- subset(caData, select = c("seat", "meeting room", "cafeteria", "home", "lobby"))
    rownames(data) <- caData$X__1
    rows <- c(event.data$key)
    View(rows)
    # View(event.data$key)
    dataSelected <- subset(data, rownames(data) %in% rows)
    rownames(dataSelected) <- rows
    View(dataSelected)
    res.ca <- CA(dataSelected, graph = FALSE)
    plot(res.ca)
    # fviz_ca_biplot(res.ca, repel = FALSE)
  })

  output$selectedData <- renderPrint({

    # Get subset based on selection
    event.data <- event_data("plotly_selected")

    # If NULL dont do anything
    if(is.null(event.data) == T) "return(NULL)" else event.data$key
  })

}

shinyApp(ui = ui, server = server)
