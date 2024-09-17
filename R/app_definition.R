#' @import shiny
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom ggplot2 ggplot geom_polygon geom_point geom_line aes scale_y_reverse scale_color_manual scale_shape_manual scale_linetype_manual labs theme_minimal theme guides guide_legend
#' @importFrom colourpicker colourInput
#' @importFrom stats median
#' @importFrom utils data
#' @importFrom Rcpp sourceCpp
#' @useDynLib CullenFreyX, .registration = TRUE
NULL

ui <- fluidPage(
  titlePanel("Interactive Cullen-Frey Graph"),

  fluidRow(
    column(6,
           selectInput("SelectDataset", "Select a Dataset:",
                       choices = c("None", "Dataset 1", "Dataset 2"),
                       selected = "None"))
  ),

  fluidRow(
    column(6, div(style = "display: inline-block; width: 60%; vertical-align:top;",
                  selectInput("bootstrapMethod", "Choose a Bootstrap Method:",
                              choices = c("None", "Bootstrap Samples", "Bootstrap Unbiased"),
                              selected = "None")),
           div(style = "display: inline-block; width: 30%; vertical-align:top;",
               numericInput("numSamples", "Number of Samples:",
                            value = 1000, min = 10, step = 50))
    )
  ),

  hr(),

  sidebarLayout(
    sidebarPanel(
      selectInput("distributionSelect", "Select a Distribution:",
                  choices = c("All", "Beta Distribution", "Lognormal", "Gamma", "Observed", "Normal", "Uniform", "Logistic", "Exponential"),
                  selected = "All"),
      colourInput("colorPicker", "Choose Distribution Color:", value = "darkgreen"),
      tags$div(
        style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 20px;",
        h4("Colorblind Friendly Palette"),
        fluidRow(
          column(12, div(
            style = "display: flex; justify-content: space-between; margin-bottom: 15px;",  # Espacio vertical añadido
            actionButton("color_cud1", "", style = "background-color: #D55E00; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud2", "", style = "background-color: #0072B2; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud3", "", style = "background-color: #009E73; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud4", "", style = "background-color: #F0E442; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud5", "", style = "background-color: #CC79A7; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud6", "", style = "background-color: #56B4E9; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud7", "", style = "background-color: #E69F00; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud8", "", style = "background-color: #999999; border-radius: 50%; width: 30px; height: 30px; border: none;")
          ))
        ),
        fluidRow(
          column(12, div(
            style = "display: flex; justify-content: space-between;",
            actionButton("color_cud9", "", style = "background-color: #4B0082; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud10", "", style = "background-color: #FF4500; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud11", "", style = "background-color: #32CD32; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud12", "", style = "background-color: #FF1493; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud13", "", style = "background-color: #FFD700; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud14", "", style = "background-color: #8A2BE2; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud15", "", style = "background-color: #00CED1; border-radius: 50%; width: 30px; height: 30px; border: none;"),
            actionButton("color_cud16", "", style = "background-color: #FF69B4; border-radius: 50%; width: 30px; height: 30px; border: none;")
          ))
        )
      ),



      textInput("tooltipText", "Insert a comment:", " "),

      fluidRow(
        column(6, actionButton("saveBtn", "Save")),
        column(6, actionButton("clearBtn", "Clear"))
      ),

      br(),
      actionButton("toggleComments", "+ Show Comments"),
      br(),
      br(),
      uiOutput("commentBox")
    ),

    mainPanel(
      plotlyOutput("distPlot", height = "500px")
    )
  )
)
server <- function(input, output, session) {

  selected_color <- reactiveVal("#FF5733")

  # Change the distributions color
  observeEvent(input$colorPicker, {
    selected_color(input$colorPicker)
  })

  # Color friendly colors
  observeEvent(input$color_cud1, { selected_color("#D55E00") })
  observeEvent(input$color_cud2, { selected_color("#0072B2") })
  observeEvent(input$color_cud3, { selected_color("#009E73") })
  observeEvent(input$color_cud4, { selected_color("#F0E442") })
  observeEvent(input$color_cud5, { selected_color("#CC79A7") })
  observeEvent(input$color_cud6, { selected_color("#56B4E9") })
  observeEvent(input$color_cud7, { selected_color("#E69F00") })
  observeEvent(input$color_cud8, { selected_color("#999999") })
  observeEvent(input$color_cud9, { selected_color("#4B0082") })
  observeEvent(input$color_cud10, { selected_color("#FF4500") })
  observeEvent(input$color_cud11, { selected_color("#32CD32") })
  observeEvent(input$color_cud12, { selected_color("#FF1493") })
  observeEvent(input$color_cud13, { selected_color("#FFD700") })
  observeEvent(input$color_cud14, { selected_color("#8A2BE2") })
  observeEvent(input$color_cud15, { selected_color("#00CED1") })
  observeEvent(input$color_cud16, { selected_color("#FF69B4") })

  # Guardar y manejar tooltips personalizados
  tooltip_texts <- reactiveVal(list(
    "Beta Distribution" = c(),
    "Lognormal" = c(),
    "Gamma" = c(),
    "Observed" = c(),
    "Normal" = c(),
    "Uniform" = c(),
    "Logistic" = c(),
    "Exponential" = c()
  ))

  commentsVisible <- reactiveVal(FALSE)

  observeEvent(input$saveBtn, {
    current_tooltips <- tooltip_texts()
    current_tooltips[[input$distributionSelect]] <- c(current_tooltips[[input$distributionSelect]], input$tooltipText)
    tooltip_texts(current_tooltips)
  })

  observeEvent(input$clearBtn, {
    updateTextInput(session, "tooltipText", value = "")
  })

  observeEvent(input$toggleComments, {
    if (commentsVisible()) {
      commentsVisible(FALSE)
      updateActionButton(session, "toggleComments", label = "+ Show Comments")
    } else {
      commentsVisible(TRUE)
      updateActionButton(session, "toggleComments", label = "- Hide Comments")
    }
  })

  # Show stored comments
  output$commentBox <- renderUI({
    if (commentsVisible()) {
      all_comments <- tooltip_texts()
      comment_list <- list()
      for (dist in names(all_comments)) {
        dist_comments <- all_comments[[dist]]
        if (length(dist_comments) > 0) {
          comment_list <- append(comment_list, paste("<strong>Comments for", dist, ":</strong><br>",
                                                     paste(seq_along(dist_comments), dist_comments, sep = ". ", collapse = "<br>"), "<br><br>"))
        }
      }

      if (length(comment_list) == 0) {
        HTML("No comments available.")
      } else {
        HTML(paste(comment_list, collapse = "<br>"))
      }
    } else {
      NULL
    }
  })

  # Reactive function to handle the bootsrap data according to the selected number of samples
  bootstrap_data <- reactive({
    num_samples <- input$numSamples

    if (input$bootstrapMethod == "Bootstrap Samples") {
      bootstrap_results <- bootstrap_method(data, num_samples, "sample", TRUE)
      bootstrap_results <- data.frame(squared_skewness = bootstrap_results$skewness^2, kurtosis = bootstrap_results$kurtosis)
      return(bootstrap_results)
    } else if (input$bootstrapMethod == "Bootstrap Unbiased") {
      bootstrap_results_unbiased <- bootstrap_method(data, num_samples, "unbiased", TRUE)
      bootstrap_results_unbiased <- data.frame(squared_skewness = bootstrap_results_unbiased$skewness^2, kurtosis = bootstrap_results_unbiased$kurtosis)
      return(bootstrap_results_unbiased)
    } else {
      return(NULL)
    }
  })

  output$distPlot <- renderPlotly({
    color_map <- c(
      "Beta Distribution" = ifelse(input$distributionSelect == "Beta Distribution", selected_color(), "lightgrey"),
      "Observed" = ifelse(input$distributionSelect == "Observed", selected_color(), "blue"),
      "Normal" = ifelse(input$distributionSelect == "Normal", selected_color(), "red"),
      "Uniform" = ifelse(input$distributionSelect == "Uniform", selected_color(), "black"),
      "Logistic" = ifelse(input$distributionSelect == "Logistic", selected_color(), "green"),
      "Exponential" = ifelse(input$distributionSelect == "Exponential", selected_color(), "purple"),
      "Lognormal" = ifelse(input$distributionSelect == "Lognormal", selected_color(), "orange"),
      "Gamma" = ifelse(input$distributionSelect == "Gamma", selected_color(), "brown"),
      "Bootstrap Samples" = "magenta",
      "Bootstrap Unbiased" = "lightblue"
    )

    # Construir el gráfico de ggplot
    p <- ggplot() +
      geom_polygon(data = data_filtered, aes(x = s2, y = y, color = "Beta Distribution"), alpha = 0.5) +
      geom_point(data = theoretical_points, aes(x = skewness_squared, y = kurtosis, color = label, shape = label), size = 3) +
      geom_line(data = lognormal_line, aes(x = skewness_squared, y = kurtosis, color = "Lognormal", linetype = "Lognormal"), size = 1) +
      geom_line(data = gamma_line, aes(x = skewness_squared, y = kurtosis, color = "Gamma", linetype = "Gamma"), size = 1)

    if (!is.null(bootstrap_data())) {
      p <- p + geom_point(data = bootstrap_data(), aes(x = squared_skewness, y = kurtosis, color = input$bootstrapMethod), size = 2, alpha = 0.6)
    }


    p <- p + scale_y_reverse(limits = c(10, 0)) +
      scale_color_manual(values = color_map) +
      scale_shape_manual(values = c("Observed" = 1, "Normal" = 8, "Uniform" = 2, "Logistic" = 3, "Exponential" = 7)) +
      scale_linetype_manual(values = c("Lognormal" = "dotted", "Gamma" = "dashed")) +
      labs(x = "Skewness^2", y = "Kurtosis", title = "Skewness^2 vs Kurtosis") +
      theme_minimal() +
      theme(legend.position = "top") +
      guides(
        color = guide_legend(title = "Distributions"),
        shape = "none",
        linetype = "none"
      )

    p_plotly <- ggplotly(p, tooltip = c("x", "y", "color"))
    for (i in seq_along(p_plotly$x$data)) {
      dist_name <- p_plotly$x$data[[i]]$name
      if (!is.null(dist_name)) {
        if (dist_name == input$distributionSelect) {
          p_plotly$x$data[[i]]$text <- paste(
            p_plotly$x$data[[i]]$text, "<br>",
            paste("Current:", input$tooltipText, collapse = "<br>"),
            "Saved Comments:",
            paste(seq_along(tooltip_texts()[[dist_name]]), tooltip_texts()[[dist_name]], sep = ". ", collapse = "<br>")
          )
        } else {
          p_plotly$x$data[[i]]$text <- paste(
            p_plotly$x$data[[i]]$text, "<br>",
            "Saved Comments:",
            paste(seq_along(tooltip_texts()[[dist_name]]), tooltip_texts()[[dist_name]], sep = ". ", collapse = "<br>")
          )
        }
        p_plotly$x$data[[i]]$hoverinfo <- "text"
      }
    }
    return(p_plotly)
  })
}

#' Run the Cullen-Frey Interactive Graph Application
#'
#' This function launches a Shiny web application that allows users to interactively explore
#' the Cullen-Frey graph.
#'
#' @return Launches the Shiny web application.
#' @export
#' @examples
#'\dontrun{
#' run_app()
#' }

run_app <- function() {
  shinyApp(ui = ui, server = server)
}

