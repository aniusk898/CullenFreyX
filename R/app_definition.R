#' @import shiny
#' @import shinyWidgets
#' @import webshot2
#' @import htmlwidgets
#' @import ggplot2
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom stats median
#' @importFrom utils data
#' @importFrom Rcpp evalCpp
#' @useDynLib CullenFreyX, .registration = TRUE
NULL

#' Run the Cullen-Frey Interactive Graph Application
#'
#' This function launches a Shiny web application that allows users to interactively explore
#' the Cullen-Frey graph. It requires preprocessed data inputs for plotting, which should be
#' provided as arguments to the function.
#'
#' @param data A numeric vector representing the dataset to be analyzed.
#' @return Launches the Shiny web application.
#' @export
run_app <- function(data) {
    ui <- fluidPage(
        # App title
        titlePanel("Interactive Cullen-Frey Graph"),
        uiOutput("dynamicStyle"),
        # Dynamic style adjustments for the app

        # Main input section
        div(
            style = "border: 1px solid #ccc; padding: 15px; margin-bottom: 20px; border-radius: 10px; background-color: #f9f9f9; width: 100%;",

            # Custom CSS for tooltips and slider styles
            tags$head(tags$style(
                HTML(
                    "
                /* Tooltip container */
                .tooltip-container {
                  position: relative;
                  display: inline-block;
                  cursor: pointer;
                }

                /* Tooltip text */
                .tooltip-container .tooltip-text {
                  visibility: hidden;
                  width: 200px;
                  background-color: #555;
                  color: #fff;
                  text-align: center;
                  border-radius: 6px;
                  padding: 5px;
                  position: absolute;
                  z-index: 1;
                  bottom: 100%;
                  left: 50%;
                  margin-left: -100px;
                  opacity: 0;
                  transition: opacity 0.3s;
                }

                /* Show the tooltip text when hovering */
                .tooltip-container:hover .tooltip-text {
                  visibility: visible;
                  opacity: 1;
                }

                /* Slider and button alignment styles */
                .slider-label {
                  display: flex;
                  align-items: center;
                }

                /* Increase the slider size */
                .slider-input {
                  flex-grow: 1;
                  width: 80%;
                }
            "
                )
            )),

            # Row for dataset column selection, data type, bootstrap method, and download buttons
            fluidRow(
              column(
                4,
                # Dataset column and data type selection
                div(
                  style = "display: inline-block; width: 100%; vertical-align:top;",
                  selectInput("selectedColumn", "Select a Dataset:", names(data)),
                ),
                div(
                  style = "display: inline-block; width: 120%; vertical-align:top;",
                  div(
                    style = "display: flex; align-items: center;",
                    h4("Select the Data Type:", style = "margin-right: 10px;"),
                    div(
                        class = "tooltip-container",
                        actionButton("infoFontSize", "?", style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; margin-left: 10px; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"),
                        span(class = "tooltip-text", "Select whether your data is continuous or discrete.")
                    )
                  ),

                  radioButtons(
                    "dataType",
                    "",
                    choices = c("Continuous" = "continuous", "Discrete" = "discrete"),
                    selected = "continuous",
                    inline = TRUE
                  ),

                  div(
                    style = "display: flex; align-items: center;",
                    h4("Select a Method:", style = "margin-right: 10px;"),
                    div(
                    class = "tooltip-container",
                    actionButton("infoFontSize", "?", style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; margin-left: 10px; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"),
                    span(class = "tooltip-text", "Unbiased method corrects for bias, while the sample method calculates directly from the data. Unbiased method is recommended when the dataset is small")
                  )
                  ),

                  radioButtons(
                    "method",
                    "",
                    choices = c("Unbiased" = "unbiased", "Sample" = "sample"),
                    selected = "unbiased",
                    inline = TRUE
                  )
                )
              ),
                column(
                    4,
                    # Bootstrap method and number of samples input
                    div(
                      style = "display: inline-block; width: 100%; vertical-align:top;",
                      div(
                        style = "display: flex; align-items: center;",
                        h4("Choose a Bootstrap Method:", style = "margin-right: 10px;"),
                        div(
                          class = "tooltip-container",
                          actionButton("infoBootstrapMethod", "?",
                                       style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"
                          ),
                          span(
                            class = "tooltip-text",
                            style = "margin-left: 10px;",
                            "Select a bootstrap method to apply resampling. Use 'Bootstrap Unbiased' for small datasets to reduce bias."
                          )
                        )
                      ),
                      selectInput(
                        "bootstrapMethod",
                        "",
                        choices = c("None", "Bootstrap Samples", "Bootstrap Unbiased"),
                        selected = "None"
                      )
                    ),
                    div(
                      style = "display: inline-block; width: 100%; vertical-align:top;",
                      div(
                        style = "display: flex; align-items: center;",
                        h4("Number of Samples:", style = "margin-right: 10px;"),
                        div(
                          class = "tooltip-container",
                          actionButton("infoNumSamples", "?",
                                       style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"
                          ),
                          span(
                            class = "tooltip-text",
                            style = "margin-left: 10px;",
                            "The number of bootstrap samples is capped based on dataset size. For large datasets (>=10,000 points), the maximum is 500 samples. For medium datasets (3,000 to 10,000 points), it is capped at 1000. For small datasets, 1,000 samples are used by default."
                          )
                        )
                      ),
                      numericInput(
                        "numSamples",
                        "",
                        value = 1000,
                        min = 10,
                        step = 50
                      )
                    ),
                    div(
                      id = "sampleNotification",
                      style = "margin-left: 10px; color: red; font-weight: bold;",
                      textOutput("sampleMessage")
                    )
                ),
                column(
                    4,
                    # Download buttons for PNG and PDF
                    br(),
                    div(style = "display: inline-block; width: 100%; vertical-align:top;", downloadButton("downloadPNG", "Download PNG")),
                    br(),
                    br(),
                    div(style = "display: inline-block; width: 100%; vertical-align:top;", downloadButton("downloadReport", "Download PDF"))
                )
            )
        ),

        # Sidebar layout
        sidebarLayout(
            sidebarPanel(
              h3("Customization Panel"),
                # Conditional panel for continuous and discrete distributions
                conditionalPanel(
                    condition = "input.dataType == 'continuous'",
                    selectInput(
                        "distributionSelect",
                        "Select a Continuous Distribution:",
                        choices = c(
                            "All",
                            "Beta Distribution",
                            "Lognormal",
                            "Gamma",
                            "Observed",
                            "Normal",
                            "Uniform",
                            "Logistic",
                            "Exponential",
                            "Bootstrap Samples",
                            "Bootstrap Unbiased"
                        ),
                        selected = "All"
                    )
                ),
                conditionalPanel(
                    condition = "input.dataType == 'discrete'",
                    selectInput(
                        "distributionSelect",
                        "Select a Discrete Distribution:",
                        choices = c(
                            "All",
                            "Observed",
                            "Normal",
                            "NegBin",
                            "Poisson",
                            "Bootstrap Samples",
                            "Bootstrap Unbiased"
                        ),
                        selected = "All"
                    )
                ),

                # Font family selection
                selectInput(
                    "fontFamily",
                    "Choose Font Family:",
                    choices = c(
                        "Arial",
                        "Helvetica",
                        "Times New Roman",
                        "Courier New",
                        "Verdana",
                        "Tahoma",
                        "Georgia",
                        "Trebuchet MS",
                        "Impact",
                        "Comic Sans MS",
                        "Lucida Console",
                        "Palatino Linotype",
                        "Gill Sans",
                        "Century Gothic",
                        "Futura"
                    ),
                    selected = "Arial"
                ),

                # Comment input box
                textInput("tooltipText", "Insert a comment:", " "),

                # Save and clear buttons for comments
                fluidRow(column(6, actionButton(
                    "saveBtn", "Submit"
                )), column(6, actionButton(
                    "clearBtn", "Clear"
                ))),

                br(),

                # Color picker and colorblind-friendly palette
                colourInput(
                    "colorPicker",
                    "Choose Distribution Color:",
                    value = "darkgreen"
                ),

                tags$div(
                    style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 20px;",
                    h4("Colorblind Friendly Palette"),
                    fluidRow(column(
                        12,
                        div(
                            style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                            actionButton("color_cud1", "", style = "background-color: #D55E00; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud2", "", style = "background-color: #0072B2; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud3", "", style = "background-color: #009E73; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud4", "", style = "background-color: #F0E442; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud5", "", style = "background-color: #CC79A7; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud6", "", style = "background-color: #56B4E9; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud7", "", style = "background-color: #E69F00; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud8", "", style = "background-color: #999999; border-radius: 50%; width: 30px; height: 30px; border: none;")
                        )
                    )),
                    fluidRow(column(
                        12,
                        div(
                            style = "display: flex; justify-content: space-between;",
                            actionButton("color_cud9", "", style = "background-color: #4B0082; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud10", "", style = "background-color: #FF4500; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud11", "", style = "background-color: #32CD32; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud12", "", style = "background-color: #FF1493; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud13", "", style = "background-color: #FFD700; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud14", "", style = "background-color: #8A2BE2; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud15", "", style = "background-color: #00CED1; border-radius: 50%; width: 30px; height: 30px; border: none;"),
                            actionButton("color_cud16", "", style = "background-color: #FF69B4; border-radius: 50%; width: 30px; height: 30px; border: none;")
                        )
                    ))
                ),



                # Sliders for point size, transparency, and font size
                div(
                    class = "slider-label",
                    div(
                        class = "slider-input",
                        sliderInput(
                            "pointSize",
                            "Point and line Size:",
                            min = 1,
                            max = 10,
                            value = 3,
                            step = 1
                        )
                    ),
                    div(
                        class = "tooltip-container",
                        actionButton("infoPointSize", "?", style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; margin-left: 10px; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"),
                        span(
                            class = "tooltip-text",
                            "Adjust the size of the points displayed in the graph. Larger values increase the size of the points."
                        )
                    )
                ),
                div(
                    class = "slider-label",
                    div(
                        class = "slider-input",
                        sliderInput(
                            "pointAlpha",
                            "Point and line Transparency:",
                            min = 0.1,
                            max = 1,
                            value = 1,
                            step = 0.1
                        )
                    ),
                    div(
                        class = "tooltip-container",
                        actionButton("infoPointAlpha", "?", style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; margin-left: 10px; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"),
                        span(
                            class = "tooltip-text",
                            "Adjust the transparency of the points in the graph. Lower values make points more transparent."
                        )
                    )
                ),
                div(
                    class = "slider-label",
                    div(
                        class = "slider-input",
                        sliderInput(
                            "fontSize",
                            "Font Size (for plot):",
                            min = 8,
                            max = 30,
                            value = 12,
                            step = 1
                        )
                    ),
                    div(
                        class = "tooltip-container",
                        actionButton("infoFontSize", "?", style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; margin-left: 10px; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"),
                        span(
                            class = "tooltip-text",
                            "Adjust the font size for the labels and text in the graph."
                        )
                    )
                ),

                # Text size slider for the app
                div(
                    class = "slider-label",
                    div(
                        class = "slider-input",
                        sliderTextInput(
                            "textSizeSlider",
                            "Choose Text Size:",
                            choices = c("Small", "Medium", "Large"),
                            selected = "Small",
                            grid = TRUE
                        )
                    ),
                    div(
                        class = "tooltip-container",
                        actionButton("infoFontSize", "?", style = "background-color: grey; color: white; border-radius: 100%; width: 25px; height: 25px; border: none; margin-left: 10px; font-size: 14px; line-height: 25px; text-align: center; padding: 0;"),
                        span(class = "tooltip-text", "Adjust the font size for the text in the app.")
                    )
                ),
                # Save color button
                actionButton("saveColorBtn", "Save Changes"),
            ),


            # Main panel for plot output and statistics/comments sections
            mainPanel(
                div(
                    style = "border: 1px solid #ccc; padding: 15px; margin-bottom: 20px; border-radius: 10px; background-color: #f9f9f9;",
                    plotlyOutput("distPlot", height = "650px"),
                    # Plot output
                    br(),
                    # Toggle for showing/hiding statistics
                    actionButton("toggleStatistics", "+ Show Statistics"),
                    tags$div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 20px;", uiOutput("statisticsPanel")),
                    # Toggle for showing/hiding comments
                    actionButton("toggleComments", "+ Show Comments"),
                    tags$div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 20px;", uiOutput("commentsPanel"))
                )
            )
        )
    )


    server <- function(input, output, session) {

        # Reactive values to store selected color, point size, and transparency (alpha)
        selected_color <- reactiveVal("#FF5733")
        selected_point_size <- reactiveVal(3)
        selected_point_alpha <- reactiveVal(1)  # Default alpha value


        # Reactive values to store saved settings for continuous and discrete distributions
        saved_colors_continuous <- reactiveVal(list())
        saved_point_sizes_continuous <- reactiveVal(list())
        saved_alpha_continuous <- reactiveVal(list())

        saved_colors_discrete <- reactiveVal(list())
        saved_point_sizes_discrete <- reactiveVal(list())
        saved_alpha_discrete <- reactiveVal(list())

        saved_line_sizes_continuous <- reactiveVal(list())
        saved_line_sizes_discrete <- reactiveVal(list())

        saved_line_alpha_continuous <- reactiveVal(list())
        saved_line_alpha_discrete <- reactiveVal(list())

        # Reactive expression for handling selected column data
        reactiveData <- reactive({
            req(input$selectedColumn)

            selected_data <- data[[input$selectedColumn]]

            # Ensure the data is not empty
            if (length(selected_data) == 0) {
              stop("The selected dataset is empty.")
            }

            # Ensure the data has at least 4 observations
            if (length(selected_data) < 4) {
              stop("The dataset must contain at least 4 data points for analysis.")
            }

            # Try to convert the data to numeric and handle errors
            selected_data <- tryCatch({
              as.numeric(selected_data)
            }, warning = function(w) {
              stop("Warning: Unable to convert input to numeric.")
            }, error = function(e) {
              stop("Error: The selected data column contains non-numeric values.")
            })

            # Check for NA or infinite values after conversion
            if (any(is.na(selected_data))) {
              stop("The dataset contains missing (NA) values. Please clean the data before analysis.")
            }

            if (any(!is.finite(selected_data))) {
              stop("The dataset contains infinite values. Please handle them before analysis.")
            }

            # Check for identical values (e.g., all values are the same)
            if (all(selected_data == selected_data[1])) {
              stop("The dataset contains identical values, which may cause problems in statistical analysis.")
            }


            # Return the validated data
            selected_data
        })

        reactiveStatistics <- reactive({
          req(reactiveData())

          # Get the method chosen by the user ("sample" or "unbiased")
          method_selected <- input$method  # Make the method reactive

          # Calculate statistics based on the chosen method
          stats_test <- calculate_statistics(reactiveData(), method = method_selected)

          stats_test  # Return the calculated statistics
        })

        # Dynamic styling based on user-selected text size
        output$dynamicStyle <- renderUI({
            sizeMapping <- list(
                "Small" = list(
                    appTitle = "16px",
                    buttonText = "14px",
                    commentText = "12px"
                ),
                "Medium" = list(
                    appTitle = "24px",
                    buttonText = "18px",
                    commentText = "16px"
                ),
                "Large" = list(
                    appTitle = "32px",
                    buttonText = "22px",
                    commentText = "20px"
                )
            )

            selectedSize <- sizeMapping[[input$textSizeSlider]]  # Get selected size

            tags$style(HTML(
                sprintf(
                    "
      /* General styles */
      h1 { font-size: %s; }
      .btn { font-size: %s; }
      #commentBox { font-size: %s; }
      label { font-size: %s; }
      .selectize-input { font-size: %s; }
      .control-label { font-size: %s; }

      /* Specific styles for statistics and comments panels */
      #statisticsPanel, #commentsPanel { font-size: %spx; font-family: %s; }
    ",
                    selectedSize$appTitle,
                    selectedSize$buttonText,
                    selectedSize$commentText,
                    selectedSize$buttonText,
                    selectedSize$buttonText,
                    selectedSize$buttonText,
                    input$fontSize,
                    input$fontFamily
                )
            ))
        })

        # Reactive value to store selected data type information
        data_info <- reactiveVal()

        # Observe event when selected column or method changes
        observe({
          req(reactiveData())

          method_selected <- input$method
          data_type_selected <- input$dataType

          if (data_type_selected == "continuous") {
            data_info(data_type.continuous(reactiveData(), method = method_selected))
          } else if (data_type_selected == "discrete") {
            data_info(data_type.discrete(reactiveData(), method = method_selected))
          }
        })

        observeEvent({
          input$dataType
          input$method
        }, {
          req(reactiveData())

          data_type_selected <- input$dataType
          method_selected <- input$method

          if (data_type_selected == "continuous") {
            data_info(data_type.continuous(reactiveData(), method = method_selected))
          } else if (data_type_selected == "discrete") {
            data_info(data_type.discrete(reactiveData(), method = method_selected))
          }
        })

        # Tooltip handling
        tooltip_texts <- reactiveVal(list())

        # Toggle visibility for comments and statistics panels
        commentsVisible <- reactiveVal(FALSE)
        statisticsVisible <- reactiveVal(FALSE)

        # Update selected color, point size, and alpha on change
        observeEvent(input$colorPicker, {
            selected_color(input$colorPicker)
        })

        observeEvent(input$pointSize, {
            selected_point_size(input$pointSize)
        })

        observeEvent(input$pointAlpha, {
            selected_point_alpha(input$pointAlpha)
        })

        # Save settings (color, point size, transparency) for the selected distribution
        observeEvent(input$saveColorBtn, {
            if (input$dataType == "continuous") {
                saved_colors_continuous(
                    update_saved_colors(
                        saved_colors_continuous(),
                        input$distributionSelect,
                        selected_color()
                    )
                )
                saved_point_sizes_continuous(
                    update_saved_point_sizes(
                        saved_point_sizes_continuous(),
                        input$distributionSelect,
                        selected_point_size()
                    )
                )
                saved_alpha_continuous(
                    update_saved_alpha(
                        saved_alpha_continuous(),
                        input$distributionSelect,
                        selected_point_alpha()
                    )
                )
                saved_line_sizes_continuous(
                    update_saved_line_sizes(
                        saved_line_sizes_continuous(),
                        input$distributionSelect,
                        input$pointSize
                    )
                )
                saved_line_alpha_continuous(
                    update_saved_alpha(
                        saved_line_alpha_continuous(),
                        input$distributionSelect,
                        input$pointAlpha
                    )
                )
            } else {
                saved_colors_discrete(
                    update_saved_colors(
                        saved_colors_discrete(),
                        input$distributionSelect,
                        selected_color()
                    )
                )
                saved_point_sizes_discrete(
                    update_saved_point_sizes(
                        saved_point_sizes_discrete(),
                        input$distributionSelect,
                        selected_point_size()
                    )
                )
                saved_alpha_discrete(
                    update_saved_alpha(
                        saved_alpha_discrete(),
                        input$distributionSelect,
                        selected_point_alpha()
                    )
                )
                saved_line_sizes_discrete(
                    update_saved_line_sizes(
                        saved_line_sizes_discrete(),
                        input$distributionSelect,
                        input$pointSize
                    )
                )
                saved_line_alpha_discrete(
                    update_saved_alpha(
                        saved_line_alpha_discrete(),
                        input$distributionSelect,
                        input$pointAlpha
                    )
                )
            }
        })


        # Helper functions for saving colors, point sizes, and transparency
        update_saved_colors <- function(saved_colors,
                                        dist_name,
                                        new_color) {
            saved_colors[[dist_name]] <- new_color
            return(saved_colors)
        }

        update_saved_point_sizes <- function(saved_point_sizes,
                                             dist_name,
                                             new_point_size) {
            saved_point_sizes[[dist_name]] <- new_point_size
            return(saved_point_sizes)
        }

        update_saved_alpha <- function(saved_alpha,
                                       dist_name,
                                       new_alpha) {
            saved_alpha[[dist_name]] <- new_alpha
            return(saved_alpha)
        }

        update_saved_line_sizes <- function(saved_line_sizes, dist_name, new_line_size) {
            saved_line_sizes[[dist_name]] <- new_line_size
            return(saved_line_sizes)
        }

        update_saved_alpha <- function(saved_alpha, dist_name, new_alpha) {
            saved_alpha[[dist_name]] <- new_alpha
            return(saved_alpha)
        }


        # Colorblind-friendly color selection buttons
        observeEvent(input$color_cud1, {
            selected_color("#D55E00")
        })
        observeEvent(input$color_cud2, {
            selected_color("#0072B2")
        })
        observeEvent(input$color_cud3, {
            selected_color("#009E73")
        })
        observeEvent(input$color_cud4, {
            selected_color("#F0E442")
        })
        observeEvent(input$color_cud5, {
            selected_color("#CC79A7")
        })
        observeEvent(input$color_cud6, {
            selected_color("#56B4E9")
        })
        observeEvent(input$color_cud7, {
            selected_color("#E69F00")
        })
        observeEvent(input$color_cud8, {
            selected_color("#999999")
        })
        observeEvent(input$color_cud9, {
            selected_color("#4B0082")
        })
        observeEvent(input$color_cud10, {
            selected_color("#FF4500")
        })
        observeEvent(input$color_cud11, {
            selected_color("#32CD32")
        })
        observeEvent(input$color_cud12, {
            selected_color("#FF1493")
        })
        observeEvent(input$color_cud13, {
            selected_color("#FFD700")
        })
        observeEvent(input$color_cud14, {
            selected_color("#8A2BE2")
        })
        observeEvent(input$color_cud15, {
            selected_color("#00CED1")
        })
        observeEvent(input$color_cud16, {
            selected_color("#FF69B4")
        })

        # Save and manage custom tooltips
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

        comments_generator <- function(all_comments) {
            comment_list <- list()
            for (dist in names(all_comments)) {
                dist_comments <- all_comments[[dist]]
                if (length(dist_comments) > 0) {
                    comments_combined <- paste(dist_comments, collapse = ", ")
                    comment_list[[dist]] <- sprintf("**%s**: %s", dist, comments_combined)
                }
            }
            return(comment_list)
        }

        output$commentsPanel <- renderUI({
            req(reactiveData())
            if (commentsVisible()) {
                all_comments <- tooltip_texts()
                comment_list <- comments_generator(all_comments)
                if (length(comment_list) == 0) {
                    HTML("No comments available.")
                } else {
                    HTML(paste(comment_list, collapse = "<br>"))
                }
            } else {
                NULL
            }
        })

        observeEvent(input$toggleStatistics, {
            statisticsVisible(!statisticsVisible())
            updateActionButton(
                session,
                "toggleStatistics",
                label = ifelse(
                    statisticsVisible(),
                    "- Hide Statistics",
                    "+ Show Statistics"
                )
            )
        })

        output$statisticsPanel <- renderUI({
            req(reactiveData())
            if (statisticsVisible()) {
              # Get the method chosen by the user ("sample" or "unbiased")
              method_selected <- input$method

              # Calculate statistics with the selected method
              stats_test <- calculate_statistics(reactiveData(), method = method_selected)
                HTML(
                    paste(
                        "Statistics:<br>",
                        "Min: ",
                        stats_test$min,
                        "<br>",
                        "Max: ",
                        stats_test$max,
                        "<br>",
                        "Median: ",
                        stats_test$median,
                        "<br>",
                        "Skewness Squared: ",
                        stats_test$skewness_squared,
                        "<br>",
                        "Kurtosis: ",
                        stats_test$kurtosis,
                        "<br>"
                    )
                )
            } else {
                NULL
            }
        })

        observe({
          req(reactiveData())  # Ensure the data is loaded

          # Check the size of the dataset
          dataset_size <- length(reactiveData())

          # Set the default number of bootstrap samples based on dataset size
          if (dataset_size >= 100000) {
            default_samples <- 100  # Limit to 500 for very large datasets
            message <- "The number of samples should be limited to 500 due to dataset size."
          } else if (dataset_size >= 30000) {
            default_samples <- 200  # Limit to 1000 for medium-sized datasets
            message <- "The number of samples should be limited to 1000 due to dataset size."
          } else {
            default_samples <- 1000  # Keep 1000 for small datasets
            message <- " "
          }

          # Update the default number of samples in the numericInput
          updateNumericInput(session, "numSamples", value = default_samples)

          # Send the notification message to be displayed in the UI
          output$sampleMessage <- renderText({
            message
          })
        })

        # Reactive function to handle bootstrap data
        bootstrap_data <- reactive({
          req(reactiveData())

          # Get the number of samples input by the user
          num_samples <- input$numSamples

          if(num_samples< 10){
            stop("Bootstrap numsamples must be at least 10")
          }

          # Validation logic: ensure the number of samples is limited if the dataset is large
          dataset_size <- length(reactiveData())

          if (dataset_size >= 100000) {
            num_samples <- min(500, num_samples)  # Limit to 500 samples for large datasets
          } else if (dataset_size >= 50000) {
            num_samples <- min(1000, num_samples)  # Limit to 1000 samples for medium datasets
          }

          # Perform bootstrap based on the selected method
          if (input$bootstrapMethod == "Bootstrap Samples") {
            bootstrap_results <- bootstrap_method(reactiveData(), num_samples, "sample", TRUE)
            bootstrap_results <- data.frame(
              squared_skewness = bootstrap_results$skewness ^ 2,
              kurtosis = bootstrap_results$kurtosis
            )
            return(bootstrap_results)
          } else if (input$bootstrapMethod == "Bootstrap Unbiased") {
            bootstrap_results_unbiased <- bootstrap_method(reactiveData(), num_samples, "unbiased", TRUE)
            bootstrap_results_unbiased <- data.frame(
              squared_skewness = bootstrap_results_unbiased$skewness ^ 2,
              kurtosis = bootstrap_results_unbiased$kurtosis
            )
            return(bootstrap_results_unbiased)
          } else {
            return(NULL)
          }
        })
        # Restore saved settings (color, size, alpha) for the selected distribution
        observeEvent(input$distributionSelect, {
            if (input$dataType == "continuous") {
                saved_size <- saved_point_sizes_continuous()[[input$distributionSelect]] %||% 3
                saved_alpha <- saved_alpha_continuous()[[input$distributionSelect]] %||% 1
                saved_color <- saved_colors_continuous()[[input$distributionSelect]] %||% "#E83209"
                saved_line_size <- saved_line_sizes_continuous()[[input$distributionSelect]] %||% 1
                saved_line_alpha <- saved_line_alpha_continuous()[[input$distributionSelect]] %||% 1
            } else {
                saved_size <- saved_point_sizes_discrete()[[input$distributionSelect]] %||% 3
                saved_alpha <- saved_alpha_discrete()[[input$distributionSelect]] %||% 1
                saved_color <- saved_colors_discrete()[[input$distributionSelect]] %||% "#E83209"
                saved_line_size <- saved_line_sizes_discrete()[[input$distributionSelect]] %||% 1
                saved_line_alpha <- saved_line_alpha_discrete()[[input$distributionSelect]] %||% 1
            }

            updateSliderInput(session, "pointSize", value = saved_size)
            updateSliderInput(session, "pointAlpha", value = saved_alpha)
            updateColourInput(session, "colorPicker", value = saved_color)
            updateSliderInput(session, "lineSize", value = saved_line_size)
            updateSliderInput(session, "lineAlpha", value = saved_line_alpha)
        })



        # Reactive plot generation
        thePlot <- reactive({
            if (!is.null(data_info())) {
                req(reactiveData())
                req(reactiveStatistics())


                polygon_data <- data_info()$polygon_data
                theoretical_points <- data_info()$theoretical_points
                xmax <- max(4, ceiling(theoretical_points$skewness_squared[1]))
                ymax <- max(10, ceiling(theoretical_points$kurtosis[1]))


                if (input$dataType == "continuous") {
                    color_map <- saved_colors_continuous()
                    point_size_map <- saved_point_sizes_continuous()
                    point_alpha_map <- saved_alpha_continuous()
                } else {
                    color_map <- saved_colors_discrete()
                    point_size_map <- saved_point_sizes_discrete()
                    point_alpha_map <- saved_alpha_discrete()
                }

                size_map <- sapply(theoretical_points$label, function(dist) {
                    if (dist == input$distributionSelect) {
                        input$pointSize  # Real-time slider value
                    } else {
                        point_size_map[[dist]] %||% 3  # Saved size or default
                    }
                })

                alpha_map <- sapply(theoretical_points$label, function(dist) {
                    if (dist == input$distributionSelect) {
                        input$pointAlpha  # Real-time slider value
                    } else {
                        point_alpha_map[[dist]] %||% 1  # Saved alpha or default
                    }
                })

                # Continuous distribution plot
                if (input$dataType == "continuous") {
                    lognormal_line <- data_info()$lognormal_line
                    gamma_line <- data_info()$gamma_line

                    line_size_map <- c(
                        "Lognormal" = ifelse(input$distributionSelect == "Lognormal", input$pointSize, saved_line_sizes_continuous()[["Lognormal"]] %||% 1),
                        "Gamma" = ifelse(input$distributionSelect == "Gamma", input$pointSize, saved_line_sizes_continuous()[["Gamma"]] %||% 1),
                        "Beta Distribution" = ifelse(input$distributionSelect == "Beta Distribution", input$pointSize, saved_line_sizes_continuous()[["Beta Distribution"]] %||% 1)
                    )

                    line_alpha_map <- c(
                        "Lognormal" = ifelse(input$distributionSelect == "Lognormal", input$pointAlpha, saved_line_alpha_continuous()[["Lognormal"]] %||% 1),
                        "Gamma" = ifelse(input$distributionSelect == "Gamma", input$pointAlpha, saved_line_alpha_continuous()[["Gamma"]] %||% 1),
                        "Beta Distribution" = ifelse(input$distributionSelect == "Beta Distribution", input$pointAlpha, saved_line_alpha_continuous()[["Beta Distribution"]] %||% 0.3)
                    )

                    color_map <- c(
                        "Beta Distribution" = ifelse(
                            input$distributionSelect == "Beta Distribution",
                            selected_color(),
                            color_map[["Beta Distribution"]] %||% "lightgrey"
                        ),
                        "Observed" = ifelse(
                            input$distributionSelect == "Observed",
                            selected_color(),
                            color_map[["Observed"]] %||% "blue"
                        ),
                        "Normal" = ifelse(
                            input$distributionSelect == "Normal",
                            selected_color(),
                            color_map[["Normal"]] %||% "pink"
                        ),
                        "Uniform" = ifelse(
                            input$distributionSelect == "Uniform",
                            selected_color(),
                            color_map[["Uniform"]] %||% "black"
                        ),
                        "Logistic" = ifelse(
                            input$distributionSelect == "Logistic",
                            selected_color(),
                            color_map[["Logistic"]] %||% "green"
                        ),
                        "Exponential" = ifelse(
                            input$distributionSelect == "Exponential",
                            selected_color(),
                            color_map[["Exponential"]] %||% "purple"
                        ),
                        "Lognormal" = ifelse(
                            input$distributionSelect == "Lognormal",
                            selected_color(),
                            color_map[["Lognormal"]] %||% "orange"
                        ),
                        "Gamma" = ifelse(
                            input$distributionSelect == "Gamma",
                            selected_color(),
                            color_map[["Gamma"]] %||% "brown"
                        ),
                        "Bootstrap Samples" = ifelse(
                            input$distributionSelect == "Bootstrap Samples",
                            selected_color(),
                            color_map[["Bootstrap Samples"]] %||% "magenta"
                        ),
                        "Bootstrap Unbiased" = ifelse(
                            input$distributionSelect == "Bootstrap Unbiased",
                            selected_color(),
                            color_map[["Bootstrap Unbiased"]] %||% "lightblue"
                        )
                    )

                    p <- ggplot() +
                        geom_polygon(
                            data = polygon_data,
                            aes(
                                x = s2,
                                y = y,
                                color = "Beta Distribution"
                            ),
                            alpha = line_alpha_map[["Beta Distribution"]],
                            linewidth = line_size_map[["Beta Distribution"]]
                        ) +
                        geom_line(
                            data = lognormal_line,
                            aes(
                                x = skewness_squared,
                                y = kurtosis,
                                color = "Lognormal",
                                linetype = "Lognormal"
                            ),
                            linewidth = line_size_map[["Lognormal"]],
                            alpha = line_alpha_map[["Lognormal"]]
                        ) +
                        geom_line(
                            data = gamma_line,
                            aes(
                                x = skewness_squared,
                                y = kurtosis,
                                color = "Gamma",
                                linetype = "Gamma"
                            ),
                            linewidth = line_size_map[["Gamma"]],
                            alpha = line_alpha_map[["Gamma"]]
                        )  +
                        scale_shape_manual(
                            values = c(
                                "Observed" = 1,
                                "Normal" = 8,
                                "Uniform" = 2,
                                "Logistic" = 3,
                                "Exponential" = 7
                            )
                        ) +
                        scale_linetype_manual(values = c(
                            "Lognormal" = "dotted",
                            "Gamma" = "dashed"
                        )) +
                        labs(x = "Squared Skewness",
                             y = "Kurtosis",
                             title = "Cullen and Frey Graph") +
                        theme_minimal(base_size = input$fontSize)
                } else {
                    # Discrete distribution plot
                    poisson_line <- data_info()$poisson_line

                    # Line size map
                    line_size_map <- c(
                        "Poisson" = ifelse(input$distributionSelect == "Poisson", input$pointSize, saved_line_sizes_discrete()[["Poisson"]] %||% 1),
                        "NegBin" = ifelse(input$distributionSelect == "NegBin", input$pointSize, saved_line_sizes_discrete()[["NegBin"]] %||% 1)
                    )

                    # Transparency map
                    line_alpha_map <- c(
                        "Poisson" = ifelse(input$distributionSelect == "Poisson", input$pointAlpha, saved_line_alpha_discrete()[["Poisson"]] %||% 1),
                        "NegBin" = ifelse(input$distributionSelect == "NegBin", input$pointAlpha, 0.3)  # Valor predeterminado 0.3
                    )

                    # Mapa de colores
                    color_map <- c(
                        "NegBin" = ifelse(
                            input$distributionSelect == "NegBin",
                            selected_color(),
                            saved_colors_discrete()[["NegBin"]] %||% "lightgrey"
                        ),
                        "Observed" = ifelse(
                            input$distributionSelect == "Observed",
                            selected_color(),
                            saved_colors_discrete()[["Observed"]] %||% "blue"
                        ),
                        "Normal" = ifelse(
                            input$distributionSelect == "Normal",
                            selected_color(),
                            saved_colors_discrete()[["Normal"]] %||% "pink"
                        ),
                        "Poisson" = ifelse(
                            input$distributionSelect == "Poisson",
                            selected_color(),
                            saved_colors_discrete()[["Poisson"]] %||% "black"
                        ),
                        "Bootstrap Samples" = ifelse(
                            input$distributionSelect == "Bootstrap Samples",
                            selected_color(),
                            saved_colors_discrete()[["Bootstrap Samples"]] %||% "magenta"
                        ),
                        "Bootstrap Unbiased" = ifelse(
                            input$distributionSelect == "Bootstrap Unbiased",
                            selected_color(),
                            saved_colors_discrete()[["Bootstrap Unbiased"]] %||% "lightblue"
                        )
                    )

                    # Create the graph
                    p <- ggplot() +
                        geom_polygon(
                            data = polygon_data,
                            aes(
                                x = s2,
                                y = y,
                                color = "NegBin"
                            ),
                            linewidth = line_size_map[["NegBin"]],
                            alpha = line_alpha_map[["NegBin"]]
                        ) +
                        geom_line(
                            data = poisson_line,
                            aes(
                                x = skewness_squared,
                                y = kurtosis,
                                color = "Poisson",
                                linetype = "Poisson"
                            ),
                            linewidth = line_size_map[["Poisson"]],
                            alpha = line_alpha_map[["Poisson"]]
                        ) +
                        scale_shape_manual(values = c(
                            "Observed" = 1,
                            "Normal" = 8
                        )) +
                        scale_linetype_manual(values = c("Poisson" = "dotted")) +
                        labs(x = "Squared Skewness",
                             y = "Kurtosis",
                             title = "Cullen and Frey Graph") +
                        theme_minimal(base_size = input$fontSize)

                }

                # Add bootstrap data if available
                if (!is.null(bootstrap_data())) {
                    p <- p + geom_point(
                        data = bootstrap_data(),
                        aes(
                            x = squared_skewness,
                            y = kurtosis,
                            color = input$bootstrapMethod
                        ),
                        size = 2,
                        alpha = 0.4,
                        shape = 1
                    )
                }

                # Add theoretical points with the correct size and alpha
                p <- p + geom_point(
                    data = theoretical_points,
                    aes(
                        x = skewness_squared,
                        y = kurtosis,
                        color = label,
                        shape = label,
                        size = size_map,
                        alpha = alpha_map
                    )
                ) +
                    scale_size_identity() +
                    scale_alpha_identity() +
                    coord_cartesian(xlim = c(0, xmax), ylim = c(ymax,0)) +
                    scale_x_continuous(breaks = seq(0, xmax, 1)) +
                    scale_y_continuous(breaks = seq(ymax, 0, -1))+
                    scale_color_manual(values = color_map) +
                    theme(
                        plot.title = element_text(
                            hjust = 0.5,
                            face = "bold",
                            family = input$fontFamily
                        ),
                        panel.grid = element_blank(),
                        panel.border = element_rect(
                            color = "black",
                            fill = NA,
                            linewidth  = 1
                        ),
                        axis.line = element_line(color = "black"),
                        text = element_text(family = input$fontFamily),
                        legend.position = "top",
                        legend.title = element_text(face = "bold")
                    ) +
                    guides(
                        color = guide_legend(title = "Distributions"),
                        shape = "none",
                        linetype = "none"
                    )

                # Convert to Plotly for interactivity
                p_plotly <- ggplotly(p, tooltip = c("x", "y", "color"))

                # Add saved comments as tooltips
                for (i in seq_along(p_plotly$x$data)) {
                    dist_name <- p_plotly$x$data[[i]]$name
                    if (!is.null(dist_name)) {
                        p_plotly$x$data[[i]]$text <- paste(
                            p_plotly$x$data[[i]]$text,
                            "<br>",
                            "Saved Comments:",
                            paste(
                                seq_along(tooltip_texts()[[dist_name]]),
                                tooltip_texts()[[dist_name]],
                                sep = ". ",
                                collapse = "<br>"
                            )
                        )
                        p_plotly$x$data[[i]]$hoverinfo <- "text"
                    }
                }

                return(p_plotly)
            }
        })




        # Render the updated plot
        output$distPlot <- renderPlotly({
            thePlot()  # Ensure updated plot is rendered
        })



        output$downloadPNG <- downloadHandler(
            filename = function() {
                # Generate filename based on the current date
                paste("graph-", Sys.Date(), ".png", sep = "")
            },
            content = function(file) {
                # Step 1: Save the current plot as an HTML file temporarily
                tempFile <- tempfile(fileext = ".html")
                saveWidget(thePlot(), tempFile)

                # Step 2: Use webshot2 to capture a screenshot of the plot as a PNG
                webshot(tempFile, file = file, selector = ".plotly")
            }
        )

        # Download PDF report handler
        output$downloadReport <- downloadHandler(
            filename = function() {
                # Generate report filename based on the current date
                paste("informe-", Sys.Date(), ".pdf", sep = "")
            },
            content = function(file) {
                # Step 1: Create a temporary file to store the plot image (PNG)
                tempPlotFile <- tempfile(fileext = ".png")

                # Step 2: Save the Plotly plot as an HTML file temporarily
                tempHtmlFile <- tempfile(fileext = ".html")
                saveWidget(thePlot(), tempHtmlFile)

                # Step 3: Capture the plot as a PNG using webshot
                webshot(tempHtmlFile,
                        file = tempPlotFile,
                        selector = ".plotly")

                # Step 4: Copy the PNG to the current working directory
                plotFile <- file.path(getwd(), "graph.png")
                file.copy(tempPlotFile, plotFile, overwrite = TRUE)

                # Step 5: Create a temporary RMarkdown file for the report
                tempReport <- tempfile(fileext = ".Rmd")

                # Step 6: Gather all the saved comments
                all_comments <- tooltip_texts()
                comment_list <- comments_generator(all_comments)

                # Step 7: Combine the comments into a single string for the report
                comment_section <- if (length(comment_list) == 0) {
                    "No comments available."
                } else {
                    paste(comment_list, collapse = "\n\n")
                }

                # Step 8: Capture the selected data and column name
                selected_data <- reactiveData()  # Store selected reactive data
                dataset_name <- input$selectedColumn  # Store selected dataset name

                # Step 9: Define the content for the RMarkdown report
                reportContent <- c(
                    "---",
                    paste0(
                        "title: 'Report of Cullen-Frey Graph and Distribution Comments for: ",
                        dataset_name,
                        "'"
                    ),
                    "output: pdf_document",
                    "---",
                    "",
                    "```{r setup, include=FALSE}",
                    "knitr::opts_chunk$set(echo = FALSE)",
                    "library(knitr)",
                    "```",
                    "",
                    "## Cullen-Frey Graph",
                    "",
                    paste0("![Graph Image](", plotFile, ")"),
                    "",
                    "## Comments",
                    "",
                    comment_section,
                    "",
                    "## Statistics",
                    "",
                    "```{r}",
                    "# Use the already processed selected data",
                    "selected_data <- ",
                    deparse(substitute(selected_data)),
                    # Store reactive data as an R object
                    "calculate_statistics(selected_data)",
                    # Calculate statistics on the data
                    "```"
                )

                # Step 10: Save the content into the temporary RMarkdown file
                writeLines(reportContent, tempReport)

                # Step 11: Render the RMarkdown file to a PDF document
                rmarkdown::render(tempReport,
                                  output_file = file,
                                  output_format = "pdf_document")
            }
        )


    }

    shinyApp(ui = ui, server = server)
}



