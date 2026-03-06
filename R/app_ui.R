#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyglide
#' @import shinyFiles
#' @importFrom shinyWidgets progressBar awesomeRadio
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "CYTOGLO",
      theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#1f77b4",  # colore brand CytoVerse
        secondary = "#ff7f0e"
      ),

      nav_panel("Upload & Preprocessing",
                glide(
                  screen(
                    next_condition = "output.check_step1 == true & output.compute_gated == true",
                    h4("Step 1. Read data from CytOF"),
                    fluidPage(

                      fluidRow(
                        h4("Select the output folder"),
                        shinyFiles::shinyDirButton("outputfolder",style ="padding:10px; font-size:110%;",
                                                   label = "Browse...", title = "Please select the output folder",
                                                   multiple = FALSE, icon = icon("folder-open")),
                        hr()

                      ),
                      conditionalPanel(
                        condition = "output.compute_peacoq == true",style = "display: none",
                        fluidRow(
                          h4("Select the data folder"),
                          shinyFiles::shinyDirButton("datafolder",style ="padding:10px; font-size:110%;",
                                                     label = "Browse...", title = "Please select the data folder",
                                                     multiple = FALSE, icon = icon("folder-open")),
                          actionButton("readdatabttn",style ="padding:10px; font-size:110%;", "Read Data", icon("gear")),
                        )
                      )

                    )

                  ),

                  screen(
                    h4("Step 2. Read data from CytOF"),
                    fluidRow(
                      progressBar(id = "prog_bar_gat", value = 0, total = 10,status = "info", title = "Samples", display_pct = TRUE)
                    ),
                    # fluidRow(
                    #   column(9, uiOutput("text_curr_sampl")),
                    # ),
                    fluidRow(
                      column(1, awesomeRadio("type_gating","Type of gating",choices = c("Rectangular","Ellipse"))),
                      column(2, selectInput("first_var_gat","Select the X var", choices = "")),
                      column(3,sliderInput("slider_x_gat","Range for X", min = 0,  max = 100, value = c(40, 60),width = "100%")),

                      column(2,offset=1,selectInput("second_var_gat","Select the Y var", choices = "")),
                      column(3,sliderInput("slider_y_gat","Range for Y", min = 0,  max = 100, value = c(40, 60),width = "100%"))
                    ),
                    hr(),
                    fluidRow(
                      column(4,plotOutput("plot_gating_before",height = "500px")),
                      column(
                        1,
                        div(
                          class = "d-flex align-items-center justify-content-center",
                          style = "height: 100%;",
                          icon("right-long", style = "font-size: 48px;")
                        )
                      ),
                      column(4,plotOutput("plot_gating",height = "500px")),
                      column(
                        2,
                        actionButton("bttn_next_gat_step2",style ="padding:10px; font-size:110%;", "Apply & Next gating", icon("circle-chevron-right")),
                        br(),
                        br(),
                        br(),br(),
                        actionButton("bttn_next_sampl_step2",style ="padding:10px; font-size:110%;", "Save & Next sample", icon("floppy-disk"))
                      )
                    )



                  ) #end of screen

                ) #end of glide
      ),

      nav_panel("Visualization",
                fluidPage(
                  plotOutput("plot_umap"),
                  plotOutput("plot_marker")
                )),

      nav_panel("Statistics",
                fluidPage(
                  verbatimTextOutput("summary_stats"),
                  tableOutput("stat_table")
                )),

      nav_panel("Custom Gating",
                fluidPage(
                  "Qui inserisci UI per gating personalizzato",
                  uiOutput("gating_ui")
                )),

      nav_panel("Export",
                fluidPage(
                  downloadButton("download_report", "Scarica report")
                ))
    )

  ) #end of taglist
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CYTOGLO"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
