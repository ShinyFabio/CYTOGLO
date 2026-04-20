#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyglide
#' @import shinyFiles
#' @importFrom shinyjs useShinyjs
#' @import waiter
#' @importFrom shinyWidgets progressBar awesomeRadio pickerOptions pickerInput materialSwitch radioGroupButtons prettyRadioButtons
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @noRd

titles <- c("Step 1. Read data", "Step 2. Pre-filtering", "Step 3. Apply Gating","Step 4. Save data")

app_ui <- function(request) {
  tagList(
    # Aumenta il limite a 2 GB

    #serve per disabilitare i tasti quando vengono premuti
    shinyjs::useShinyjs(),

    use_waiter(), # OBBLIGATORIO: inizializza waiter nella UI

    tags$head(
      tags$style(HTML("
      .title-clipper {
        width: 100%;
        overflow: hidden;
        height: 40px;
        display: flex;
        justify-content: center;
        align-items: center;
      }

      #screen_title {
        display: inline-block;
        font-weight: bold;
        font-size: 1.3em;
        white-space: nowrap;
        /* La durata deve essere identica a quella di Glide (default 400ms) */
        animation-duration: 0.4s;
        animation-fill-mode: both;
        animation-timing-function: cubic-bezier(0.165, 0.84, 0.44, 1);
      }

      @keyframes slideNext {
        0% { transform: translateX(50px); opacity: 0; }
        100% { transform: translateX(0); opacity: 1; }
      }

      @keyframes slidePrev {
        0% { transform: translateX(-50px); opacity: 0; }
        100% { transform: translateX(0); opacity: 1; }
      }

      .animate-next { animation-name: slideNext; }
      .animate-prev { animation-name: slidePrev; }
    "))
    ),

    tags$script(HTML(sprintf("
    var titles = %s;
    var currentIdx = 0;

    $(document).on('shiny:connected', function() {

      function updateInstant(direction) {
        var nextIdx = currentIdx;

        if (direction === 'next' && currentIdx < titles.length - 1) {
          nextIdx++;
        } else if (direction === 'prev' && currentIdx > 0) {
          nextIdx--;
        } else {
          return; // Nessun movimento se ai bordi
        }

        var $title = $('#screen_title');
        var animClass = (direction === 'next') ? 'animate-next' : 'animate-prev';

        // 1. Cambia il testo istantaneamente
        $title.text(titles[nextIdx]);

        // 2. Riavvia l'animazione
        $title.removeClass('animate-next animate-prev');
        $title[0].offsetWidth; // Reset CSS
        $title.addClass(animClass);

        currentIdx = nextIdx;

        // Comunica l'indice a Shiny (opzionale, per logica server)
        Shiny.setInputValue('wizard_step', currentIdx + 1);
      }

      // Intercetta il CLICK (su mousedown e' ancora più veloce)
      $(document).on('mousedown', '.next-screen', function() {
        if (!$(this).hasClass('disabled')) updateInstant('next');
      });
      $(document).on('mousedown', '.prev-screen', function() {
        if (!$(this).hasClass('disabled')) updateInstant('prev');
      });

      // Intercetta la TASTIERA
      $(document).on('keydown', function(e) {
        if (e.key === 'ArrowRight') updateInstant('next');
        if (e.key === 'ArrowLeft') updateInstant('prev');
      });
    });
  ", jsonlite::toJSON(titles)))),


    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = span(
        img(src = "www/CYTOGLO_icon.png", height = "35px", style = "margin-right: 10px; vertical-align: middle;"),
        "CYTOGLO"
      ),
      # title = "CYTOGLO",
      theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#1f77b4",  # colore brand CytoVerse
        secondary = "#ff7f0e"
      ),

      nav_panel("Upload & Preprocessing",

                glide(id = "wizard",controls_position = "top",
                      custom_controls = div(
                        fluidRow(
                          # Prev a sinistra
                          column(3,div(style = "display:flex; justify-content:flex-start; align-items:center;",
                                prevButton(class = "btn btn-primary"))),
                          # Titolo al centro (reactive)
                          column(
                          6,
                          div(
                            style = "display:flex; justify-content:center; align-items:center; height:40px;font-size:24px;
                                      padding: 8px 16px; background-color: #1f77b4; color: white;
                                      border-radius: 12px; font-weight: bold; box-shadow: 1px 1px 5px rgba(0,0,0,0.3);",
                            textOutput("screen_title", inline = TRUE))),
                          # Next a destra
                          column(3, div(style = "display:flex; justify-content:flex-end; align-items:center;",
                                nextButton(class = "btn btn-primary")))
                        )
                      ),

                  ################# STEP 1 ##########
                  screen(
                    next_condition = "input.type_of_data == 'fcs'",
                    br(),
                    fluidRow(

                      column(3,
                             card(card_header("Type of Data"),
                                  radioGroupButtons("type_of_data", "Choose your type of data",
                                                    choices = c("Files from CytOF (.fcs)" = "fcs", "SingleCellExperiment (.rds)" = "sce"),
                                                    direction = "vertical", individual = F,justified = T, size = "lg",
                                                    checkIcon = list(
                                                      yes = tags$i(class = "fa fa-circle",
                                                                   style = "color: steelblue"),
                                                      no = tags$i(class = "fa fa-circle-o",
                                                                  style = "color: steelblue"))
                                  )
                             )
                      ),
                      column(
                        9,
                        card(
                          conditionalPanel(
                            "input.type_of_data == 'fcs'",
                            fluidRow(
                              column(
                                5,
                                card(
                                  card_header("FCS File Import and Preprocessing"),
                                  "Here you can read your .fcs files into a flowFrame object.
                            After loading them, you can start the analysis directly, or first
                            perform filtering and preprocessing steps such as gating and quality control with PeacoQC.")),
                              column(
                                3,
                                h5("Select the data folder"),
                                shinyFiles::shinyDirButton("datafolder",style ="padding:10px; font-size:110%;",
                                                           label = "Browse...", title = "Please select the data folder",
                                                           multiple = FALSE, icon = icon("folder-open"))),
                              column(
                                3,
                                conditionalPanel(
                                  "output.check_folderdata == true",
                                  div(class = "d-flex align-items-center justify-content-center", style = "height: 100%;",
                                      actionButton("readdatabttn",style ="padding:10px; font-size:150%;", "Read Data", icon("gear")))
                                ))
                            )
                          ),

                          conditionalPanel(
                            "input.type_of_data == 'sce'",
                            fluidRow(
                              column(
                                4,
                                card(
                                  card_header("SingleCellExperiment Import"),
                                  paste0("Import the SingleCellExperiment (.rds) previously created with ", golem::get_golem_name()))),
                              column(3, fileInput("filesce_input","Load a SCE file (.rds)", accept = ".rds")),
                              column(5,style="display: flex; justify-content: center; align-items: center;",
                                     conditionalPanel(
                                       "output.check_sce_data == true",
                                       uiOutput("rds_analysis_perf")
                                       )
                              )
                            )
                          )
                        )
                      )
                    )
                  ), #end of screen

                  ################# STEP 2 ##########
                  screen(
                    next_condition = "input.type_of_data == 'fcs'",
                    br(),
                    card(
                      fluidRow(
                        column(3,
                               pickerInput("picker_peacoqc", "Select the samples", choices = c(), multiple = TRUE,
                                 options = pickerOptions(container ="body",actionsBox = TRUE),width = "100%")
                        ),
                        column(3,
                               shinyWidgets::materialSwitch("checkplot_peaco", "Save plots?", value = FALSE, status = "primary"),
                               conditionalPanel(
                                 "input.checkplot_peaco == true",
                                 shinyFiles::shinyDirButton("outputfolder_peaco",style ="padding:10px; font-size:110%;",
                                                            label = "Browse...", title = "Please select the output folder",
                                                            multiple = FALSE, icon = icon("folder-open"))
                               )
                        ),
                        column(
                          3,
                          card(
                            card_header("PeacoQC settings"),
                            shinyWidgets::materialSwitch("remv_0_peaco",
                                                         tooltip(trigger = list("Remove zeros",icon("info-circle", style="color: #1f77b4;")),
                                                                 "If this is set to TRUE, the zero values will be removed before the peak detection step.
                                                                  They will not be indicated as 'bad' value. This is recommended when cleaning mass cytometry data."),
                                                         value = TRUE, status = "primary"),
                            pickerInput("channels_peacoqc",
                                        label = tooltip(trigger = list("Select the channels",icon("info-circle", style="color: #1f77b4;")),
                                                "Channels in the flowframe on which peaks have to be determined. By default it uses all the technical channels."),
                                        choices = c(), multiple = TRUE,
                                        options = pickerOptions(container ="body",actionsBox = TRUE),width = "100%")

                          )),

                        column(2, offset=1,
                               div(class = "d-flex align-items-center justify-content-center", style = "height: 100%;",
                                   actionButton("apply_peaco",style ="padding:10px; font-size:110%;", class = "btn-success btn-lg","Apply peacoQC", icon("gear")))
                               )
                      )
                    )


                  ),

                  ################# STEP 3 ##########
                  screen(
                    next_condition = "input.type_of_data == 'fcs'",
                    br(),
                    card(
                    fluidRow(
                      column(5, DTOutput("dt_sample_gating")),
                      column(2,
                             shinyWidgets::materialSwitch("checkplot_gat", "Save gating plots?", value = FALSE, status = "primary"),
                               conditionalPanel(
                                 "input.checkplot_gat == true",
                                 shinyFiles::shinyDirButton("outputfolder_gat",style ="padding:10px; font-size:110%;",
                                                            label = "Browse...", title = "Please select the output folder",
                                                            multiple = FALSE, icon = icon("folder-open")),

                             )),
                      column(3, uiOutput("text_curr_sampl")),
                      column(2,
                             div(class = "d-flex align-items-center justify-content-center", style = "height: 100%;",
                                 actionButton("save_gating", "Save Gating for this sample", class = "btn-success btn-lg", icon("floppy-disk"))))
                    )
                    ),
                    hr(),

                    fluidRow(

                      #sidebar with options
                      column(
                        3,
                        wellPanel(
                          fluidRow(
                            column(6,awesomeRadio("nvars_gating","Number of variables",choices = c("One variable","Two variables"), selected = "Two variables")),
                            column(6,
                              conditionalPanel("input.nvars_gating == 'Two variables'",
                                     awesomeRadio("type_gating","Type of gating",choices = c("Rectangular","Ellipse"))))
                          ),

                          fluidRow(
                            selectInput("first_var_gat","Select the X var", choices = "",width = "100%"),

                            conditionalPanel("input.type_gating == 'Rectangular' & input.nvars_gating == 'Two variables'",
                                             sliderInput("slider_x_gat","Range for X", min = 0,  max = 100, value = c(40, 60),width = "100%")),


                            conditionalPanel("input.nvars_gating == 'Two variables'",
                              selectInput("second_var_gat","Select the Y var", choices = "",width = "100%"),
                              conditionalPanel("input.type_gating == 'Rectangular'",
                                sliderInput("slider_y_gat","Range for Y", min = 0,  max = 100, value = c(40, 60),width = "100%"))
                              ),


                            conditionalPanel("input.type_gating == 'Ellipse' & input.nvars_gating == 'Two variables'",
                                             sliderInput("slider_ellipse_gat","Quantile for the ellipse", min = 0,  max = 0.99, value = 0.9,width = "100%")),

                            conditionalPanel("input.nvars_gating == 'One variable'",
                                             sliderInput("slider_1var_gat","Quantile", min = 0,  max = 0.99, value = 0.9,width = "100%"),
                                             radioGroupButtons("rl_1var_gat", tooltip(trigger = list("Where to cut",icon("info-circle", style="color: #1f77b4; cursor: help;")),
                                                                                      "Select whether to remove data from the left or right side of the quantile line."),
                                                               choices = c("<i class='fa fa-angle-left'></i> Left" = "left", "<i class='fa fa-angle-right'></i> Right" = "right"),
                                                               justified = TRUE)
                            )

                          )

                        ), #wellpanel
                        br()

                      ),

                      #panel with plots
                      column(
                        width = 8,
                        card(
                          # Creiamo un contenitore grid invece di fluidRow
                          div(
                            # 1fr = una frazione di spazio libero. I due grafici si divideranno equamente lo spazio
                            # lasciando esattamente 80px al centro per la freccia.
                            style = "display: grid; grid-template-columns: 1fr 80px 1fr; align-items: center; gap: 15px;",

                            # Contenitore Plot 1 (prenderà il primo 1fr)
                            div(
                              plotOutput("plot_gating_before", height = "500px")
                            ),

                            # Contenitore Freccia al centro (prenderà gli 80px)
                            div(
                              class = "d-flex align-items-center justify-content-center",
                              icon("right-long", style = "font-size: 55px; color:#1f77b4;")
                            ),

                            # Contenitore Plot 2 (prenderà il secondo 1fr)
                            div(
                              shinycssloaders::withSpinner(type=4,plotOutput("plot_gating", height = "500px"))
                            )
                          )
                        )
                      ),
                      column(
                        1,
                        div(
                          class = "d-flex align-items-center justify-content-center", style = "height: 100%;",
                          actionButton("bttn_next_gat_step2",style ="padding:10px; font-size:110%;", "Apply & Next gating", icon("circle-chevron-right"))))
                    )

                  ), #end of screen gating

                  ################# STEP 4 ##########
                  screen(
                    next_condition = "input.type_of_data == 'fcs'",
                    br(),
                    fluidRow(
                      column(
                        3,
                        card(
                          card_header("Step 4.1 Download gated files"),
                          p("Here you can download the .fcs files processed so far along with the workflow log containing all the gating settings.
                            Alternatively, in Step 4.2 you can create and download the final SCE object."),
                          fluidRow(
                            style = "display:flex; align-items:flex-end;",

                            column(6,h5("Search a folder"),
                                   shinyFiles::shinyDirButton("outputfolder_data",style ="padding:10px; font-size:110%;",
                                                                label = "Browse...", title = "Please select the output folder",
                                                                multiple = FALSE, icon = icon("folder-open"))),
                            column(6,
                                   conditionalPanel(
                                     "output.check_outputfolderdata == true",
                                     actionButton("bttn_save_data","Download .fcs data",style ="padding:10px; font-size:110%;", class = "btn-success btn-lg",icon("download"))
                                   ))
                          )
                        )
                      ),

                      column(
                        6,
                        card(
                          card_header("Step 4.2: Generate and Filter the SCE Object"),
                          p(paste("Load the Panel file and the Metadata file to generate a final SingleCellExperiment object.
                                  You can also check for any remaining problematic samples and remove them if necessary.")),
                          fluidRow(
                            column(5,
                              fileInput(
                                "panelfile",
                                label = tags$span(
                                  "Panel file (.csv or .xlsx) ",
                                  tags$span(
                                    # Questo blocca il click: non aprirà il selettore file se clicchi l'icona
                                    onclick = "event.preventDefault();",
                                    popover(
                                      # 1. Il Trigger (l'icona) deve essere esplicitamente il primo argomento
                                      trigger = icon("info-circle", style = "color: #1f77b4; cursor: help;"),
                                      title = "Panel file requirements",
                                      HTML("The panel file must contain, for each channel:<br>
                                      - <b>fcs_colname</b>:the column with the channel name in the FCS file<br>
                                      - <b>antigen</b>: the column with the targeted protein marker<br>
                                      - <b>marker_class</b>: optionally, the marker class ('type', 'state', or 'none')"),
                                      options = list(trigger = "hover")
                                    )
                                  )
                                )
                              ),

                              fileInput(
                                "metadatafile",
                                label = tags$span(
                                  "Metadata file (.csv or .xlsx) ",
                                  tags$span(
                                    # Questo blocca il click: non aprirà il selettore file se clicchi l'icona
                                    onclick = "event.preventDefault();",
                                    popover(
                                      # 1. Il Trigger (l'icona) deve essere esplicitamente il primo argomento
                                      trigger = icon("info-circle", style = "color: #1f77b4; cursor: help;"),
                                      title = "Metadata file requirements",
                                      HTML("The metadata file describes the experiment and must contain:<br>
                                            - <b>file_name</b>: the FCS file name<br>
                                            - <b>sample_id</b>: a unique sample identifier<br>
                                            - <b>patient_id</b>: the patient ID
                                           - <b>condition</b>: brief sample description (e.g. reference/stimulated, healthy/diseased)"),
                                      options = list(trigger = "hover")
                                    )
                                  )
                                )
                              )
                            ),


                            column(4,style = "text-align: center;",
                                   conditionalPanel(
                                     "output.check_panel_and_metadata == true",
                                     br(),
                                     actionButton("create_sce","Create SCE",icon("gear"),style ="padding:10px; font-size:140%;")
                                   ),
                                   br(), br(),
                                   conditionalPanel(
                                     "output.check_sce_data == true",
                                     actionButton("show_modal_filtSCE", "Filter SCE",icon("gear"),style ="padding:10px; font-size:140%;")
                                   )
                            ),

                            column(
                              3,
                              conditionalPanel(
                                "output.check_sce_data == true",
                                br(),br(),
                                actionButton("go_to_analysis_2","Go to the Analysis!",icon("rocket"),class = "btn-primary",style ="padding:10px; font-size:150%;"))
                            )
                          )
                        )
                      ),

                      column(
                        3,
                        card(
                          card_header("Step 4.3: Download the SCE Object"),
                          p(paste("Download the SingleCellExperiment object to use in", golem::pkg_name(), "for subsequent sessions.
                                  This may take a while depending on the size of your experiment")),
                          conditionalPanel(
                            "output.check_sce_data == true",
                            br(),
                            div(style = "text-align: center;",
                                downloadButton("download_sce","Download SCE",style ="padding:10px; font-size:110%;",
                                               class = "btn-success btn-lg",icon("download"))
                            )
                          )
                        )
                      )

                    )
                  ) #end screen step4


                ) #end of glide
      ),

      ################## CLUSTERING ##############
      nav_panel("Clustering",
        fluidRow(
          column(
            3,
            wellPanel(
              p("Here you can create a clustering and update the SCE."),
              fluidRow(
                column(6, actionButton("run_cluster", "Run clustering!",icon("gear"),style ="padding:10px; font-size:110%;")),
                column(6,
                       conditionalPanel("output.check_clusterdata == true",
                         downloadButton("down_cluster", "Download SCE",style ="padding:10px; font-size:110%;", class = "btn-success btn-lg",icon("download")))
                  )
              ),
              br(), hr(), br(),
              p("Plot settings"),
              conditionalPanel(
                "output.check_clusterdata == true",
                selectInput("metak_clust", "Number of clusters", choices = c(paste0("meta", 2:20))),

                selectInput("fun_clust", "Function for the summary statistic", choices = c("median", "mean", "sum")),
                selectInput("scale_clust", "Scaling strategy", choices = c("scale & trim then aggregate" = "first",
                                                                           "aggregate only" = "never")),
                checkboxInput("bar_heatmap","Barplot of cell counts per cluster", value = TRUE),
                checkboxInput("perc_heatmap","Display percentage labels next to bars", value = TRUE)
              )

            )
          ),

          column(
            9,
            shinycssloaders::withSpinner(type=4,plotOutput("heatmap_clust", height = "80vh", width = "100%"))
          )
        )

      ),

      nav_panel("Dimensionality Reduction",

                fluidRow(
                  column(
                    3,
                    wellPanel(
                      p("Here you can perform a dimensionality reduction and update the SCE."),
                      selectInput("type_pca","Type of dimension reduction to run", choices = c("UMAP", "TSNE", "PCA", "MDS", "DiffusionMap")),
                      sliderInput("ncells_rd",min = 100,  max = 10000, value = 1000,step = 100,width = "100%",
                                  label=tooltip(trigger = list("Cells per sample",icon("info-circle", style="color: #1f77b4;")),
                                          "The maximal number of cells per sample to use for dimension reduction.
                                          Keep in mind that using too many cells can significantly increase computation time and memory usage.")
                                  ),
                      fluidRow(
                        column(6, actionButton("run_pca", "Run DR!",icon("gear"),style ="padding:10px; font-size:110%;")),
                        column(6,
                               conditionalPanel("output.check_pcadata == true",
                                          downloadButton("down_pca", "Download SCE",style ="padding:10px; font-size:110%;", class = "btn-success btn-lg",icon("download")))
                        )
                      ),
                    br(), hr(), br(),
                    p("Plot settings"),
                    conditionalPanel(
                      "output.check_pcadata == true",
                      selectInput("type_pca_plot","Type of dimension reduction to plot", choices = ""),
                      fluidRow(
                        column(6,awesomeRadio("typevar_colorpca","Color by (da cambiare nomi)",choices = c("coldata","rowdata"))),
                        column(6, selectInput("var_colorpca", "Color by",choices = ""))
                      ),
                      checkboxInput("scale_pcaplot","Scale data", value = TRUE),
                      selectInput("facet_pcaplot","Facet by",choices = "")
                    )


                    )
                  ),

                  column(9, shinycssloaders::withSpinner(type=4,plotOutput("plot_pca", height = "80vh", width = "100%")))

                  )
                ), #end of navpanel






      nav_panel("Differential Expression",
                fluidRow(
                  column(
                    3,
                    wellPanel(
                      p("Here you can perform a Dunn's Kruskal-Wallis Multiple Comparisons test to see the significative conditions in each cluster and for each marker."),
                      selectInput("DE_selcluster","Number of clusters", choices = c(paste0("meta", 2:20))),

                      pickerInput("DE_picker_mark", "Select the markers", choices = c(), multiple = TRUE,
                                  options = pickerOptions(container ="body",actionsBox = TRUE),width = "100%"),

                      prettyRadioButtons("type_aggr_DE", "How to aggregate data", choices = c("mean", "sum", "num.detected", "prop.detected", "median"), inline = TRUE, selected = "mean"),
                      prettyRadioButtons("expdes_thresh", "p-value threshold", choices = c(0.01, 0.05, 0.1), inline = TRUE, selected = 0.05),

                      conditionalPanel(
                        "output.check_clusterdata == true",
                        fluidRow(
                          column(5, actionButton("run_DE", "Run DE!",icon("gear"),style ="padding:10px; font-size:110%;")),
                          column(7,
                            conditionalPanel("output.check_DEdata == true",
                              downloadButton("down_DE", "Download DE results",style ="padding:10px; font-size:110%;", class = "btn-success btn-lg",icon("download")))
                          )
                        )
                      )
                    )
                  ),


                  column(5,DTOutput("DE_table")),
                  column(4,shinycssloaders::withSpinner(type=4,plotOutput("DE_boxplot")))


                )
                )#end of navpanel


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
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CYTOGLO"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
