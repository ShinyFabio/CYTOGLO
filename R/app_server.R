#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFiles
#' @importFrom flowCore read.FCS Subset write.FCS
#' @importFrom PeacoQC PeacoQC
#' @importFrom fs path_home
#' @import ggcyto
#' @import openCyto
#' @import ggplot2
#' @importFrom shinyWidgets updateProgressBar
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  volumes = c(Home = fs::path_home(), shinyFiles::getVolumes()())

  ############################ STEP 1  ###################################################

  shinyFiles::shinyDirChoose(input, 'outputfolder', roots = volumes, session = session)
  output_path = reactive({
    if(length(input$outputfolder) != 1 ) {
      shinyFiles::parseDirPath(volumes,input$outputfolder)
    }else{
      NULL
    }
  })

  check_peacoq = reactiveVal(1)
  check_gated = reactiveVal(0)

  observeEvent(output_path(), {
    req(output_path())

    #folder gated files
    data_dir = paste0(output_path(),'/Gating/Data/')
    files_gat <- list.files(data_dir, full.names = TRUE)

    #folder peacoQ
    dir_selected <- paste0(output_path(),"/PeacoQC_results/fcs_files")
    files <- list.files(dir_selected, full.names = TRUE)


    if (length(files_gat) > 0) {
      check_peacoq(1)
      check_gated(1)
      showModal(
        modalDialog(
          title = "Warning",
          paste0("Detected ", length(files_gat), " file in: ", data_dir,
                 "This means you have already performed the gating step. What do you want to do?"),
          footer = tagList(
            modalButton("Change folder"),
            actionButton("confirm_load_gat", "Load them")
          ),
          easyClose = FALSE
        )
      )

    } else if (length(files) > 0){
      check_gated(0)
      check_peacoq(1)
      showModal(
        modalDialog(
          title = "Warning",
          paste0("Detected ", length(files), " file in: ", dir_selected,
          "This means you have already removed anomalies using peak-based detection. What do you want to do?"),
          footer = tagList(
            modalButton("Change folder"),
            actionButton("confirm_load_peaco", "Load them")
          ),
          easyClose = FALSE
        )
      )

    } else {
      check_peacoq(0)
      sendmessages(paste0("Output folder set to: ",output_path()), type = "info")
    }

  })





  ########## true se devo elaborare i dati gated
  output$compute_gated = reactive({
    req(check_gated())
    check_gated()==0
  })
  outputOptions(output, "compute_gated", suspendWhenHidden = FALSE)


  step2 <- reactiveValues(data = NULL,
                          gate_settings = data.frame(sample = character(), n_in = double(), n_gated = double(), VarX = character(), VarY = character(),
                                                     minRangeX = double(), maxRangeX = double(), minRangeY = double(), maxRangeY = double()))


  observeEvent(input$confirm_load_gat, {
    req(output_path())
    removeModal()

    data_dir = paste0(output_path(),'/Gating/Data/')
    files_gat <- list.files(data_dir, full.names = TRUE)

    nfiles =length(files_gat)

    percentage <- 0
    withProgress(message = "Reading data...", value=0, {
      results = lapply(files_gat, function(i){
        percentage <<- percentage + 1/nfiles*100
        incProgress(1/nfiles, detail = paste0("Progress: ",round(percentage,0), " %"))
        cat("Processing file:", i, "\n")  # Messaggio per tracciare il progresso
        flowCore::read.FCS(i)
      })
    })

    names(results) <- basename(files_gat)
    step2$data <- results

    sendmessages(paste(nfiles, "files successfully loaded."), type = "success")
  })





  #true se devo elaborare i dati peacoq
  output$compute_peacoq = reactive({
    req(check_peacoq())
    check_peacoq()==0
    })
  outputOptions(output, "compute_peacoq", suspendWhenHidden = FALSE)


  step1 <- reactiveVal(NULL)

  observeEvent(input$confirm_load_peaco, {
    req(output_path())
    removeModal()

    dir_selected <- paste0(output_path(),"/PeacoQC_results/fcs_files/")

    files <- list.files(dir_selected, full.names = TRUE)
    nfiles =length(files)

    percentage <- 0
    withProgress(message = "Reading data...", value=0, {
      results = lapply(files, function(i){
        percentage <<- percentage + 1/nfiles*100
        incProgress(1/nfiles, detail = paste0("Progress: ",round(percentage,0), " %"))
        cat("Processing file:", i, "\n")  # Messaggio per tracciare il progresso
        flowCore::read.FCS(i)
      })
    })

    names(results) <- basename(files)
    step1(results)

    sendmessages(paste(nfiles, "files successfully loaded."), type = "success")
  })



  shinyFiles::shinyDirChoose(input, 'datafolder', roots = volumes, session = session)

  data_path = reactive({
    if(length(input$datafolder) != 1 ) {
      shinyFiles::parseDirPath(volumes,input$datafolder)
    }else{
      NULL
    }
  })



  observeEvent(input$readdatabttn, {

    req(data_path(), output_path())

    #mi leggo tutti i file presenti nella cartella
    file_list = list.files(data_path(), pattern = ".fcs$", full.names = TRUE)


    nfiles <- length(file_list)
    sendmessages(paste0("Number of files to be imported: ",nfiles), type = "info")

    sendmessages("Starting reading files...", type = "gears", duration = 7)


    percentage <- 0
    withProgress(message = "Reading data...", value=0, {
      results = lapply(file_list, function(i){
        percentage <<- percentage + 1/nfiles*100
        incProgress(1/nfiles, detail = paste0("Progress: ",round(percentage,0), " %"))

        ff <- flowCore::read.FCS(i)
        cat("Processing file:", i, "\n")
        #questa f scrive anche i file
        peacoqc_result <- PeacoQC::PeacoQC(
          ff,
          output_directory = output_path(),
          channels = c(1:7),
          IT_limit = 0.6,
          remove_zeros = TRUE,
          time_units = 50000
        )
        peacoqc_result$FinalFF
      })
    })

    names(results) <- basename(file_list)
    if(!is.null(results)){
      sendmessages("Reading file completed!", type = "success")
      step1(results)
    }

  })


  #se step1 è stato caricato. false if null
  output$check_step1 = reactive(
    return(!is.null(step1()))
  )
  outputOptions(output, "check_step1", suspendWhenHidden = FALSE)



  ############################ STEP 2  ###################################################

  currentSample <- reactiveVal(1)



  step1_sample <- reactiveVal(NULL)


  observe({
    req(step1(), currentSample())
    step1_sample(step1()[[currentSample()]])
    updateProgressBar(session = session, id = "prog_bar_gat", value = currentSample(), title = paste("Current sample:",names(step1()[currentSample()])), total = length(step1()))

  })



  observeEvent(step1_sample(),{
    req(step1_sample())
    vars = unique(step1_sample()@parameters@data$name)
    updateSelectInput(session, "first_var_gat", choices = vars, selected = vars[1])
    updateSelectInput(session, "second_var_gat", choices =  vars, selected = vars[2])
  })

  observeEvent(input$first_var_gat,{
    req(step1_sample())
    tt = as.data.frame(step1_sample()@exprs)[[input$first_var_gat]]
    updateSliderInput(session, "slider_x_gat", min = floor(min(tt)),  max = ceiling(max(tt)), value = unname(quantile(tt, probs = c(0.25, 0.75), na.rm = TRUE)))
  })

  observeEvent(input$second_var_gat,{
    req(step1_sample())
    tt = as.data.frame(step1_sample()@exprs)[[input$second_var_gat]]
    updateSliderInput(session, "slider_y_gat", min = floor(min(tt)),  max = ceiling(max(tt)), value = unname(quantile(tt, probs = c(0.25, 0.75), na.rm = TRUE)))
  })

  #plot senza gating
  output$plot_gating_before = renderPlot({
    req(step1_sample(),input$first_var_gat,input$second_var_gat)
    validate(
      need(input$first_var_gat != input$second_var_gat, "Variables X and Y must be different.")
    )

    autoplot(step1_sample(), x = input$first_var_gat, y = input$second_var_gat, bins=100)+
      labs(title=paste("Sample", step1_sample()@description$GUID))


  })

  #plot con gating
  output$plot_gating = renderPlot({
    req(step1_sample(),input$first_var_gat,input$second_var_gat)
    req(input$slider_x_gat, input$slider_y_gat)
    validate(
      need(input$first_var_gat != input$second_var_gat, "Variables X and Y must be different.")
    )

    min_limits <- c(min(input$slider_x_gat), min(input$slider_y_gat))
    max_limits <- c(max(input$slider_x_gat), max(input$slider_y_gat))

    if(input$type_gating == "Rectangular"){
      g <- openCyto:::.boundary(step1_sample(), channels = c(input$first_var_gat, input$second_var_gat), min = min_limits, max = max_limits)
    }else{
      g <- openCyto:::gate_flowclust_2d(step1_sample(), xChannel = input$first_var_gat,yChannel = input$second_var_gat, K=1, target=c(25,10), quantile=0.90)
    }


    autoplot(step1_sample(), x = input$first_var_gat, y = input$second_var_gat, bins=100)+ geom_gate(g)+geom_stats() +
      labs(title=paste("Sample", step1_sample()@description$GUID,"after gating"))

  })


  observeEvent(input$bttn_next_gat_step2,{

    showModal(
      modalDialog(
        title = "Warning",
        paste0("Do you want to apply this setting and perform another gating? You won't be able to go back to the previous gating."),
        footer = tagList(
          modalButton("No"),
          actionButton("confirm_next_gat_vars", "Yes")
        ),
        easyClose = FALSE
      )
    )
  })

  observeEvent(input$confirm_next_gat_vars,{

    req(step1_sample(),input$first_var_gat,input$second_var_gat)
    req(input$slider_x_gat, input$slider_y_gat)
    validate(need(input$first_var_gat != input$second_var_gat, "Variables X and Y must be different."))

    removeModal()

    #folder plot output
    plots_dir = paste0(output_path(),'/Gating/Plots/',step1_sample()@description$GUID,"/")
    if (!dir.exists(plots_dir)) {
      dir.create(plots_dir,recursive = TRUE)
    }


    min_limits <- c(min(input$slider_x_gat), min(input$slider_y_gat))
    max_limits <- c(max(input$slider_x_gat), max(input$slider_y_gat))

    if(input$type_gating == "Rectangular"){
      g <- openCyto:::.boundary(step1_sample(), channels = c(input$first_var_gat, input$second_var_gat), min = min_limits, max = max_limits)
    }else{
      g <- openCyto:::gate_flowclust_2d(step1_sample(), xChannel = input$first_var_gat,yChannel = input$second_var_gat, K=1, target=c(25,10), quantile=0.90)
    }

    p = autoplot(step1_sample(), x = input$first_var_gat, y = input$second_var_gat, bins=100)+ geom_gate(g)+geom_stats() +
      labs(title=paste("Sample", step1_sample()@description$GUID))

    ggsave(filename = paste0(plots_dir, input$first_var_gat,"_",input$second_var_gat,"_sample_", currentSample(), ".png"), plot = p, width = 8, height = 6)

    sendmessages(paste0("Plot saved in ",plots_dir,"."),"info")

    gated_data <- flowCore::Subset(step1_sample(), g)


    gate_settings =  data.frame(sample = gated_data@description$GUID, n_in = nrow(step1_sample()@exprs), n_gated = nrow(gated_data@exprs),
                                VarX = input$first_var_gat, VarY = input$second_var_gat,
                                minRangeX = min(input$slider_x_gat), maxRangeX = max(input$slider_x_gat),
                                minRangeY = min(input$slider_y_gat), maxRangeY = max(input$slider_y_gat))

    step1_sample(gated_data)

    step2$gate_settings <- rbind(step2$gate_settings, gate_settings)

    print(step2$gate_settings)

    sendmessages("Gating setting applied for the current sample.","info")

  })


  # SAVE AND GO TO THE NEXT SAMPLE
  observeEvent(input$bttn_next_sampl_step2,{

    if(currentSample() <= length(step1())){
      showModal(
        modalDialog(
          title = "Warning",
          paste0("Do you want to apply the current gating settings and proceed to the next sample? You won't be able to go back to the previous sample."),
          footer = tagList(
            modalButton("No"),
            actionButton("confirm_next_gat_sampl", "Yes")
          ),
          easyClose = FALSE
        )
      )
    }
    else{
      sendmessages("No more samples to process. You have reached the last sample.","info")
    }
  })

  observeEvent(input$confirm_next_gat_sampl,{
    removeModal()
    currentSample(currentSample() + 1)

    step2$data[[step1_sample()@description$GUID]] <- step1_sample()

    data_dir = paste0(output_path(),'/Gating/Data/')
    if (!dir.exists(data_dir)) {
      dir.create(data_dir,recursive = TRUE)
    }

    flowCore::write.FCS(step1_sample(), paste0(data_dir,step1_sample()@description$GUID,"_gated.fcs"))

    sendmessages("All gating settings have been applied to the current sample!", "success")
    sendmessages(paste0("The current sample has been saved in: ", data_dir), "success")

  })







}
