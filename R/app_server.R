#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFiles
#' @importFrom shinyjs enable disable
#' @import waiter
#' @importFrom flowCore read.FCS Subset write.FCS flowSet keyword
#' @importFrom PeacoQC PeacoQC
#' @importFrom fs path_home
#' @import ggcyto
#' @import openCyto
#' @import ggplot2
#' @importFrom shinyWidgets updatePickerInput show_alert
#' @importFrom DT renderDT datatable
#' @importFrom CATALYST prepData plotCounts filterSCE cluster plotExprHeatmap plotDR runDR cluster_ids state_markers
#' @importFrom readxl read_excel
#' @importFrom scuttle aggregateAcrossCells
#' @importFrom FSA dunnTest
#' @importFrom SummarizedExperiment assay
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  options(shiny.maxRequestSize = 2000 * 1024^2)


  volumes = c(Home = fs::path_home(), shinyFiles::getVolumes()())

  titles <- c("Step 1. Read data", "Step 2. Pre-filtering", "Step 3. Apply Gating","Step 4. Save data")

  output$screen_title <- renderText({
    idx <- input$wizard_step

    if (is.null(idx)) return(titles[1])

    idx <- min(max(as.integer(idx), 1L), length(titles))
    titles[idx]
  })




  ###################################### STEP 1  ###################################################

  sce_data <- reactiveVal(NULL)


  observeEvent(input$filesce_input, {
    req(input$filesce_input)
    ext <- tools::file_ext(input$filesce_input$name)

    if (ext != "rds") {
      shinyWidgets::show_alert("Invalid file!", "Please upload a .rds file", type = "error")
      return()
    }

    w <- Waiter$new(
      html = tagList(
        spin_flower(),
        h3("Loading SingleCellExperiment...", style = "color:white;"),
        p("This may take a while depending on the file size.", style = "color:white;")
      ),
      color = "rgba(0, 0, 0, 0.7)"
    )
    w$show()

    on.exit({
      w$hide()
    }, add = TRUE)

    file <- readRDS(file = input$filesce_input$datapath)

    if (!is.null(file)) {
      sce_data(file)
      sendmessages("SingleCellExperiment successfully loaded!", type = "success")
    }
  })

  observeEvent(sce_data(),{
    req(sce_data())
    if("cluster_id" %in% colnames(sce_data()@colData)){
      sendmessages("Clustering detected in the SCE object.", type = "info")
    }
    if(length(SingleCellExperiment::reducedDimNames(sce_data()))>0){
      sendmessages(paste("Dimension reduction", paste0(SingleCellExperiment::reducedDimNames(sce_data()),collapse=", "),"detected in the SCE object."), type = "info")
    }
  }, once = TRUE)




  output$rds_analysis_perf <- renderUI({
    req(sce_data())



    if("cluster_id" %in% colnames(sce_data()@colData)){
      icon_cl = icon("circle-check", style = "color: #28a745;")
    }else{
      icon_cl = icon("times-circle", style = "color: #d9534f;")
    }
    if(length(SingleCellExperiment::reducedDimNames(sce_data()))>0){
      icon_dr = icon("circle-check", style = "color: #28a745;")
    }else{
      icon_dr = icon("times-circle", style = "color: #d9534f;")

    }

    card(
      class = "bg-primary",
      style= "width: 70%;",
      card_header("Details of the SingleCellExperiment loaded"),
      card_body(
        div(style="font-size: 1.5rem;",class = "d-flex justify-content-between align-items-center",
            "Clustering", icon_cl),
        div(style="font-size: 1.5rem;",class = "d-flex justify-content-between align-items-center",
            "Dimension Reduction", icon_dr)
      )
    )
  })




  shinyFiles::shinyDirChoose(input, 'datafolder', roots = volumes, session = session)

  datafolder_path <- reactiveVal(NULL)
  observeEvent(input$datafolder,{
    if(length(input$datafolder) != 1 ) {
      folder <- shinyFiles::parseDirPath(volumes,input$datafolder)
      datafolder_path(folder)
      sendmessages(paste0("Data folder set to: ",datafolder_path()), type = "info")

      nfiles <- length(list.files(folder, pattern  = "\\.fcs$"))
      if(nfiles>0){
        sendmessages(paste0("Found ",nfiles," files."), type = "info")
      }else{
        show_alert(title = "Error !!", text = "No .fcs files found in this folder.", type = "error")
        datafolder_path(NULL)
      }
    }
  })


  #se step1 è stato caricato. false if null
  output$check_folderdata = reactive(
    return(!is.null(datafolder_path()))
  )
  outputOptions(output, "check_folderdata", suspendWhenHidden = FALSE)



  datafcs <- reactiveValues(data = NULL,
                            peacoQC_samples = data.frame(sample = character(), peacoQC = character()),
                            gate_settings = data.frame(sample = character(), n_in = double(), n_gated = double(), VarX = character(), VarY = character(),
                                                       minRangeX = double(), maxRangeX = double(), minRangeY = double(), maxRangeY = double(),
                                                       quantile_ellipse  = double(), quantile_1var = double(), side_removed = character()))



  observeEvent(input$readdatabttn, {
    req(datafolder_path())

    files <- list.files(datafolder_path(), full.names = TRUE, pattern  = "\\.fcs$")

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
    datafcs$data <- results

    sendmessages(paste(nfiles, "files successfully loaded."), type = "success")
  })



  stato_campioni <- reactiveVal(NULL)

  observeEvent(datafcs$data,{
    req(datafcs$data)

    df <- data.frame(
      File_FCS = names(datafcs$data),
      PeacoQC = as.character(icon("times-circle", style = "color: #d9534f;")),
      Gating = as.character(icon("times-circle", style = "color: #d9534f;")),
      stringsAsFactors = FALSE
    )

    ngated <- 0
    npeaco <- 0
    # 2. Ciclo di aggiornamento
    # Assumendo che datafcs$data sia la lista dei flowFrame
    for (i in seq_along(datafcs$data)) {

      ff <- datafcs$data[[i]]
      kw <- flowCore::keyword(ff) # Estraiamo le keyword una volta sola per efficienza

      # Controllo PeacoQC
      if (!is.null(kw$PEACOQC_PROCESSED) && kw$PEACOQC_PROCESSED == "TRUE") {
        df$PeacoQC[i] <- as.character(icon("circle-check", style = "color: #28a745;"))
        npeaco <- npeaco+1
      }

      # Controllo Gating
      if (!is.null(kw$GATED) && kw$GATED == "TRUE") {
        df$Gating[i] <- as.character(icon("circle-check", style = "color: #28a745;"))
        ngated <- ngated+1
      }
    }

    if(sum(ngated,npeaco)>0){
      sendmessages(sprintf(
        "%d file%s already processed with PeacoQC and %d file%s already gated. See the table in Step 3.",
        npeaco,ifelse(npeaco == 1, "", "s"),ngated, ifelse(ngated == 1, "", "s")), type = "info",duration=5)
    }

    stato_campioni(df)
  }, once = TRUE)

  ######################################################### STEP 2 PEACOQC ###########################################



  observeEvent(datafcs$data, {
    # req(datafcs$data)
    current_selected <- input$picker_peacoqc
    all_choices <- names(datafcs$data)
    updatePickerInput(session, "picker_peacoqc",choices = all_choices,selected = intersect(current_selected, all_choices))

    nl = datafcs$data[[1]]@parameters@data
    updatePickerInput(session, "channels_peacoqc",choices = unname(nl$name[is.na(nl$desc)]),selected = unname(nl$name[is.na(nl$desc)]))


  }, ignoreNULL = TRUE)

  shinyFiles::shinyDirChoose(input, 'outputfolder_peaco', roots = volumes, session = session)

  peaco_plot_path = reactive({
    if(length(input$outputfolder_peaco) != 1 ) {
      folder <- shinyFiles::parseDirPath(volumes,input$outputfolder_peaco)
      sendmessages(paste0("Folder plot set to: ",folder), type = "info")
      return(folder)
    }else{
      NULL
    }
  })





  observeEvent(input$apply_peaco, {
    req(datafcs$data)

    if(is.null(input$picker_peacoqc)){
      sendmessages("Select at least one sample before applying the correction.", type = "danger")
      return(NULL)
    }


    if(input$checkplot_peaco){
      if(is.null(peaco_plot_path())){
        show_alert(title = "Error !!", text = "Select a folder where to save the plots or remove the plots generation.", type = "error")
        return(NULL)
      }else{
        folder_plot <- peaco_plot_path()
      }
    }else{
      folder_plot <- NULL
    }

    file_list = names(datafcs$data)

    if(!("Time" %in% input$channels_peacoqc)){
      sendmessages("Select Time",type="danger")
      return(NULL)
    }
    if(length(input$channels_peacoqc)<2){
      sendmessages("Select at least two channels",type="danger")
      return(NULL)
    }

    channl_sel = which(datafcs$data[[1]]@parameters@data$name %in% input$channels_peacoqc)


    percentage <- 0
    withProgress(message = "Processing data...", value=0, {
      results_2 = lapply(file_list, function(i){

        if(i %in% input$picker_peacoqc){
          cat("Processing sample:", i, "\n")
          peacoqc_result <- PeacoQC::PeacoQC(
            datafcs$data[[i]],
            save_fcs = F,
            output_directory = folder_plot, #da testare se salva i plot
            channels = channl_sel,
            IT_limit = 0.6,
            remove_zeros = input$remv_0_peaco, #da far scegliere
            time_units = 50000
          )
          percentage <<- percentage + 1/length(input$picker_peacoqc)*100
          incProgress(1/length(input$picker_peacoqc), detail = paste0("Progress: ",round(percentage,0), " %"))
          ff_pulito <- peacoqc_result$FinalFF

          flowCore::keyword(ff_pulito)$PEACOQC_PROCESSED <- "TRUE"

          col_names <- flowCore::colnames(ff_pulito)

          ff_pulito[, col_names != "Original_ID"] ####ho rimosso Original_ID perchè poi x creare SCE se non ho fatto peacoQC ovunque va in errore

        }else{
          datafcs$data[[i]]
        }
      })
    })
    names(results_2) <- file_list


    datafcs$peacoQC_samples <- data.frame(
      sample = file_list,
      peacoQC = ifelse(file_list %in% input$picker_peacoqc, "yes", "no"),
      stringsAsFactors = FALSE
    )
    datafcs$data <- results_2


    # Aggiorniamo solo i file selezionati nel picker che NON sono già stati completati
    df <- stato_campioni()
    df$PeacoQC <- ifelse(
      df$File_FCS %in% input$picker_peacoqc,
      as.character(icon("circle-check", style = "color: #28a745;")),
      df$PeacoQC
    )
    stato_campioni(df)


    sendmessages(paste(length(input$picker_peacoqc), "samples successfully cleaned."), type = "success")
  })


  ############################################## STEP 3 GATING #######################################################


  shinyFiles::shinyDirChoose(input, 'outputfolder_gat', roots = volumes, session = session)

  gating_plot_path = reactive({
    if(length(input$outputfolder_gat) != 1 ) {
      folder <- shinyFiles::parseDirPath(volumes,input$outputfolder_gat)
      sendmessages(paste0("Folder plot set to: ",folder), type = "info")
      return(folder)
    }else{
      NULL
    }
  })


  # 2. Renderizziamo la tabella
  output$dt_sample_gating <- renderDT({
    req(stato_campioni())
    datatable(
      stato_campioni(),
      escape = FALSE,
      selection = "single", # Permette di selezionare una sola riga alla volta
      rownames = FALSE,
      caption = 'Please select a sample to apply the gating.',
      autoHideNavigation=T,
      # style = "bootstrap",
      options = list(dom = 'tp', pageLength = 7,scrollX = TRUE) # 't' nasconde la barra di ricerca per pulizia
    )
  },server = F) #server=F può rallentare se ci sono tanti elementi



  currentSample <- reactiveVal(NULL)

  observeEvent(input$dt_sample_gating_rows_selected,{
    currentSample(names(datafcs$data)[input$dt_sample_gating_rows_selected])
  })


  # 3. Mostriamo quale file stiamo analizzando
  output$text_curr_sampl <- renderUI({
    if (is.null(currentSample())) return("No sample selected. Click on a row in the table.")
    sendmessages(paste("Gating in progress on:", currentSample()),"info",3)

    card(
      card_header("Gating in progress on:"),
      card_body(currentSample()),
      class =  "bg-primary"
    )
  })





  step1_sample <- reactiveVal(NULL)


  observe({
    req(datafcs$data, currentSample())
    step1_sample(datafcs$data[[currentSample()]])
  })

  observeEvent(step1_sample(),{

    nl = step1_sample()@parameters@data
    nl$desc[is.na(nl$desc)] <- nl$name[is.na(nl$desc)]

    updateSelectInput(session, "first_var_gat", choices = setNames(nl$name, nl$desc), selected = nl$name[1])
    updateSelectInput(session, "second_var_gat", choices = setNames(nl$name, nl$desc), selected = nl$name[2])
  })

  observe({
    req(input$first_var_gat)
    req(step1_sample())
    tt = as.data.frame(step1_sample()@exprs)[[input$first_var_gat]]
    qnts = unname(quantile(tt, probs = c(0.25, 0.75), na.rm = TRUE))
    if(qnts[1] == qnts[2]){qnts[2] <- qnts[2]+1}
    updateSliderInput(session, "slider_x_gat", min = floor(min(tt)),  max = ceiling(max(tt)), value = qnts)
  })

  observe({
    req(input$second_var_gat)
    req(step1_sample())
    tt = as.data.frame(step1_sample()@exprs)[[input$second_var_gat]]
    qnts = unname(quantile(tt, probs = c(0.25, 0.75), na.rm = TRUE))
    if(qnts[1] == qnts[2]){qnts[2] <- qnts[2]+1}
    updateSliderInput(session, "slider_y_gat", min = floor(min(tt)),  max = ceiling(max(tt)), value = qnts)
  })

  # PLOT SENZA GATING
  output$plot_gating_before = renderPlot({
    req(step1_sample(), input$first_var_gat)
    if(input$nvars_gating != 'One variable') req(input$second_var_gat)

    build_gating_plot(
      sample = step1_sample(), n_vars = input$nvars_gating,
      var_x = input$first_var_gat, var_y = input$second_var_gat,
      gating_type = input$type_gating,
      slider_1v = input$slider_1var_gat,
      slider_x = input$slider_x_gat, slider_y = input$slider_y_gat,
      ellipse_val = input$slider_ellipse_gat,
      apply_gate = FALSE,
      side_qnt_1v = input$rl_1var_gat
    )
  })

  # PLOT CON GATING
  output$plot_gating = renderPlot({
    req(step1_sample(), input$first_var_gat)
    # Aggiungi i req necessari per i gating

    build_gating_plot(
      sample = step1_sample(), n_vars = input$nvars_gating,
      var_x = input$first_var_gat, var_y = input$second_var_gat,
      gating_type = input$type_gating,
      slider_1v = input$slider_1var_gat,
      slider_x = input$slider_x_gat, slider_y = input$slider_y_gat,
      ellipse_val = input$slider_ellipse_gat,
      apply_gate = TRUE,
      side_qnt_1v = input$rl_1var_gat
    )
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




  observeEvent(input$confirm_next_gat_vars, {
    req(step1_sample(), input$first_var_gat)
    removeModal()

    # 1. Setup Directory
    if(input$checkplot_gat){
      if(is.null(gating_plot_path())){
        show_alert(title = "Error !!", text = "Select a folder where to save the plots or remove the plots generation.", type = "error")
        return(NULL)
      }else{
        plots_dir <- file.path(gating_plot_path(), "Gating", "Plots", step1_sample()@description$GUID)
        if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
        cat(plots_dir, " created.\n")
      }
    }

    # 2. Generate Plot
    p <- build_gating_plot(
      sample      = step1_sample(),
      n_vars      = input$nvars_gating,
      var_x       = input$first_var_gat,
      var_y       = input$second_var_gat,
      gating_type = input$type_gating,
      apply_gate   = FALSE,
      slider_1v   = input$slider_1var_gat,
      slider_x    = input$slider_x_gat,
      slider_y    = input$slider_y_gat,
      ellipse_val = input$slider_ellipse_gat,
      side_qnt_1v = input$rl_1var_gat
    )

    # 3. Logic for Gate Object (g) and Filename
    if (input$nvars_gating == 'One variable') {
      req(input$slider_1var_gat)
      cut_val <- quantile(step1_sample()@exprs[, input$first_var_gat], input$slider_1var_gat)
      g <- rectangleGate(.gate = setNames(list(c(cut_val, Inf)), input$first_var_gat))

      file_name <- paste0(input$first_var_gat, "_sample_", currentSample(), ".png")
    } else {
      req(input$second_var_gat)
      if (input$type_gating == "Rectangular") {
        req(input$slider_x_gat, input$slider_y_gat)
        g <- openCyto:::.boundary(step1_sample(),
                                  channels = c(input$first_var_gat, input$second_var_gat),
                                  min = c(min(input$slider_x_gat), min(input$slider_y_gat)),
                                  max = c(max(input$slider_x_gat), max(input$slider_y_gat)))
      } else {
        req(input$slider_ellipse_gat)
        g <- openCyto:::gate_flowclust_2d(step1_sample(), xChannel = input$first_var_gat,
                                          yChannel = input$second_var_gat, K = 1,
                                          quantile = input$slider_ellipse_gat)
      }
      file_name <- paste0(input$first_var_gat, "_", input$second_var_gat, "_sample_", currentSample(), ".png")
    }

    if(input$checkplot_gat){
      # 4. Save Plot and Notify
      ggsave(filename = file.path(plots_dir, file_name), plot = p, width = 8, height = 6)
      sendmessages(paste0("Plot saved in ", plots_dir, "."), "info")
    }

    # 5. Data Subsetting
    gated_data <- flowCore::Subset(step1_sample(), g)
    flowCore::keyword(gated_data)$GATED <- "TRUE"

    # 6. Build Gate Settings DataFrame
    new_settings <- data.frame(
      sample           = gated_data@description$GUID,
      n_in             = nrow(step1_sample()@exprs),
      n_gated          = nrow(gated_data@exprs),
      VarX             = input$first_var_gat,
      VarY             = if(input$nvars_gating == 'Two variables') input$second_var_gat else NA,
      type_gate        = if(input$nvars_gating == 'Two variables') input$type_gating else NA,
      minRangeX        = if(input$nvars_gating == 'Two variables' && input$type_gating == "Rectangular") min(input$slider_x_gat) else NA,
      maxRangeX        = if(input$nvars_gating == 'Two variables' && input$type_gating == "Rectangular") max(input$slider_x_gat) else NA,
      minRangeY        = if(input$nvars_gating == 'Two variables' && input$type_gating == "Rectangular") min(input$slider_y_gat) else NA,
      maxRangeY        = if(input$nvars_gating == 'Two variables' && input$type_gating == "Rectangular") max(input$slider_y_gat) else NA,
      quantile_ellipse = if(input$nvars_gating == 'Two variables' && input$type_gating != "Rectangular") input$slider_ellipse_gat else NA,
      quantile_1var    = if(input$nvars_gating == 'One variable') input$slider_1var_gat else NA,
      side_removed = input$rl_1var_gat,
      stringsAsFactors = FALSE
    )

    # 7. Update State
    step1_sample(gated_data)
    datafcs$gate_settings <- rbind(datafcs$gate_settings, new_settings)

    sendmessages("Gating setting applied for the current sample.", "info")
  })





  # SAVE AND GO TO THE NEXT SAMPLE
  observeEvent(input$save_gating,{

    if(any(datafcs$gate_settings$sample== step1_sample()@description$GUID)){
      showModal(
        modalDialog(
          title = "Warning",
          paste0("Do you want to apply these gating settings? The current sample data will be overwritten with the gated subset,
               and you will not be able to undo this operation or return to the previous state."),
          footer = tagList(
            modalButton("No"),
            actionButton("confirm_next_gat_sampl", "Yes")
          ),
          easyClose = FALSE
        )
      )
    }else{
      show_alert(title = "Warning !", text = "No gating has been applied to this sample. Nothing will be saved.
          To apply a gating, first click the 'Apply & Next Gating' button.", type = "warning")
    }

  })

  observeEvent(input$confirm_next_gat_sampl,{
    removeModal()

    datafcs$data[[step1_sample()@description$GUID]] <- step1_sample()

    # Aggiorniamo solo i file selezionati nel picker che NON sono già stati completati
    df <- stato_campioni()
    df$Gating <- ifelse(
      df$File_FCS == currentSample(),
      as.character(icon("circle-check", style = "color: #28a745;")),
      df$Gating
    )
    stato_campioni(df)

    sendmessages("All gating settings have been applied to the current sample!", "success")

  })



  ####################################### STEP 4 SAVE ALL THE DATA #######################



  shinyFiles::shinyDirChoose(input, 'outputfolder_data', roots = volumes, session = session)

  output_data_path <- reactiveVal(NULL)
  observeEvent(input$outputfolder_data,{
    if(length(input$outputfolder_data) != 1 ) {
      folder <- shinyFiles::parseDirPath(volumes,input$outputfolder_data)
      output_data_path(folder)
      sendmessages(paste0("Data output folder set to: ",output_data_path()), type = "info")
    }
  })

  #se outputfolder_data è stato caricato. false if null
  output$check_outputfolderdata = reactive(
    return(!is.null(output_data_path()))
  )
  outputOptions(output, "check_outputfolderdata", suspendWhenHidden = FALSE)



  observeEvent(input$bttn_save_data, {

    if(is.null(output_data_path())){
      show_alert(title = "Error!", text = "Select a folder where to save the samples.", type = "error")
      return(NULL)
    }

    req(datafcs$data)

    files <- file.path(output_data_path(), names(datafcs$data))
    nfiles =length(names(datafcs$data))

    if (any(file.exists(files))) {

      cat(paste("The following file already exists:",basename(files[any(file.exists(files))]),"\n"))

      showModal(
        modalDialog(
          title = "Warning",
          paste("Some files already exist. Do you want to overwrite them? You can check the console to see the specific filenames."),
          footer = tagList(
            modalButton("No"),
            actionButton("confirm_overwrite_sampl", "Yes")
          ),
          easyClose = FALSE
        )
      )

    } else {
      percentage <- 0
      withProgress(message = "Writing data...", value=0, {
        for (i in names(datafcs$data)) {
          cat("Processing file:", i, "\n")

          flowCore::write.FCS(datafcs$data[[i]], file.path(output_data_path(), i))
          percentage <- percentage + 1/nfiles * 100
          incProgress(1/nfiles, detail = paste0("Progress: ", round(percentage,0), " %"))
        }
      })

      sendmessages("Samples successfully saved.", "success")

      save_workflow_log(output_data_path(),peac_df = datafcs$peacoQC_samples, gat_df=datafcs$gate_settings)
      sendmessages(paste0("Workflow log saved to: ", file.path(output_data_path(), "workflow_log.txt")), "success")

      }
  })

  observeEvent(input$confirm_overwrite_sampl, {

    removeModal()

    files <- file.path(output_data_path(), names(datafcs$data))
    nfiles =length(names(datafcs$data))

    percentage <- 0
    withProgress(message = "Writing data...", value=0, {
      for (i in names(datafcs$data)) {
        cat("Processing file:", i, "\n")

        flowCore::write.FCS(datafcs$data[[i]], file.path(output_data_path(), i))
        percentage <- percentage + 1/nfiles * 100
        incProgress(1/nfiles, detail = paste0("Progress: ", round(percentage,0), " %"))
      }
    })

    sendmessages("Samples successfully saved.", "success")

    save_workflow_log(output_data_path(),peac_df = datafcs$peacoQC_samples, gat_df=datafcs$gate_settings)
    sendmessages(paste0("Workflow log saved to: ", file.path(output_data_path(), "workflow_log.txt")), "success")
  })



  paneldata <- reactiveVal(NULL)
  observeEvent(input$panelfile,{

    ext <- tools::file_ext(input$panelfile$datapath)
    validate(need(ext %in% c("csv","xlsx","xls"), "Please upload a csv or Excel file"))

    if(ext=="csv"){
      panel <- read.csv(input$panelfile$datapath)
    }else if(ext %in% c("xlsx","xls")){
      panel <- readxl::read_excel(input$panelfile$datapath)
    }else{
      shinyWidgets::show_alert("Invalid file!", "Please upload a csv or Excel file", type = "error")
      return(NULL)
    }

    if(all(c("fcs_colname", "antigen", "marker_class") %in% colnames(panel))){
      paneldata(panel)
      sendmessages("Panel data file loaded correctly.", "success")
    }else{

      shinyWidgets::show_alert("Invalid file! Your panel data is missing required columns: 'fcs_colname', 'antigen', or 'marker_class'.
                               Please update the file before proceeding.", type = "error")
      return()

    }

    # a data.frame containing, for each channel, its column name in the input data, targeted protein marker, and (optionally)
    # class ("type", "state", or "none").
    # panel_cols = list(channel = "fcs_colname", antigen = "antigen", class = "marker_class"),

    # a names list specifying the panel column names that contain channel names, targeted protein markers, and (optionally) marker classes.
    # When only some panel_cols deviate from the defaults, specifying only these is sufficient.

  })


  metadata <- reactiveVal(NULL)

  observeEvent(input$metadatafile,{
    ext <- tools::file_ext(input$metadatafile$datapath)

    # validate(need(ext %in% c("csv","xlsx","xls"), "Please upload a csv or Excel file"))

    if(ext=="csv"){
      md <- read.csv(input$metadatafile$datapath)
    }else if(ext %in% c("xlsx","xls")){
      md <- readxl::read_excel(input$metadatafile$datapath)
    }else{
      shinyWidgets::show_alert("Invalid file!", "Please upload a csv or Excel file", type = "error")
      return(NULL)
    }

    if(all(c("file_name", "sample_id", "patient_id", "condition") %in% colnames(md))){
      md$sample_id <- factor(md$sample_id, levels = md$sample_id[order(md$condition)])
      md$condition <- factor(md$condition)
      sendmessages("Metadata file loaded correctly.", "success")
      metadata(md)
    }else{
      shinyWidgets::show_alert("Invalid file! Your metadata file is missing required columns: 'file_name', 'sample_id', 'patient_id', or 'condition'.
                               Please update the file before proceeding.", type = "error")
      return()

    }

    # md_cols = list(file = "file_name", id = "sample_id", factors = c("condition", "patient_id"))
    # a table with column describing the experiment. An exemplary metadata table could look as follows:
    #   file_name: the FCS file name
    # sample_id: a unique sample identifier
    # patient_id: the patient ID
    # condition: brief sample description (e.g. reference/stimulated, healthy/diseased)
  })


  #check data correctly loaded
  output$check_panel_and_metadata = reactive(
    return(all(!is.null(metadata()),!is.null(paneldata())))
  )
  outputOptions(output, "check_panel_and_metadata", suspendWhenHidden = FALSE)


  observeEvent(input$create_sce,{
    req(datafcs$data, paneldata(),metadata())
    #create flowset

    sendmessages("Start creating SingleCellExperiment file...",type="gears")

    w <- Waiter$new(html =  tagList(
      spin_flower(), # Una bella animazione a fiore
      h3("Creating SingleCellExperiment file...", style = "color:white;")
    ), color = "rgba(0, 0, 0, 0.5)") # Nero al 50% di trasparenza)

    shinyjs::disable("create_sce") # Disabilita subito il tasto
    w$show()

    on.exit({
      w$hide()
      shinyjs::enable("create_sce")
    }, add = TRUE)


    fcs_pro <- flowCore::flowSet(datafcs$data)

    sce.orig <- CATALYST::prepData(fcs_pro, paneldata(), metadata(), features = paneldata()$fcs_colname)

    sendmessages("SingleCellExperiment file created!", "success")
    # sendmessages("Now you can download it or proceed with the analysis.", "info")

    sce_data(sce.orig)
  })

  #check data correctly loaded
  output$check_sce_data = reactive(
    return(!is.null(sce_data()))
  )
  outputOptions(output, "check_sce_data", suspendWhenHidden = FALSE)



  #### Filter SCE
  output$plot_filt_sce = renderPlot({
    req(sce_data())
    CATALYST::plotCounts(sce_data(), group_by = "patient_id", color_by = "condition")
  })

  observeEvent(input$show_modal_filtSCE,{
    req(sce_data())
    showModal(
      modalDialog(
        title = "Filter SCE",
        fluidRow(
          column(9, plotOutput("plot_filt_sce")),
          column(3, selectInput("pat_filt_sce","Samples to remove", choices = levels(sce_data()@colData$patient_id),multiple =TRUE),
                 br(),
                 div(style="text-align:center;",actionButton("run_filt_pat_sce","Update SCE!", icon("gear"),style ="padding:10px; font-size:130%;")))
        ),
        size = "xl"
      )
    )
  })

  observeEvent(input$run_filt_pat_sce,{
    if(length(input$pat_filt_sce)>0){
      showModal(
        modalDialog(
          title = "Warning",
          paste("Do you want to remove the samples and update the SCE?"),
          footer = tagList(modalButton("No"),  actionButton("confirm_filt_pat_sce", "Yes")),
          easyClose = FALSE
        )
      )
    }else{
      sendmessages("No sample selected for filtering.",type="danger")
    }

  })

  observeEvent(input$confirm_filt_pat_sce, {
    req(sce_data())

    removeModal()

    sce<- CATALYST::filterSCE(sce_data(), !patient_id %in% input$pat_filt_sce) ## ELIMINO CAMPIONI
    sendmessages(paste("Removed",length(input$pat_filt_sce),"samples from the SCE."),type="success")
    sce_data(sce)
  })



  output$download_sce <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("SCE_", Sys.Date(), ".rds")
    },
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {

      w <- Waiter$new(html =  tagList(
        spin_flower(), # Una bella animazione a fiore
        h3("Preparing the .rds file... The download will start shortly. Please wait and do not refresh the page.", style = "color:white;")
      ), color = "rgba(0, 0, 0, 0.5)") # Nero al 50% di trasparenza)

      shinyjs::disable("download_sce") # Disabilita subito il tasto
      w$show()

      on.exit({
        w$hide()
        shinyjs::enable("download_sce")
      }, add = TRUE)


      saveRDS(sce_data(), file)
    }
  )



########################################## CLUSTERING #######################################################

  run_clustering_impl <- function() {
    req(sce_data())
    sendmessages("Start clustering...", type = "gear")

    w <- Waiter$new(html = tagList(
      spin_flower(),
      h3("Running clustering...", style = "color:white;")
    ), color = "rgba(0, 0, 0, 0.5)")

    shinyjs::disable("run_cluster")
    w$show()
    on.exit({
      w$hide()
      shinyjs::enable("run_cluster")
    }, add = TRUE)

    sce <- CATALYST::cluster(sce_data(), features = "state", xdim = 10, ydim = 10, maxK = 20, seed = 1234)

    sendmessages("Clustering completed.", type = "success")
    sce_data(sce)
    sendmessages("The SCE object is updated.", type = "info")
  }


  observeEvent(input$run_cluster,{
    req(sce_data())

    if ("cluster_id" %in% colnames(sce_data()@colData)) {
      showModal(modalDialog(
        title = "Clustering already detected",
        "It seems that clustering has already been performed. Do you want to overwrite it?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_cluster", "Yes, overwrite", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    }else{
      run_clustering_impl()
    }
  })

  observeEvent(input$confirm_cluster, {
    req(sce_data())
    removeModal()
    run_clustering_impl()
  })


  #se step1 è stato caricato. false if null
  output$check_clusterdata = reactive({
    req(sce_data())
    return("cluster_id" %in% colnames(sce_data()@colData))
  })
  outputOptions(output, "check_clusterdata", suspendWhenHidden = FALSE)


  output$down_cluster <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("SCE_clustered_", Sys.Date(), ".rds")
    },
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {

      w <- Waiter$new(html =  tagList(
        spin_flower(),
        h3("Preparing the .rds file... The download will start shortly. Please wait and do not refresh the page.", style = "color:white;")
      ), color = "rgba(0, 0, 0, 0.5)") # Nero al 50% di trasparenza

      shinyjs::disable("down_cluster")
      w$show()

      on.exit({
        w$hide()
        shinyjs::enable("down_cluster")
      }, add = TRUE)

      saveRDS(sce_data(), file)
    }
  )



  output$heatmap_clust = renderPlot({
    req(sce_data())
    validate(need("cluster_id" %in% colnames(sce_data()@colData), "Clustering is required."))

    CATALYST::plotExprHeatmap(sce_data(), features = "state",
                              by = "cluster_id", k = input$metak_clust,
                              bars = input$bar_heatmap, perc = input$perc_heatmap, fun = input$fun_clust)
  })



  ########################################## DIFFERENTIAL EXPRESSION ########################################################


  observeEvent(sce_data(),{

    updatePickerInput(session, "DE_picker_mark",choices = CATALYST::state_markers(sce_data()),selected = CATALYST::state_markers(sce_data()))
  })


  DE_results = eventReactive(input$run_DE,{
    req(sce_data())
    validate(need("cluster_id" %in% colnames(sce_data()@colData), "Clustering is required."))


    clusters <- levels(CATALYST::cluster_ids(sce_data(), k = input$DE_selcluster))
    shinyjs::disable("run_DE")

    all_results <- list()

    percentage <- 0
     withProgress(message = "Computing Dunn's Test...", value=0, {
      for (cl in clusters) {
        sce1 <- filterSCE(sce_data(), cluster_id %in% cl, k = input$DE_selcluster)

        agg <- scuttle::aggregateAcrossCells(sce1,
                                             subset.row = input$DE_picker_mark,
                                             ids = sce1$sample_id,
                                             use.assay.type = "exprs",
                                             statistics = input$type_aggr_DE) #far scegliere mean/median o quello che c'è

        for (marker_to_check in input$DE_picker_mark) {
          df_sub <- data.frame(
            expr = SummarizedExperiment::assay(agg, "exprs")[marker_to_check, ],
            group = sce1$condition[match(colnames(agg), sce1$sample_id)]
          )

          fit <- kruskal.test(expr ~ group, data = df_sub)

          if (fit$p.value < input$expdes_thresh) {

            suppressMessages(
              dunn_res <- FSA::dunnTest(expr ~ group, data = df_sub, method = "bh")
            )
            posthoc_results <- dunn_res$res


            means <- tapply(df_sub$expr, df_sub$group, mean)
            group1 <- sapply(strsplit(posthoc_results$Comparison, " - "), "[", 1)
            group2 <- sapply(strsplit(posthoc_results$Comparison, " - "), "[", 2)
            delta <- round(unname(means[group1] - means[group2]),3)
            log2fc <- round(unname(log2(means[group1] + 1e-6) - log2(means[group2] + 1e-6)),3)

            all_results[[paste(cl, marker_to_check, sep = "_")]] <- data.frame(
              cluster = cl,
              marker = marker_to_check,
              kruskal_p = round(fit$p.value,3),
              comparison = posthoc_results$Comparison,
              Z = round(posthoc_results$Z,3),
              p_adj = round(posthoc_results$P.adj,3),
              delta_exprs = delta,
              Log2FC = log2fc
            )

          }
        }
        percentage <- percentage + 1/length(clusters) * 100
        incProgress(1/length(clusters), detail = paste0("Progress: ", round(percentage,0), " %"))
      }

    })
    final_results <- if(length(all_results) > 0) do.call(rbind, all_results) else NULL
    final_results = dplyr::filter(final_results, p_adj < input$expdes_thresh)

    on.exit({
      shinyjs::enable("run_DE")
    }, add = TRUE)


    return(final_results)

  })




  output$check_DEdata = reactive({
    req(DE_results())
    return(!is.null(DE_results()))
  })
  outputOptions(output, "check_DEdata", suspendWhenHidden = FALSE)


  output$DE_table = renderDT({
    req(DE_results())
    datatable(
      DE_results(),
      escape = FALSE,
      selection = "single", # Permette di selezionare una sola riga alla volta
      rownames = FALSE,
      caption = 'Significative comparisons.',
      options = list(dom = 'tp', pageLength = 10,scrollX = TRUE) # 't' nasconde la barra di ricerca per pulizia
    )
  },server=T)



  output$DE_boxplot = renderPlot({
    req(DE_results(), sce_data())

    validate(need(input$DE_table_rows_selected,"No sample selected. Click on a row in the table."))

    selected_row <- input$DE_table_rows_selected


    sce1 <- filterSCE(sce_data(), cluster_id %in% DE_results()$cluster[selected_row], k = input$DE_selcluster)

    agg <- scuttle::aggregateAcrossCells(sce1,
                                         subset.row = DE_results()$marker[selected_row],
                                         ids = sce1$sample_id,
                                         use.assay.type = "exprs",
                                         statistics = input$type_aggr_DE)
    df_sub <- data.frame(
      expr = SummarizedExperiment::assay(agg, "exprs")[DE_results()$marker[selected_row],],
      group = sce1$condition[match(colnames(agg), sce1$sample_id)]
    )

    # Boxplot per tutti i marker e cluster
    ggplot(df_sub, aes(x = group, y = expr, fill = group)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, size = 1) +
      labs(title = paste("Cluster", DE_results()$cluster[selected_row], "-", DE_results()$marker[selected_row]),
           x = "Condition", y = "Mean Expression") +
      theme_minimal(base_size = 16)

  })


  output$down_DE <- downloadHandler(
    filename = function() {
       sprintf("DE_%s_%s_pval%s_%s.csv",input$DE_selcluster,input$type_aggr_DE,input$expdes_thresh,Sys.Date())
    },
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      write.csv(DE_results(), file, row.names = FALSE)
    }
  )



  ########################################## DIMENSIONALITY REDUCTION #######################################################


  run_pca_impl <- function() {
    req(sce_data())
    sendmessages("Start dimension reduction...", type = "gear")

    w <- Waiter$new(html = tagList(
      spin_flower(),
      h3("Running dimension reduction...", style = "color:white;")
    ), color = "rgba(0, 0, 0, 0.5)")

    shinyjs::disable("run_pca")
    w$show()
    on.exit({
      w$hide()
      shinyjs::enable("run_pca")
    }, add = TRUE)

    sce <- CATALYST::runDR(sce_data(), dr = input$type_pca, cells = input$ncells_rd, features = "state")

    sendmessages("Dimension reduction completed.", type = "success")
    sce_data(sce)
    sendmessages("The SCE object is updated.", type = "info")
  }


  observeEvent(input$run_pca,{
    req(sce_data())

    if(length(SingleCellExperiment::reducedDimNames(sce_data()))>0){
      showModal(modalDialog(
        title = "Dimension reduction already detected",
        paste("It seems that a dimension reduction has already been performed (",SingleCellExperiment::reducedDimNames(sce_data()),
              "). Do you want to overwrite it?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_pca", "Yes, overwrite", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    }else{
      run_pca_impl()
    }
  })

  observeEvent(input$confirm_cluster, {
    req(sce_data())
    removeModal()
    run_pca_impl()
  })


  #se step1 è stato caricato. false if null
  output$check_pcadata = reactive({
    req(sce_data())
    return(length(SingleCellExperiment::reducedDimNames(sce_data()))>0)
  })
  outputOptions(output, "check_pcadata", suspendWhenHidden = FALSE)


  output$down_pca <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      if("cluster_id" %in% colnames(sce_data()@colData)){
        paste0("SCE_clustered_DR_", Sys.Date(), ".rds")
      }else{
        paste0("SCE_",paste0(SingleCellExperiment::reducedDimNames(sce_data()),collapse="_"),"_", Sys.Date(), ".rds")
      }
    },
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {

      w <- Waiter$new(html =  tagList(
        spin_flower(),
        h3("Preparing the .rds file... The download will start shortly. Please wait and do not refresh the page.", style = "color:white;")
      ), color = "rgba(0, 0, 0, 0.5)") # Nero al 50% di trasparenza

      shinyjs::disable("down_pca")
      w$show()

      on.exit({
        w$hide()
        shinyjs::enable("down_pca")
      }, add = TRUE)

      saveRDS(sce_data(), file)
    }
  )

  observeEvent(sce_data(),{
    req(sce_data())
    if(length(SingleCellExperiment::reducedDimNames(sce_data()))>0){
      updateSelectInput(session, "type_pca_plot",choices = SingleCellExperiment::reducedDimNames(sce_data()))
    }
    if(input$typevar_colorpca == "coldata"){
      updateSelectInput(session, "var_colorpca", choices= colnames(sce_data()@colData))
    }else{
      updateSelectInput(session, "var_colorpca", choices= rownames(sce))
    }
    updateSelectInput(session, "facet_pcaplot", choices= c("None", colnames(sce_data()@colData)))

  })

  output$plot_pca = renderPlot({
    req(sce_data(),input$facet_pcaplot,input$type_pca_plot,input$var_colorpca)
    validate(need(length(SingleCellExperiment::reducedDimNames(sce_data()))>0, "Dimension reduction is required."))

    facetvar <- if (input$facet_pcaplot == "None") { NULL } else { input$facet_pcaplot }

    p <- CATALYST::plotDR(sce_data(), input$type_pca_plot, color_by = input$var_colorpca, facet_by = facetvar, scale= input$scale_pcaplot)

    p$layers[[1]]$aes_params$size <- 2  # <--- Imposta qui la dimensione dei punti

    p + theme(
      axis.title = element_text(size = 16, face = "bold"), # Titoli assi (X e Y)
      axis.text = element_text(size = 14),                # Numeri sugli assi
      legend.text = element_text(size = 14),              # Testo della legenda
      legend.title = element_text(size = 16),             # Titolo della legenda
      strip.text = element_text(size = 16)                # Testo dei facet (se presenti)
    )

  })


}
