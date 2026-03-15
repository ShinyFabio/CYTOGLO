#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFiles
#' @importFrom flowCore read.FCS Subset write.FCS flowSet
#' @importFrom PeacoQC PeacoQC
#' @importFrom fs path_home
#' @import ggcyto
#' @import openCyto
#' @import ggplot2
#' @importFrom shinyWidgets updatePickerInput show_alert
#' @importFrom DT renderDT datatable
#' @importFrom CATALYST prepData
#' @importFrom readxl read_excel
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


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

  #import SCE
  observe({
    req(input$filesce_input)
    ext <- tools::file_ext(input$filesce_input$name)
    if(ext != "rds"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .rds file", type = "error")
    }
    validate(need(ext == "rds", "Invalid file! Please upload a .rds file"))
    file = readRDS(file = input$filesce_input$datapath)
    if(!is.null(file)){
      sendmessages("SingleCellExperiment successfully loaded!",type="success")
      sce_data(file)
    }
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
    stato_campioni(df)
  },once = TRUE)

  ######################################################### STEP 2 PEACOQC ###########################################



  observeEvent(datafcs$data, {
    # req(datafcs$data)
    current_selected <- input$picker_peacoqc
    all_choices <- names(datafcs$data)
    updatePickerInput(session, "picker_peacoqc",choices = all_choices,selected = intersect(current_selected, all_choices))
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
      sendmessages(title = "Select at least one sample before applying the correction.", type = "danger")
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

    percentage <- 0
    withProgress(message = "Processing data...", value=0, {
      results_2 = lapply(file_list, function(i){

        if(i %in% input$picker_peacoqc){
          cat("Processing sample:", i, "\n")
          peacoqc_result <- PeacoQC::PeacoQC(
            datafcs$data[[i]],
            save_fcs =F,
            output_directory = folder_plot, #da testare se salva i plot
            channels = c(1:7), #da far scegliere "Time" "Event_length"       "Center"        "Width"     "Residual"       "Offset"    "Amplitude"
            IT_limit = 0.6,
            remove_zeros = input$remv_0_peaco, #da far scegliere
            time_units = 50000
          )
          percentage <<- percentage + 1/length(input$picker_peacoqc)*100
          incProgress(1/length(input$picker_peacoqc), detail = paste0("Progress: ",round(percentage,0), " %"))
          ff_pulito <- peacoqc_result$FinalFF
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

  observeEvent(input$first_var_gat,{
    req(step1_sample())
    tt = as.data.frame(step1_sample()@exprs)[[input$first_var_gat]]
    qnts = unname(quantile(tt, probs = c(0.25, 0.75), na.rm = TRUE))
    if(qnts[1] == qnts[2]){qnts[2] <- qnts[2]+1}
    updateSliderInput(session, "slider_x_gat", min = floor(min(tt)),  max = ceiling(max(tt)), value = qnts)
  })

  observeEvent(input$second_var_gat,{
    req(step1_sample())
    tt = as.data.frame(step1_sample()@exprs)[[input$second_var_gat]]
    qnts = unname(quantile(tt, probs = c(0.25, 0.75), na.rm = TRUE))
    if(qnts[1] == qnts[2]){qnts[2] <- qnts[2]+1}
    updateSliderInput(session, "slider_y_gat", min = floor(min(tt)),  max = ceiling(max(tt)), value = qnts)
  })

  # PLOT SENZA GATING
  output$plot_gating_before = renderPlot({
    req(step1_sample(), input$first_var_gat)
    if(input$nvars_gating != 'one variable') req(input$second_var_gat)

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
    if (input$nvars_gating == 'one variable') {
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

    # 6. Build Gate Settings DataFrame
    new_settings <- data.frame(
      sample           = gated_data@description$GUID,
      n_in             = nrow(step1_sample()@exprs),
      n_gated          = nrow(gated_data@exprs),
      VarX             = input$first_var_gat,
      VarY             = if(input$nvars_gating == 'two variables') input$second_var_gat else NA,
      type_gate        = if(input$nvars_gating == 'two variables') input$type_gating else NA,
      minRangeX        = if(input$nvars_gating == 'two variables' && input$type_gating == "Rectangular") min(input$slider_x_gat) else NA,
      maxRangeX        = if(input$nvars_gating == 'two variables' && input$type_gating == "Rectangular") max(input$slider_x_gat) else NA,
      minRangeY        = if(input$nvars_gating == 'two variables' && input$type_gating == "Rectangular") min(input$slider_y_gat) else NA,
      maxRangeY        = if(input$nvars_gating == 'two variables' && input$type_gating == "Rectangular") max(input$slider_y_gat) else NA,
      quantile_ellipse = if(input$nvars_gating == 'two variables' && input$type_gating != "Rectangular") input$slider_ellipse_gat else NA,
      quantile_1var    = if(input$nvars_gating == 'one variable') input$slider_1var_gat else NA,
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
    paneldata(panel)
    sendmessages("Panel data file loaded correctly.", "success")
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
    md$sample_id <- factor(md$sample_id, levels = md$sample_id[order(md$condition)])
    md$condition <- factor(md$condition)

    sendmessages("Metadata file loaded correctly.", "success")
    metadata(md)
  })



  observeEvent(input$create_sce,{
    req(datafcs$data, paneldata(),metadata())
    #create flowset

    # saveRDS(datafcs$data,"fcs_pro.rds")

    fcs_pro <- flowCore::flowSet(datafcs$data)


    #crea oggetto su cui lavorerai
    sce.orig <- CATALYST::prepData(fcs_pro, paneldata(), metadata(), features = paneldata()$fcs_colname)
    sendmessages("SingleCellExperiment file created!", "success")
    sendmessages("Now you can download it or proceed with the analysis.", "info")

    sce_data(sce.orig)
  })


  #check data correctly loaded
  output$check_sce_data = reactive(
    return(!is.null(sce_data()))
  )
  outputOptions(output, "check_sce_data", suspendWhenHidden = FALSE)


  output$download_sce <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("SCE_", Sys.Date(), ".rds")
    },
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      saveRDS(sce_data(), file)
    }
  )






}
