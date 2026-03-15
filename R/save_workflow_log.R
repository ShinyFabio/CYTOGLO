#' @title save_workflow_log
#'
#' @description This function saves a single text file containing both PeacoQC and Gating information. Each section is clearly labeled with a title for readability.
#'
#' @param output_path Character. The directory where the log file will be saved.
#' @param peac_df Data frame. The PeacoQC results to include in the log.
#' @param gat_df Data frame. The Gating results to include in the log.
#' @param filename Character. The name of the log file. Default is "workflow_log.txt".
#'
#'

save_workflow_log <- function(output_path, peac_df, gat_df, filename = "workflow_log.txt") {

  # Percorso completo del file
  log_file <- file.path(output_path, filename)

  # Apri il file in scrittura
  con <- file(log_file, "w")

  # --- Sezione PeacoQC ---
  cat("===== PeacoQC =====\n\n", file = con)
  write.table(
    peac_df,
    file = con,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )
  cat("\n\n", file = con)

  # --- Sezione Gating ---
  cat("===== Gating =====\n\n", file = con)
  write.table(
    gat_df,
    file = con,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  # Chiudi il file
  close(con)

  message("Workflow log saved to: ", log_file)
}
