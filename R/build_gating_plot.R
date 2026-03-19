#' @title build_gating_plot
#'
#' @description Generate Gating Plots for Flow Cytometry
#'
#' @param sample A flowFrame object.
#' @param n_vars Character, either 'one variable' or 'two variables'.
#' @param var_x Character, the name of the X-axis channel.
#' @param var_y Character, the name of the Y-axis channel (optional for 1D).
#' @param gating_type Character, 'Rectangular' or 'Ellipse' (for 2D).
#' @param apply_gate Logical, if TRUE apply the gate and compute the plot.
#' @param slider_1v Numeric, quantile value for 1D gating.
#' @param side_qnt_1v Character, either 'left' or 'right' corresponding to the side of cut.
#' @param slider_x Numeric vector of length 2 for rectangular X limits.
#' @param slider_y Numeric vector of length 2 for rectangular Y limits.
#' @param ellipse_val Numeric, quantile for flowClust 2D gating.
#'
#' @return A ggplot or ggcyto object.
#'
#' @import ggplot2
#' @import ggcyto
#' @import openCyto
#'

# Funzione helper interna al server (o fuori)
build_gating_plot <- function(sample, n_vars, var_x, var_y, gating_type,
                              slider_1v = NULL, side_qnt_1v = "left", slider_x = NULL, slider_y = NULL,
                              ellipse_val = NULL, apply_gate = FALSE) {

  # --- LOGICA 1 VARIABILE ---
  if(n_vars == 'One variable' && !is.null(slider_1v)) {
    df <- data.frame(value = as.data.frame(sample@exprs)[[var_x]])
    cut <- quantile(sample@exprs[, var_x], slider_1v)
    max_y <- max(density(sample@exprs[, var_x])$y)

    if(apply_gate) {

      if(side_qnt_1v == "left"){
        #questo salva i dati da cut a Inf
        g <- rectangleGate(.gate = setNames(list(c(cut,Inf)), var_x))
      }else{
        #questo salva i dati da -Inf a cut
        g <- rectangleGate(.gate = setNames(list(c(-Inf,cut)), var_x))
      }

      gated_data <- Subset(sample, g)
      df_gated <- data.frame(value = as.data.frame(gated_data@exprs)[[var_x]])
      p <- ggplot(df_gated, aes(x = value)) + geom_density(fill = "lightblue") +
        coord_cartesian(xlim = c(0, quantile(df_gated$value, 0.999, na.rm = TRUE))) +
        labs(title = "Sample post-gating")

    }else{
      p <- ggplot(df, aes(x = value)) + geom_density(fill = "lightblue") +
        coord_cartesian(xlim = c(0, quantile(df$value, 0.999, na.rm = TRUE))) +
        labs(title = "Sample pre-gating") +
        geom_vline(xintercept = cut, color = "red") +
        annotate("text", x = cut, y = max_y, label = slider_1v,
                 size = 4.5, color = "blue", angle = 90, vjust = 0)
    }

  }else{
    # --- LOGICA 2 VARIABILI ---
    if(gating_type == "Rectangular") {
      g <- openCyto:::.boundary(sample, channels = c(var_x, var_y),
                                min = c(min(slider_x), min(slider_y)),
                                max = c(max(slider_x), max(slider_y)))
    } else {
      g <- openCyto:::gate_flowclust_2d(sample, xChannel = var_x, yChannel = var_y, K = 1, quantile = ellipse_val)
    }


    if(apply_gate) {
      p <- autoplot(Subset(sample, g), x = var_x, y = var_y, bins = 100,max_nrow_to_plot = Inf) +
        labs(title = "Sample post-gating")

    }else{
      p <- autoplot(sample, x = var_x, y = var_y, bins = 100,max_nrow_to_plot = Inf) + geom_gate(g) + geom_stats() +
        labs(title = "Sample pre-gating")
    }
  }

  return(p)

}
