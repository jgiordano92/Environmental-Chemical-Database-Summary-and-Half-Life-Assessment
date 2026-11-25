#' Generate "decline" plots and anlysis (compare DAT to residue concentration when more than one harvest was performed in the trial)
#' 
#' Find the decline trials - trials with same Trial.Trt.ID and Crop.Standard, but variable DATs
#' Create curve plot for each decline trial (DAT on x axis, residue on y axis)
#'
#' @param data Residue dataset, cleaned and crop name standardized
#'
#' @param output_dir decline graphs. Divided by crop, up to 8 graphs per page
#'
#' @import ggplot2
#' @importFrom dplyr filter group_by summarise n_distinct pull
#'
#' @export
generate_trial_dat_plots <- function(
    data,
    output_dir = "Trial DAT Visualizations"
) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  crops <- sort(unique(data$Crop.Standard))
  
  for (crop in crops) {
    
    crop_data <- data %>%
      dplyr::filter(Crop.Standard == crop)
    
    # Identify trials with multiple distinct DAT values
    qualifying_trials <- crop_data %>%
      dplyr::group_by(Trial.Trt.ID) %>%
      dplyr::summarise(
        n_DAT = dplyr::n_distinct(DAT..d, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_DAT > 1) %>%
      dplyr::pull(Trial.Trt.ID)
    
    if (length(qualifying_trials) == 0) {
      next
    }
    
    plots <- list()
    
    for (trial in qualifying_trials) {
      
      df <- crop_data %>%
        dplyr::filter(Trial.Trt.ID == trial)
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = DAT..d,
          y = Rpt.Conc.SE
        )
      ) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::scale_x_continuous(
          limits = c(0, NA)
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, NA)
        ) +
        ggplot2::labs(
          title = paste0("Trial: ", trial),
          x = "Days After Treatment (DAT)",
          y = "Residue, Parent Equivalent (mg/kg)"
        ) +
        ggplot2::theme_bw()
      
      plots[[length(plots) + 1]] <- p
    }
    
    # Split plots into groups of six (one PDF page per group)
    plot_groups <- split(
      plots,
      ceiling(seq_along(plots) / 6)
    )
    
    pdf_file <- file.path(
      output_dir,
      paste0(
        gsub("[^A-Za-z0-9]", "_", crop),
        ".pdf"
      )
    )
    
    grDevices::pdf(
      pdf_file,
      width = 14,
      height = 10
    )
    
    for (plot_group in plot_groups) {
      
      # Arrange four plots on top and two on bottom
      combined_plot <- patchwork::wrap_plots(
        plot_group,
        ncol = 4,
        nrow = 2
      ) +
        patchwork::plot_annotation(
          title = crop
        )
      
      print(combined_plot)
    }
    
    grDevices::dev.off()
    
    message("Created PDF: ", crop)
  }
  
  message(
    "All crop reports saved to: ",
    output_dir
  )
  
  invisible(NULL)
}