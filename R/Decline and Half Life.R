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
generate_decline_plots <- function(
    data,
    output_dir = "Decline Curves"
) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  crops <- sort(unique(data$Crop.Standard))
  
  for (crop in crops) {
    
    crop_data <- data %>%
      dplyr::filter(Crop.Standard == crop)
    
    # Identify trials meeting all criteria
    qualifying_trials <- crop_data %>%
      dplyr::group_by(Trial.Trt.ID) %>%
      dplyr::summarise(
        n_DAT = dplyr::n_distinct(DAT..d, na.rm = TRUE),
        all_censored = all(Censored, na.rm = FALSE),
        has_early_DAT = any(DAT..d <= 5, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        n_DAT > 1,
        !all_censored,
        has_early_DAT
      ) %>%
      dplyr::pull(Trial.Trt.ID)
    
    if (length(qualifying_trials) == 0) {
      next
    }
    
    plots <- list()
    
    for (trial in qualifying_trials) {
      
      trial_data <- crop_data %>%
        dplyr::filter(Trial.Trt.ID == trial)
      
      analytes <- sort(unique(trial_data$Analyte))
      analytes <- analytes[!is.na(analytes)]
      
      for (analyte in analytes) {
        
        df <- trial_data %>%
          dplyr::filter(Analyte == analyte)
        
        # Skip analytes with no variation in DAT
        if (dplyr::n_distinct(df$DAT..d, na.rm = TRUE) <= 1) {
          next
        }
        
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
            title = paste0(
              "Trial: ", trial,
              " | ", analyte
            ),
            x = "Days After Treatment (DAT)",
            y = "Residue Concentration (mg/kg)"
          ) +
          ggplot2::theme_bw()
        
        plots[[length(plots) + 1]] <- p
      }
    }
    
    if (length(plots) == 0) {
      next
    }
    
    # Split plots into groups of six
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