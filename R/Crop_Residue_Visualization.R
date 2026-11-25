#' Visualize Residue Results per standardized crop per each of the three analytes
#'
#' @param data standardized residue dataset.
#' @param output_dir directory where visualizations.
#' @param min_n minimum observations required for a crop-analyte
#'   combination to be plotted.
#'
#' @return various graphs
#'
#' @import ggplot2
#' @import patchwork
#'
#' @export
generate_crop_pdf_reports <- function(
    data,
    output_dir = "Chemical X Visualizations",
    min_n = 5
) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  crops <- sort(unique(data$Crop.Standard))
  
  for (crop in crops) {
    
    crop_data <- data %>%
      dplyr::filter(Crop.Standard == crop)
    
    analyte_counts <- crop_data %>%
      dplyr::count(Analyte)
    
    if (!any(analyte_counts$n >= 5)) {
      next
    }
    
    analytes <- sort(unique(crop_data$Analyte))
    
    analytes <- analytes[
      !is.na(analytes)
    ]
    
    analytes <- analytes[
      vapply(
        analytes,
        function(a) {
          sum(crop_data$Analyte == a, na.rm = TRUE) >= 5
        },
        logical(1)
      )
    ]

    pdf_file <- file.path(
      output_dir,
      paste0(
        crop,
        ".pdf"
      )
    )
    
    pdf(
      pdf_file,
      width = 14,
      height = 10
    )
    
    for (analyte in analytes) {
      
      df <- crop_data %>%
        dplyr::filter(Analyte == analyte)
      
      if (nrow(df) < min_n) {
        next
      }

      title_text <- paste0(
        crop,
        " | ",
        analyte,
        " (n = ",
        nrow(df),
        ")"
      )
      
      n_method <- dplyr::n_distinct(df$Method)
      
      n_continent <- dplyr::n_distinct(df$TSite.Continent)
      
      n_form <- dplyr::n_distinct(df$Form..Type)
      
      p1 <- ggplot(
        df,
        aes(
          x = Appl..Rate,
          y = Rpt.Conc.SE
        )
      ) +
        geom_point(alpha = 0.6) +
        scale_x_continuous(limits = c(0, NA)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(
          title = "Single Application Rate vs Residue",
          x = "Application Rate (kg ai/ha)",
          y = "Residue, Parent Equivalent (mg/kg)"
        ) +
        theme_bw()
      
      p2 <- ggplot(
        df,
        aes(
          x = Seasonal.Appl..Rate,
          y = Rpt.Conc.SE
        )
      ) +
        geom_point(alpha = 0.6) +
        scale_x_continuous(limits = c(0, NA)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(
          title = "Seasonal Application Rate vs Residue",
          x = "Seasonal Application Rate (kg ai/ha)",
          y = "Residue (Paren Equivalent (mg/kg)"
        ) +
        theme_bw()
      
      p3 <- ggplot(
        df,
        aes(
          x = DAT..d,
          y = Normalized.Residue
        )
      ) +
        geom_point(alpha = 0.6) +
        scale_x_continuous(limits = c(0, NA)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(
          title = "Harvest Interval (DAT) vs Normalized Residue",
          x = "DAT",
          y = "Residue Normalized to 2.0 kg ai/ha, Parent Equivalent (mg/kg)"
        ) +
        theme_bw()
      
      if (n_method >= 2) {
        p4 <- ggplot(
          df,
          aes(
            x = Method,
            y = Normalized.Residue
          )
        ) +
          geom_boxplot(outlier.alpha = 0.6) +
          labs(
            title = "Method of Application (Foliar or Soil) vs Normalized Residue",
            x = "Application Method",
            y = "Residue Normalized to 2.0 kg ai/ha, Parent Equivalent (mg/kg)"
          ) +
          theme_bw()
      } else {
        p4 <- ggplot() + theme_void()
      }
      
      if (n_continent >=2) {
        p5 <- ggplot(
          df,
          aes(
            x = TSite.Continent,
            y = Normalized.Residue
          )
        ) +
          geom_boxplot(outlier.alpha = 0.6) +
          labs(
            title = "Continent of Data Generation vs Normalized Residue",
            x = "Continent",
            y = "Residue Normalized to 2.0 kg ai/ha, Parent Equivalent (mg/kg)"
          ) +
          theme_bw()
      } else {
        p5 <- ggplot() + theme_void()
      }
      
      if (n_form >= 2) {
        p6 <- ggplot(
          df,
          aes(
            x = Form..Type,
            y = Normalized.Residue
          )
        ) +
          geom_boxplot(outlier.alpha = 0.6) +
          labs(
            title = "Formulation Type vs Normalized Residue",
            x = "Formulation Type",
            y = "Residue Normalized to 2.0 kg ai/ha, Parent Equivalent (mg/kg)"
          ) +
          theme_bw()
      } else {
        p6 <- ggplot() + theme_void()
      }
      
      combined_plot <-
        (p1 + p2) /
        (p3 + p4) /
        (p5 + p6) +
        patchwork::plot_annotation(
          title = title_text
        )
      
      print(combined_plot)
      
    }
    
    dev.off()
    
    message(
      "Created PDF: ",
      crop
    )
    
  }
  
  message(
    "All crop reports saved to: ",
    output_dir
  )
  
}