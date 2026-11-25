#' Summarize Normalized Residue
#'
#' Calculates summary statistics for normalized residue concentrations
#' grouped by Crop.Standard and Analyte.
#'
#' Summary statistics:
#'   Count (N)
#'   Minimum
#'   25th Percentile (P25)
#'   Median
#'   75th Percentile (P75)
#'   Maximum
#'   Standard Deviation (SD)
#'
#' Statistics are calculated from the Normalized.Residue column.
#'
#' If the observation corresponding to the minimum, 25th percentile,
#' median, 75th percentile, or maximum is censored, an asterisk (*)
#' is included in the output.
#'
#' @param data A data frame containing Crop.Standard, Analyte,
#'   Normalized.Residue, and Censored columns.
#'
#' @return A data frame containing summary statistics for each
#'   Crop.Standard and Analyte combination.
#'
#' @export

summarize_normalized_residue <- function(data) {
  
  format_stat <- function(value, censored) {
    if (censored) {
      paste0(round(value, 4), "*")
    } else {
      as.character(round(value, 4))
    }
  }
  
  data %>%
    dplyr::group_by(Crop.Standard, Analyte) %>%
    dplyr::group_modify(~{
      
      x <- .x$Normalized.Residue
      cens <- .x$Censored
      
      ord <- order(x)
      
      x_sorted <- x[ord]
      cens_sorted <- cens[ord]
      
      n <- length(x_sorted)
      
      min_idx <- 1
      p25_idx <- ceiling(0.25 * n)
      med_idx <- ceiling(0.50 * n)
      p75_idx <- ceiling(0.75 * n)
      max_idx <- n
      
      tibble::tibble(
        N = n,
        
        Min = format_stat(
          min(x_sorted, na.rm = TRUE),
          cens_sorted[min_idx]
        ),
        
        P25 = format_stat(
          stats::quantile(x_sorted,
                          probs = 0.25,
                          na.rm = TRUE),
          cens_sorted[p25_idx]
        ),
        
        Median = format_stat(
          stats::median(x_sorted,
                        na.rm = TRUE),
          cens_sorted[med_idx]
        ),
        
        P75 = format_stat(
          stats::quantile(x_sorted,
                          probs = 0.75,
                          na.rm = TRUE),
          cens_sorted[p75_idx]
        ),
        
        Max = format_stat(
          max(x_sorted,
              na.rm = TRUE),
          cens_sorted[max_idx]
        ),
        
        SD = round(
          stats::sd(x_sorted,
                    na.rm = TRUE),
          4
        )
      )
      
    }) %>%
    dplyr::ungroup()
  
}