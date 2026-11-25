#' Standardize Crop Names
#'
#' Standardizes crop names by identifying key crop and commodity terms
#' within the \code{Crop.Fraction} column and assigning a consistent
#' crop description. This approach is intended to resolve spelling
#' differences, formatting differences, and alternative naming conventions
#' while preserving meaningful commodity information (e.g., Forage,
#' Grain, Seed, Silage).
#'
#' Standardization is performed using keyword matching with
#' \code{grepl()} and \code{dplyr::case_when()}.
#'
#' Examples:
#' \itemize{
#'   \item "Millet forage" -> "Millet Forage"
#'   \item "Cottonseed" -> "Cotton Seed"
#'   \item "Cotton Seed, Undelinted" -> "Cotton Seed"
#'   \item "Soya bean seed" -> "Soybean Seed"
#'   \item "Maize forage" -> "Field Corn Forage"
#'   \item "Field corn forage" -> "Field Corn Forage"
#' }
#'
#' @param data A data frame containing a \code{Crop.Fraction} column.
#'
#' @return A data frame with an added \code{Crop.Standard} column
#' containing standardized crop names.
#'
#' @details
#' Crop names are standardized using rule-based pattern matching.
#' New standardization rules can be added by including additional
#' \code{grepl()} conditions within the corresponding
#' \code{dplyr::case_when()} statement.
#'
#' @export

standardize_crop_names <- function(data) {
  
  data %>%
    dplyr::mutate(
      Crop.Standard = dplyr::case_when(
        
        grepl("millet", Crop.Fraction, ignore.case = TRUE) &
          grepl("forage", Crop.Fraction, ignore.case = TRUE) ~
          "Millet Forage",
        
        grepl("millet", Crop.Fraction, ignore.case = TRUE) &
          grepl("grain", Crop.Fraction, ignore.case = TRUE) ~
          "Millet Grain",
        
        grepl("millet", Crop.Fraction, ignore.case = TRUE) &
          grepl("straw", Crop.Fraction, ignore.case = TRUE) ~
          "Millet Straw",
        
        grepl("alfalfa", Crop.Fraction, ignore.case = TRUE) &
          grepl("forage", Crop.Fraction, ignore.case = TRUE) ~
          "Alflafa Forage",
        
        grepl("alfalfa", Crop.Fraction, ignore.case = TRUE) &
          grepl("hay", Crop.Fraction, ignore.case = TRUE) ~
          "Alflafa Hay",
        
        grepl("clover", Crop.Fraction, ignore.case = TRUE) &
          grepl("forage", Crop.Fraction, ignore.case = TRUE) ~
          "Clover Forage",
        
        grepl("clover", Crop.Fraction, ignore.case = TRUE) &
          grepl("hay", Crop.Fraction, ignore.case = TRUE) ~
          "Clover Hay",
        
        grepl("cottonseed|cotton seed", Crop.Fraction,
              ignore.case = TRUE) ~
          "Cotton Seed",
        
        grepl("cotton", Crop.Fraction, ignore.case = TRUE) &
          grepl("trash", Crop.Fraction, ignore.case = TRUE) ~
          "Cotton Gin Trash",
        
        grepl("soybean|soya bean", Crop.Fraction,
              ignore.case = TRUE) &
          grepl("seed", Crop.Fraction,
                ignore.case = TRUE) ~
          "Soybean Seed",
        
        TRUE ~ Crop.Fraction
        
      )
    )
  
}
