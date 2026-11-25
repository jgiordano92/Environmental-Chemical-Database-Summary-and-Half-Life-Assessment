#' Standardize Crop Names
#'
#' Standardizes crop names, combining crops with synonymous names in database and resolve spelling
#' differences and formatting differences
#' 
#' @param data A data frame containing a Crop.Fraction column.
#'
#' @return A data frame with an added Crop.Standard column
#' with the standardized crop names.
#'
#' @details
#' New standardization rules can be added by including additional
#' grepl() conditions within dplyr::case_when()
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
        
        
        grepl("dried|dry", Crop.Fraction, ignore.case = TRUE) &
          grepl("beans", Crop.Fraction, ignore.case = TRUE) ~
          "Dry Bean Seed",
        
        grepl("field corn|maize", Crop.Fraction, ignore.case = TRUE) &
          grepl("fodder|stover", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Stover",
        
        grepl("field corn|maize", Crop.Fraction, ignore.case = TRUE) &
          grepl("foliage|forage|upper plant|whole plant|stalks", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Forage",
        
        grepl("field corn|maize", Crop.Fraction, ignore.case = TRUE) &
          grepl("silage", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Silage",
        
        grepl("field corn distillate", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Distillate",
        
        grepl("field corn germ", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Germ",
        
        grepl("field corn|maize", Crop.Fraction, ignore.case = TRUE) &
          grepl("grain", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Grain",
        
        grepl("field corn grits", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Grits",
        
        grepl("field corn hulls", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Hulls",
        
        grepl("field corn meal", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Meal",
        
        grepl("field corn oil", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Oil",
        
        grepl("field corn press cake", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Press Cake",
        
        grepl("field corn screenings", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Screenings",
        
        grepl("field corn soapstock", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Soapstock",
        
        grepl("field corn|maize", Crop.Fraction, ignore.case = TRUE) &
          grepl("cob|ear", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Cob",
        
        grepl("maize kernels", Crop.Fraction, ignore.case = TRUE) ~
          "Field Corn Kernels",
       
         grepl("potato tuber", Crop.Fraction, ignore.case = TRUE) ~
          "Potato Tuber",
        
        grepl("grapes", Crop.Fraction, ignore.case = TRUE) ~
          "Grape",
        
        grepl("soybean|soya bean", Crop.Fraction,
              ignore.case = TRUE) &
          grepl("seed", Crop.Fraction,
                ignore.case = TRUE) ~
          "Soybean Seed",
        
        TRUE ~ Crop.Fraction
        
      )
    )
  
}
