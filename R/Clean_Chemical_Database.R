#' Clean Chemical Data
#'
#' Removes control treatments and selects the variables needed for
#' residue data analysis from a chemical residue database.
#'
#' @param data A dataframe containing chemical residue study data.
#'
#' @return A dataframe containing the selected variables for analysis, excluding control samples.
#'
#' @examples
#' cleaned_data <- Clean_Chemical_Data(test_data)
#'
#' @export


Clean_Chemical_Data <- function(data) {
  
  data %>%
    dplyr::filter(Form..Type != "Control") %>%
    dplyr::select(
      TSite.Country,
      Reg..Region,
      Form..Type,
      Trial.Type,
      Analyte,
      LOQ,
      Study.ID,
      Trial.ID,
      Crop,
      Method,
      Spray.Vol.,
      Spray.Vol..UOM,
      Appl..Rate,
      Appl..Rate.UOM,
      No..of.Trt.s.,
      Commodity,
      Rpt.Conc.SE,
      DAT..d,
      Growth.Stage.at.Trt.s.,
      Trt.Num
    )
}