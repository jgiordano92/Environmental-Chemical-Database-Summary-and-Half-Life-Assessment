#' Clean Chemical Data
#'
#' Removes control treatments and trials where the necessary application or residue concentration information is missing
#' Converts countries to continents
#' Gives all application rate units in kg ai/ha, including unit conversion where necessary
#' Gives seasonal application rate by multiplying application rate by number of applications
#' Combined crop and commodity into new column
#' Combines Trial ID and Treatment number into new column (unique trial ID)
#' Simplify application method into "Foliar" or "Soil"
#' Identifies numeric residue concentrations and "censored" residue concentrations (i.e, non-detectable ND or below the limit of quantification <xxx)
#' Changes censored residue concentrations to numeric upper end (i.e., at the limit of detection for reported as ND and at the limit of quantitation for <xxx)
#' For uncensored residue concentrations, converts Derivative A and Derivative B into parent chemical equivalent molar concentrations. In these cases, it changes description of analyte from Derivative A/B to Derivative A/B (parent equivalents)
#' Adds normalized residue concentration to a seasonal application rate of 2.0 kg ai/ha (for uncensored residue values)
#' Filters out columns with information not needed for our assessment
#'
#' @param data A dataframe containing chemical residue study data.
#'
#' @return A dataframe containing the selected variables for analysis, excluding control samples and organizing data in a useful way.
#'
#' @examples
#' cleaned_data <- Clean_Chemical_Data(test_data)
#'
#' @export


Clean_Chemical_Data <- function(data) {
  
  data_subset <- data %>%
    dplyr::filter(
      Form..Type != "Control",
      Appl..Rate != "",
      Rpt.Conc.SE != ""
    ) %>%
    
    #Convert country to Continent
    dplyr::mutate(
      TSite.Continent = dplyr::case_when(
        TSite.Country %in% c("Brazil", "Argentina") ~ "South America",
        TSite.Country == "United States" ~ "North America",
        TSite.Country == "South Africa" ~ "Africa",
        TSite.Country %in% c(
          "Belgium",
          "France",
          "Germany",
          "Greece",
          "Italy",
          "Spain"
        ) ~ "Europe",
        TRUE ~ TSite.Country
      )
    ) %>%
    
    #Convert all units of application rate to kg ai/ha
    dplyr::mutate(
      Appl..Rate = dplyr::case_when(
        Appl..Rate.UOM == "g ai/ha" ~ Appl..Rate / 1000,
        Appl..Rate.UOM == "lb ai/acre" ~ Appl..Rate * 1.12,
        TRUE ~ Appl..Rate
      ),
      
      Appl..Rate.UOM = dplyr::case_when(
        Appl..Rate.UOM == "g ai/ha" ~ "Kg ai/ha",
        Appl..Rate.UOM == "lb ai/acre" ~ "Kg ai/ha",
        TRUE ~ Appl..Rate.UOM
      ),
      
      #Give seasonal application rate
      Seasonal.Appl..Rate = dplyr::case_when(
        No..of.Trt.s. == 2 ~ Appl..Rate * 2,
        TRUE ~ Appl..Rate
      )
    ) %>%
    
    # Combine Crop and Commodity
    dplyr::mutate(
      Crop.Fraction = paste(Crop, Commodity, sep = " ")
    ) %>%
    
    # Combine Trial ID and Trt Num
    dplyr::mutate(
      Trial.Trt.ID = paste(Trial.ID, Trt.Num, sep = ".")
    ) %>%
    
    #Categorize application method
    dplyr::mutate(
      Method = dplyr::case_when(
        grepl("Foliar", Method, ignore.case = TRUE) ~ "Foliar",
        grepl("Base|Floor|Soil", Method, ignore.case = TRUE) ~ "Soil",
        TRUE ~ Method
      )
    ) %>%
    
    # Preserve uncensored residue concentrations
    dplyr::mutate(
      Rpt.Conc.SE.Original = Rpt.Conc.SE
    ) %>%
    
    # Identify censored residue concentrations
    dplyr::mutate(
      Censored = grepl("<", Rpt.Conc.SE) |
        Rpt.Conc.SE == "ND"
    ) %>%
    
    # Convert concentrations to numeric values
    dplyr::mutate(
      Rpt.Conc.SE = dplyr::case_when(
        Rpt.Conc.SE == "ND" ~
          suppressWarnings(as.numeric(gsub("^<", "", LOD))),
        grepl("<", Rpt.Conc.SE) ~
          suppressWarnings(as.numeric(gsub("^<", "", Rpt.Conc.SE))),
        TRUE ~
          suppressWarnings(as.numeric(Rpt.Conc.SE))
      )
    ) %>%
    
    # Convert uncensored derivative concentrations to parent equivalents
    dplyr::mutate(
      Rpt.Conc.SE = dplyr::case_when(
        Analyte == "Derivative A" & !Censored ~
          Rpt.Conc.SE * 2.00,
        Analyte == "Derivative B" & !Censored ~
          Rpt.Conc.SE * 1.78,
        TRUE ~ Rpt.Conc.SE
      )
    ) %>%
    
    # Update analyte names
    dplyr::mutate(
      Analyte = dplyr::case_when(
        Analyte == "Derivative A" ~
          "Derivative A (parent equivalent)",
        Analyte == "Derivative B" ~
          "Derivative B (parent equivalent)",
        TRUE ~ Analyte
      )
    ) %>%
    
    # Calculate residue normalized to 2.0 kg ai/ha
    dplyr::mutate(
      Normalized.Residue = dplyr::case_when(
        Censored == FALSE ~
          round((2.0 / Seasonal.Appl..Rate) * Rpt.Conc.SE,3),
        TRUE ~ Rpt.Conc.SE
      )
    ) %>%
    
    # Convert DAT to numeric
      dplyr::mutate(
        DAT..d = suppressWarnings(as.numeric(DAT..d))
      ) %>%
    
    dplyr::select(
      TSite.Continent,
      Form..Type,
      Analyte,
      Trial.Trt.ID,
      Crop.Fraction,
      Method,
      Appl..Rate,
      Appl..Rate.UOM,
      No..of.Trt.s.,
      Seasonal.Appl..Rate,
      Rpt.Conc.SE,
      Rpt.Conc.SE.Original,
      Censored,
      DAT..d,
      LOD,
      Normalized.Residue
    )
}