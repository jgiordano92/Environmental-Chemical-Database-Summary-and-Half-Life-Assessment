Clean_Chemical_Data <- function(data) {
  
  data_subset <- dplyr::select(
    data,
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
  
  return(data_subset)
}