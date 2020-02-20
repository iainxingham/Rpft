# Extract lung function data from pdf files

library(pdftools)
library(stringr)
library(lubridate)

lung_func <- list(FEV1 = list('FEV1', 'FEV1.*'),
                  FVC = list('FVC', 'FVC.*'), 
                  TLco = list('TLco', 'TLco.*'),
                  VAsb = list('VAsb', 'VAsb.*'),
                  KCO = list('KCO', 'KCO.*'),
                  FRC = list('FRC', 'FRC.*'),
                  VC = list('VC', '(?<!F)VC.*'),
                  TLC = list('TLC', 'TLC.*'),
                  RV = list('RV', 'RV .*'),
                  RV_TLC = list('RV_TLC', 'RV/TLC.*'))

extractlungfunc <- function(measurement, regex_str, txt) {
  if(measurement == 'FEV1')
    datstr <- str_match_all(str_match(txt, regex_str)[1], '(-?\\d{1,3}\\.?\\d{0,2})')[[1]][-1,1]
  else if(measurement == 'VC') {
    tempstr <- str_match_all(str_match_all(txt, regex_str)[[1]], '(-?\\d{1,3}\\.?\\d{0,2})')
    if(length(tempstr) == 0) return (NA)
    if(length(tempstr) == 1) datstr <- tempstr[[1]][,1]
    else datstr <- tempstr[[2]][,1]
  }
  else
    datstr <- str_match_all(str_match(txt, regex_str)[1], '(-?\\d{1,3}\\.?\\d{0,2})')[[1]][,1]
  
  datnum <- sapply(datstr, as.numeric)
  if(length(datstr) == 4) ret <- list(Predicted = datnum[1],
                                      Measured_pre = datnum[2],
                                      Percent_pred_pre = datnum[3],
                                      SR_pre = datnum[4])
  else if(length(datstr) == 3) ret <- list(Predicted = datnum[1],
                                           Measured_pre = datnum[2],
                                           Percent_pred_pre = datnum[3])
  else if(length(datstr) == 8) ret <- list(Predicted = datnum[1],
                                           Measured_pre = datnum[2],
                                           Percent_pred_pre = datnum[3],
                                           Measured_post = datnum[4],
                                           Percent_pred_post = datnum[5],
                                           Percent_change = datnum[6],
                                           SR_pre = datnum[7],
                                           SR_post = datnum[8])
  else ret <- NA
  
  return (ret)
}

extractPFT <- function(txt) {
  ret <- list(rxr = str_to_upper(str_match(txt, "Patient ID:[\\s]*(((rxr)|(RXR))(\\d){7})")[1,2]),
              date = dmy(str_match(txt, "Study Date:[\\s]*(\\d{1,2}[\\\\/\\.:]\\d{1,2}[\\\\/\\.:]\\d{2,4})")[1,2]),
              dob = dmy(str_match(txt, "Birth Date:[\\s]*(\\d{1,2}[\\\\/\\.:]\\d{1,2}[\\\\/\\.:]\\d{2,4})")[2,2]),
              height = as.numeric(str_match(txt, "Height: (\\d{2,3}(\\.\\d)?) cm")[1,2]),
              weight = as.numeric(str_match(txt, "Weight: (\\d{2,3}(\\.\\d)?) kg")[1,2]),
              lname = str_to_sentence(str_match(txt, "Last Name:[\\s]*([A-Za-z]+)")[1,2]),
              fname = str_to_sentence(str_match(txt, "First Name:[\\s]*([A-Za-z]+)")[1,2]),
              sex = str_to_sentence(str_match(txt, "Gender:[\\s]*((Male)|(MALE)|(Female)|(FEMALE))")[1,2])
              )
  
  c(ret, lapply(lung_func, function(x) {extractlungfunc(x[[1]], x[[2]], txt)}))
}