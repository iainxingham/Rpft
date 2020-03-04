# Extract lung function data from pdf files

library(pdftools)
library(stringr)
library(lubridate)
library(DBI)
library(log4r)

lung_func <- list(FEV1 = list('FEV1', 'FEV1.*'),
                  FVC = list('FVC', 'FVC.*'), 
                  TLco = list('TLco', 'TLco.*'),
                  VAsb = list('VAsb', 'VAsb.*'),
                  KCO = list('KCO', 'KCO.*'),
                  FRC = list('FRC', 'FRC.*'),
                  VC = list('VC', '(?<!F)VC.*'),
                  TLC = list('TLC', 'TLC.*'),
                  RV = list('RV', 'RV .*'),
                  RV_TLC = list('RVTLC', 'RV/TLC.*'))

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

# Create database connection
dbcon <- dbConnect(RSQLite::SQLite(), "./data/pfts.sqlite")

dbDisconnect(dbcon)


updatepatient <- function(name, needed, source, con) {
  if(needed) {
    if(name %in% names(source) && !is.na(source[name])) {
      dbExecute(con, paste0('UPDATE patients SET ', 
                             name, ' = "', source[name], 
                             '" WHERE rxr = "' ,
                             source$rxr, '"'))
    }
  }
}

addpatient <- function(datlist, con) {
  existing <- dbGetQuery(con, paste0('SELECT * FROM patients WHERE rxr = "',
                                     datlist$rxr, '"'))

  if(nrow(existing) > 0) {
    # Add any extra details
    notthere <- sapply(existing, is.na)
    mapply(updatepatient, names(notthere), notthere, 
           MoreArgs = list(source=datlist, con=con))
  }
  else {
    dbAppendTable(con, "patients", 
                  as.data.frame(datlist[names(datlist) %in% 
                                          c("rxr", "lname", "fname", "sex", "dob")],
                                stringsAsFactors = FALSE))
  }
  
  # Enable pipeline
  return(datlist)
}

addprefixtoname <- function(x, prefix) {
  paste0(prefix, x)
}

sortspirovalnames <- function(vals, prefix) {

  x <- str_match(vals, "Measured_pre")
  if(!is.na(x[1,1])) return(paste0(prefix, "pre"))
  x <- str_match(vals, "Predicted")
  if(!is.na(x[1,1])) return(paste0(prefix, "pred"))  
  x <- str_match(vals, "Percent_pred_pre")
  if(!is.na(x[1,1])) return(paste0(prefix, "pre_percent_pred"))
  x <- str_match(vals, "Measured_post")
  if(!is.na(x[1,1])) return(paste0(prefix, "post"))
  
  x <- str_match(vals, "Percent_pred_post")
  if(!is.na(x[1,1])) return(paste0(prefix, "post_percent_pred"))
  x <- str_match(vals, "Percent_change")
  if(!is.na(x[1,1])) return(paste0(prefix, "percent_change"))  
  x <- str_match(vals, "SR_pre")
  if(!is.na(x[1,1])) return(paste0(prefix, "pre_SR"))
  x <- str_match(vals, "SR_post")
  if(!is.na(x[1,1])) return(paste0(prefix, "post_SR")) 
  
  return("Error")
}

# Add spirometry
addspiro <- function(datlist, con, sourceid) {
  rxr_id <- dbGetQuery(con, paste0('SELECT id FROM patients WHERE rxr = "',
                                   datlist$rxr, '"'))
  if(nrow(rxr_id) < 1) {
    # Error - no patient
    error(logs, paste("Missing patient id:", datlist$rxr, " --- addspiro()"))
    return(NA)
  }
  else {
    names(rxr_id) <- "subject_id"
    fev1s <- as.data.frame(datlist$FEV1, stringsAsFactors = FALSE)
    names(fev1s) <- lapply(names(fev1s), sortspirovalnames, prefix='fev1_')
    fvcs <- as.data.frame(datlist$FVC, stringsAsFactors = FALSE)
    names(fvcs) <- lapply(names(fvcs), sortspirovalnames, prefix='fvc_')
    names(sourceid) <- "source_id"
    study_date <- datlist$date
    names(study_date) <- "study_date"
    tbl <- cbind(rxr_id[1], fev1s, fvcs, sourceid, study_date)
    dbAppendTable(con, "spirometry", tbl)
  }
  
  return(dbGetQuery(con, "SELECT last_insert_rowid()")[1])
}

# Get list of files in subdirectories
list.files(".", pattern = "*.pdf", recursive = TRUE)
p1 <- pdf_text("data/pft2.pdf") %>%
  extractPFT()

purrr::walk(pftFiles, processFile, recordtype = 'FULL_PFT', con = dbcon)

# Logging
logs <- create.logger("data/pft.log", level = "DEBUG")

# Process a file
processFile <- function(pdf, recordtype, con) {
  p1 <- pdf_text(pdf)
  
  if(recordtype == 'FULL_PFT') {
    p1 <- extractPFT(p1)
    # Check if only spiro available
    if(is.na(p1$TLco) && is.na(p1$TLC)) recordtype = 'PFT_Spiro'
    
    addpatient(p1, con)
    sourceid <- addSourceFile(con, p1$rxr, pdf, recordtype)
    spiroid <- addspiro(p1, con, sourceid)
    if(is.na(spiroid)) {
      info(logs, paste("No spirometry in ", p1$rxr, " --- processFile()"))
      return(p1$rxr)
    }
    
    if(recordtype == 'FULL_PFT') {
      names(spiroid) <- "spiro_id"
      names(sourceid) <- "source_id"
      rxrid <- p1$rxr
      names(rxrid) <- "subject_id"
      studydate <- p1$date
      names(studydate) <- "study_date"
      vals <- purrr::map_dfc(c('TLco', 'VAsb', 'KCO', 'FRC', 'VC', 'TLC', 'RV', 'RV_TLC'), 
                           addLungFunc, datlist=p1)
      names(vals) <- sapply(names(vals), sortlungfuncvalnames)
      tbl <- cbind(rxrid, spiroid, sourceid, studydate, vals)
      print(tbl)
      dbAppendTable(con, "lungfunc", tbl)
    }
 
  }
  
  # Other record types here
  
}

addLungFunc <- function(measure, datlist) {
  if(!is.na(datlist[measure])) {
    phys <- as.data.frame(datlist[[measure]], stringsAsFactors = FALSE)
    names(phys) <- lapply(names(phys), addprefixtoname, 
                          prefix=paste0(tolower(measure), "_"))
  }
  else phys <- NULL
  
  return(phys)
}

# Add source for record
addSourceFile <- function(con, rxr, pdf, study_type){
  rxr_id <- dbGetQuery(con, paste0('SELECT id FROM patients WHERE rxr = "',
                                   rxr, '"'))
  if(nrow(rxr_id) < 1) {
    # Error - no patient
    error(logs, paste("Missing patient id:", rxr, " --- addSourceFile()"))
    return(NA)
  }
  
  study_id <- dbGetQuery(con, paste0('SELECT id FROM studies WHERE tag = "',
                                    study_type, '"'))
  if(nrow(study_id) < 1) {
    error(logs, paste("Invalid study tag:", study_type, " --- addSourceFile()"))
    return(NA)
  }
  
  extracttime <- as.character(now())
  names(rxr_id) <- "subject_id"
  names(study_id) <- "study_type"
  dbAppendTable(con, "datasource",
                data.frame(import_date = extracttime, 
                           source_file = pdf,
                           subject_id = rxr_id,
                           study_type = study_id,
                           stringsAsFactors = FALSE))
  
  ret <- dbGetQuery(con, paste0('SELECT id FROM datasource WHERE import_date = "',
                                extracttime, '"'))[1]
  
  return(ret)
}

sortlungfuncvalnames <- function(vals) {
  # Lung func
  # x_Measured_pre -> x
  # x_Predicted -> x_pred
  # x_Percent_pred_pre -> x_percent_pred
  # x_SR_pre -> x_SR
  # Other x -> drop

  x <- str_match(vals, "([:alnum:]*)_Measured_pre")
  if(!is.na(x[1,2])) return(x[1,2])
  x <- str_match(vals, "([:alnum:]*_)Predicted")
  if(!is.na(x[1,2])) return(paste0(x[1,2], "pred"))  
  x <- str_match(vals, "([:alnum:]*_)Percent_pred_pre")
  if(!is.na(x[1,2])) return(paste0(x[1,2], "percent_pred"))
  x <- str_match(vals, "([:alnum:]*_)SR_pre")
  if(!is.na(x[1,2])) return(paste0(x[1,2], "SR"))
  
  return("Error")
}