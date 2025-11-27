
#' Function to extract the latest Rdata, rds or cvs
#'
#' @param dat_dir: data directory 
#'
#' @return: The latest data file with the extension of RData, rds(default) or cvs
#' @export
#'
#' @examples: dt <- load_latest_file(dir_path = dat_dir, name_prefix = "data_manipulated", file_ext = "rds")
load_latest_file <- function(dir_path, name_prefix, file_ext) {
  # Clean extension (remove leading dot if present)
  file_ext <- sub("^\\.", "", file_ext)
  
  # Load readxl if needed
  if (tolower(file_ext) == "xlsx" && !requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read .xlsx files. Please install it with install.packages('readxl').")
  }
  
  # Build pattern: underscore optional between prefix and date
  pattern <- paste0("^", name_prefix, "_?", "\\d{4}-\\d{2}-\\d{2}\\.", file_ext, "$")
  
  # List matching files
  all_files <- list.files(path = dir_path, pattern = pattern, full.names = TRUE)
  
  if (length(all_files) == 0) {
    stop("No matching files found with prefix '", name_prefix, "' and extension '.", file_ext, "'")
  }
  
  # Extract dates
  date_pattern <- paste0(name_prefix, "_?", "(\\d{4}-\\d{2}-\\d{2})\\.", file_ext, "$")
  file_dates <- sub(paste0("^.*", date_pattern), "\\1", all_files)
  file_dates <- as.Date(file_dates, format = "%Y-%m-%d")
  
  # Get most recent file
  latest_file <- all_files[which.max(file_dates)]
  
  # Message
  message("Loaded file: ", basename(latest_file))
  
  # Load depending on extension
  switch(tolower(file_ext),
         rds = readRDS(latest_file),
         csv = read.csv(latest_file),
         rdata = {
           load(latest_file, envir = .GlobalEnv)
           invisible(NULL)
         },
         xlsx = readxl::read_excel(latest_file),
         stop("Unsupported file extension: ", file_ext)
  )
}

