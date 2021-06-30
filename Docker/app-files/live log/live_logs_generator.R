library(dplyr)
library(readr)
library(data.table)
library(stringr)

i <- 1

setwd(paste0(getwd(),"/" ,str_extract(readLines("live_logs_generator.conf")[6], "\\../.*")))

# command line arguments, if supplied
args <- commandArgs(trailingOnly = TRUE)

while (i <= 10000) {
  z <- tibble(ip_address = sample(c("69.162.124.228",
                                    "13.66.139.72",
                                    "23.100.232.233",
                                    "100.25.199.237",
                                    "185.191.171.36",
                                    "40.77.167.29",
                                    "200.18.160.23",
                                    "85.208.98.22",
                                    "66.249.64.139",
                                    "147.92.153.7",
                                    "64.62.252.163"), size = 1),
                         remote_user_ident = "-",
                         local_user_ident = "-",
                         timestamp = paste0(format(Sys.time(), format = "%d/Jun/%Y:%H:%M:%S %z")),
                         request = paste0(sample(c("GET", "HEAD", "POST", "PUT"), size = 1), 
                                          "  +45hsparql/.*",
                                          sample(c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK"), size = 1)),
                         status_code = sample(c("200", "201","301","401","403","404"), size = 1),
                         bytes_sent = round(runif(1)*10000),
                         referer = NA,
                         user_agent = sample(c("Mozilla", "Apache", "Chrome"), size = 1))
  tryCatch(
      expr =   {write_delim(z, delim = " ",
                           file =  paste0(getwd(),"/", str_extract(readLines("live_logs_generator.conf")[6], "\\../.*"), "live_logs.log"),
                           col_names = FALSE,
                           append = TRUE)},
      error = function(a){
        options(show.error.messages = FALSE)
        Sys.sleep(0.1)
      }
  )

  i = i +1
  
  if(length(args) == 0){
    Sys.sleep(str_extract(readLines("live_logs_generator.conf")[1], "\\d+\\.\\d+"))
  } else {
    Sys.sleep(as.numeric(args[1]))
  }
  
}
