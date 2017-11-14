require(readxl)
require(xts)

#Get sheet indices
sheets <- paste0("Sheet",2:11)
name = c("IT","FIN","ENG","HLTH","CONS","COND","UTIL","TELS","INDU","MATR")
#Set directory for reading and writing data (directory of raw data)

#dir <- '/Users/marcogancitano/Dropbox/SMIFRisk/RiskScripts/Factor_Model'
#setwd(dir)

#Lambda function to get all data
factor_data_list <- lapply(sheets,function(sheet){
  #Reads given worksheet
  x <- data.frame(read_excel('SectorData.xlsx',sheet = sheet),stringsAsFactors = F)
  
  #Converting df to xts
  dates <- as.Date(as.numeric(x[-1,1]),origin = '1899-12-30')
  colnames(x) <- x[1,]
  dat <- x[-1,]
  
  #Warning is fine, just converting N/A to NA
  data_for_xts <- suppressWarnings(data.frame(sapply(dat[,2:ncol(dat)],as.numeric),stringsAsFactors = F))
  
  xts_data <- xts(data_for_xts,order.by = dates)
  
  #Get yield compared to ratios #zh: LONG TERM PE ADDED
  xts_data[,c('INDX_ADJ_PE','INDX_PX_BOOK','LONG_TERM_PRICE_EARNINGS_RATIO')] <- 1/xts_data[,c('INDX_ADJ_PE','INDX_PX_BOOK','LONG_TERM_PRICE_EARNINGS_RATIO')]
  
  #Get rolling vol/mean for these
  EBIT_vol <- rollapply(xts_data$EBIT,FUN = sd,width = 30)
  EBIT_mu <- rollapply(xts_data$EBIT,FUN = mean,width = 30)
  
  xts_data$EBIT <- EBIT_vol/EBIT_mu
  
  CF_vol <- rollapply(xts_data$CASH_FLOW_PER_SH,FUN = sd,width = 30)
  CF_mu <- rollapply(xts_data$CASH_FLOW_PER_SH,FUN = mean,width = 30)
  
  xts_data$CASH_FLOW_PER_SH <- CF_vol/CF_mu
  
  #Get what columns are all NA
  del_idx <- as.numeric(which(sapply(xts_data, FUN = function(x)all(is.na(x)))))
  
  #Delete
  ret_data <- xts_data[,-del_idx]
  
  #Get what columns that have 'BEST' in name
  best_idx <- which(grepl('BEST',colnames(ret_data)))
  
  #Deletes, then gets rid of NAs (created by CF & EBIT calc), and returns for the lambda function
  as.data.frame(na.omit(ret_data[,-best_idx]))
})

#Save list as r data
save(factor_data_list,file = 'factor_data_list.rdata')
load("factor_data_list.rdata")

names(factor_data_list) <- name

