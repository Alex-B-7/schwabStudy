#When running fIns() the output is x which is a tribble of the 3 data files
#Then fSchwabStudy is created with the tbblIns parameter
#At the end of the script we set tbblIns <- fIns() so the fSchwabStudy function can access the information in the tribble
#Then we create 3 variables to access the 3 files in the tribble
#tbblIns$myData accesses all 3 files
#tbblIns$myData[tbblIns$source == "schwab"] only accesses the schwab file
#tbblIns$myData[tbblIns$source == "schwab"][[1]] grabs the first element with the source 'schwab'
#if we had more rows in the tribble with the source 'schwab' we could access those by using [[2]], [[3]], etc.

require(tidyverse); library(readxl); library(magrittr); library(dplyr); library(scales);

fIns <- function(){
  fInsSchwabMVFromINfovisa <- function(){
    fReadMVIF <- function(sheet_name) {
      tbblMVIF1 <- read_excel(sPath, sheet = 'mvFromInfovisa', skip = 2, range = 'L3:R3000')
      tbblMVIF <- tbblMVIF1 %>%
        filter(rowSums(is.na(.)) <= 6)
      # filter(ProcessTypeDesc == "Mutual Fund" & rowSums(is.na(.)) <= 6)
        return(tbblMVIF)
    }

    cat(paste0('schwabStudy::fInsSchwab() Running ...  ', Sys.time(), '\n'))
    sPath <- '~/Library/CloudStorage/OneDrive-TheTrustCompany/Shared Documents - Team - Investments/Sprints/TrustCompanyCompletedAssetReview20240502.xlsm'
    tbblMVIF <- fReadMVIF('mvFromInfovisa')
    cat(paste0('schwabStudy::fInsSchwab() Complete.  ', Sys.time(), '\n'))
    return(tbblMVIF)
  }
  fInsSchwabMF <- function(){
    fReadMF <- function(sheet_name) {
      tbblMF1 <- read_excel(sPath, sheet = 'Mutual Funds', skip = 6, range = 'A7:U1000')
      tbblMF <- tbblMF1 %>%
        filter(rowSums(is.na(.)) <= 10) %>%
        rename('Cusip' = 'CUSIP', 'Ticker' = 'Schwab Symbol', 'Restrict' = 'Schwab Restrictions') %>%
        select('Cusip', 'Ticker', 'Restrict')
      return(tbblMF)
    }

    cat(paste0('schwabStudy::fInsSchwab2() Running ...  ', Sys.time(), '\n'))
    sPath <- '~/Library/CloudStorage/OneDrive-TheTrustCompany/Shared Documents - Team - Investments/Sprints/TrustCompanyCompletedAssetReview20240502.xlsm'
    tbblMF <- fReadMF('Mututal Funds')
    cat(paste0('schwabStudy::fInsSchwab2() Complete.  ', Sys.time(), '\n'))
    return(tbblMF)
  }
  fInsInfovisaTrades <- function(){
    cat(paste0('schwabStudy::fInsInfovisaTrades() Running ...  ', Sys.time(), '\n'))
    sFile <- "~/Library/CloudStorage/OneDrive-TheTrustCompany/Shared Documents - Team - Investments/Sprints/InfovisaTrades.txt"
    tbbl <- read.delim(sFile, sep = '|', header = TRUE) %>% as_tibble()
    tbbl %<>% select('AssetID','TradeTypeID','Cash', 'Shares','SharePrice', 'OriginalCost')
    cat(paste0('schwabStudy::fInsInfovisaTrades() Complete.  ', Sys.time(), '\n'))
    return(tbbl)
  }
  fInsInfovisaAssets <- function(){
    cat(paste0('schwabStudy::fInsInfovisaAssets() Running ...  ', Sys.time(), '\n'))
    sFile <- '~/Library/CloudStorage/OneDrive-TheTrustCompany/Shared Documents - Team - Investments/Sprints/DAILY_S_SQL_TTC_VIEW_ASSET.txt'
    tbbl <- read.delim(sFile, sep = "|", header = TRUE) %>% as_tibble() %>%
      rename('Ticker' = 'TickerSymbol') %>%
      select('Ticker', 'AssetName', 'Cusip', 'ProcessTypeDesc', 'AssetID')
    cat(paste0('schwabStudy::fInsInfovisaAssets() Complete.  ', Sys.time(), '\n'))
    return(tbbl)
  }



  cat(paste0("schwabStudy::fSchwabStudy() Running ...  ", Sys.time(), '\n'))
  x <- tribble( ~source, ~myData
                ,'mvFromInfovisa', fInsSchwabMVFromINfovisa()
                ,'mfFromSchwab', fInsSchwabMF()
                ,'ivTrades', fInsInfovisaTrades()
                ,'ivAssets', fInsInfovisaAssets()
  )
  cat(paste0("schwabStudy::fSchwabStudy() Complete.  ", Sys.time(), '\n'))
  return(x)
}

fSchwabStudy <- function(tbblIns){

  tbbl_mvFromInfovisa <- tbblIns %>%
    filter(str_detect(source, 'mvFromInfovisa')) %>%
    pull(myData) %>%
    first()

  tbbl_mfFromSchwab <- tbblIns %>%
    filter(str_detect(source, 'mfFromSchwab')) %>%
    pull(myData) %>%
    first()

  tbbl_ivAssets <- tbblIns %>%
    filter(str_detect(source, 'ivAssets')) %>%
    pull(myData) %>%
    first()

  tbbl_ivTrades <- tbblIns %>%
    filter(str_detect(source, 'ivTrades')) %>%
    pull(myData) %>%
    first()


  merge1 <- tbbl_mvFromInfovisa %>%
    left_join(tbbl_mfFromSchwab)


  merge2 <- merge1 %>%
    left_join(tbbl_ivAssets)

#COLLAB ON THE CODE BLOCK BELOW


  tbblRestrict <- merge1 %>%
    group_by(Restrict) %>%
    summarize(total_mv = sum(mv, na.rm = TRUE)) %>%
    arrange(desc(total_mv)) %>%
    mutate(total_mv = dollar(total_mv, prefix = '$'))



  # Don't worry about trades yet .. this data will change.
  merged_all <- merge2 %>%
    left_join(tbbl_ivTrades)

  return(merged_all)

}
#sick
tbblIns <- fIns()

merged_all <- fSchwabStudy(tbblIns)
