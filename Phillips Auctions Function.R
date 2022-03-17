# I'm using the most recent RStudio + R on rstudio.cloud, but this should also work on slightly older R/package versions

# install.packages("tidyverse")
# Installing this package will install all relevant packages

library(tidyverse)
library(rvest)

# Load currency and HTML elements reference tables
source("Reference Tables.R")

scrapePhillipsAuctions <- function(websiteUrl){
  
  htmlPage <- read_html(websiteUrl)
  
  
  # Get lot information
  lotAttributeDF <- htmlPage %>% 
    html_elements(".phillips-lot__description") %>% 
    html_children() %>% 
    html_attrs() %>% 
    tibble::enframe() %>% 
    unnest(cols = value) %>% 
    rename(`HTML Element` = value)
  
  lotAttributeTextDF <- htmlPage %>% 
    html_elements(".phillips-lot__description") %>% 
    html_children() %>% 
    html_text() %>% 
    tibble::enframe() %>% 
    rename(`Page Text` = value)
  
  lotDF <- lotAttributeDF %>% 
    left_join(lotAttributeTextDF) %>% 
    mutate(Lot = ifelse(`HTML Element` == "phillips-lot__description__lot-number-wrapper",
                        `Page Text`,
                        NA)) %>% 
    fill(Lot) %>% 
    right_join(elementNames) %>% 
    select(-`HTML Element`) %>% 
    pivot_wider(id_cols = Lot, 
                names_from = colNames, 
                values_from = `Page Text`)
  
  lotDF <- lotDF %>% 
    mutate(Estimate = str_extract_all(Estimate, "[\\d+,]+"),
           `Transaction Price` = str_extract(`Transaction Price`, "[\\d+,]+")) %>%  
    unnest_wider(col = Estimate,
                 names_sep = "") %>% 
    rename(`Low Estimate` = Estimate1,
           `High Estimate` = Estimate2) %>% 
    mutate_at(vars(`Low Estimate`, `High Estimate`, `Transaction Price`), str_remove_all, "[,]") %>% 
    mutate_at(vars(-c(`Artist Name`, `Title of Work`)), as.numeric) %>% 
    mutate(`Auction Site` = websiteUrl)
  
  
  # Get and combine general auction information
  auctionName <- htmlPage %>% 
    html_elements(".auction-page__hero__title") %>% 
    html_text() %>% 
    enframe(name = NULL, 
            value = "Auction Name")
  
  auctionLocationDate <- htmlPage %>% 
    html_elements(".auction-page__hero__date") %>% 
    html_text() %>% 
    enframe(name = NULL) %>% 
    separate(col = value, 
             into = c("Location", "Auction Date"), 
             sep = " Auction ")
  
  auctionTable <- bind_cols(auctionName, auctionLocationDate)
  
  auctionTable <- auctionTable %>% # works for one day and two days?
    mutate(`Start Date` = str_remove(`Auction Date`, "(-\\d+)"),
           `End Date` = str_remove(`Auction Date`, "(\\d+-)")) %>% 
    mutate_at(vars(`Start Date`, `End Date`), as.Date, tryFormats = c("%d %B %Y")) %>% 
    mutate(`Auction House` = "Phillips")
  
  
  # Determine if auction results are over several days
  # This would have to be changed for online auctions
  isMultiDaySale <- str_count(auctionLocationDate$`Auction Date`, "\\b\\d{1,2}\\b") > 1
  
  ## Multi day function
  multiDayFunction <- function(){
    
    daysRows <- htmlPage %>% 
      html_elements(".auction-details .row") %>% 
      html_children() %>% 
      html_text() %>% 
      enframe(name = NULL, value = "Daily Auction Name") %>% 
      unnest(cols = `Daily Auction Name`)
    
    daysRows <- daysRows %>% 
      bind_cols(auctionTable) %>% 
      mutate(`Date of Sale` = str_extract(`Daily Auction Name`, paste0("\\d{1,2}\\s", paste0(month.name, collapse = "|"))),
             lots = str_extract(`Daily Auction Name`, "(?<=(lots.)).*") %>% str_extract_all("\\d+")) %>% 
      mutate(`Date of Sale` = paste(`Date of Sale`, lubridate::year(`Start Date`))) %>% 
      mutate(`Date of Sale` = as.Date(`Date of Sale`, tryFormats = c("%d %B %Y"))) %>% 
      unnest_wider(col = lots) %>% 
      rename(`Lot Min` = `...1`,
             `Lot Max` = `...2`) %>% 
      mutate_at(vars(starts_with("Lot")), as.numeric) %>% 
      mutate(`Auction Site` = websiteUrl)
    
    return(daysRows)
    
  }
  
  
  ## Single day function
  singleDayFunction <- function(){
    
    daysRows <- tibble("Daily Auction Name" = NA) %>%
      bind_cols(auctionTable) %>%
      mutate(`Date of Sale` = `Auction Date`) %>%
      mutate(`Date of Sale` = as.Date(`Date of Sale`, tryFormats = c("%d %B %Y"))) %>% 
      mutate(`Lot Min` = min(lotDF$Lot),
             `Lot Max` = max(lotDF$Lot)) %>% 
      mutate(`Auction Site` = websiteUrl)
    
    return(daysRows)
    
  }
  
  
  daysDF <- switch(isMultiDaySale + 1, singleDayFunction(), multiDayFunction())
  
  # Shape final output for lots
  finalOutput <- lotDF %>%
    left_join(daysDF, by = "Auction Site") %>%
    left_join(select(currencyDF, `House Location`, Currency), by = c("Location" = "House Location")) %>%
    filter(Lot >= `Lot Min`& Lot <= `Lot Max`) %>%
    select(`Auction House`, `Auction Name`, Location, Lot, `Artist Name`, `Title of Work`, `Date of Sale`,
           `Local Currency` = Currency, `Low Estimate`, `High Estimate`, `Transaction Price`)
  
  
  return(finalOutput)
}

# Test UK, NYC, HK auctions
phillipsLinks <- c("https://www.phillips.com/auctions/auction/NY030422",
                   "https://www.phillips.com/auctions/auction/NY010122",
                   "https://www.phillips.com/auctions/auction/UK010222",
                   "https://www.phillips.com/auctions/auction/HK010321")

scrapePhillipsAuctions(phillipsLinks[4])
# FYI - There are warnings/messages for output, which are OK

finalOutput <- map_dfr(phillipsLinks, scrapePhillipsAuctions)

#