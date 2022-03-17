
# Local Currency DF
currencyDF <- tibble(`House Location` = c("London", "New York", "Hong Kong"),
                     Currency = c("GBP", "USD", "HKD"),
                     `Currency Symbol` = c("£", "$", "HK$"))

# Specific HTML elements used on Phillips to map to output field names
elementNames <- tibble(`HTML Element` = c("phillips-lot__description__artist",
                                          "phillips-lot__description__title",
                                          "phillips-lot__description__estimate",
                                          "phillips-lot__sold"),
                       colNames = c("Artist Name",
                                    "Title of Work",
                                    "Estimate",
                                    "Transaction Price"))
