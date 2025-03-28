#Main Scraping

# Libraries 
library(httr)
library(jsonlite)
library(dplyr)

# Search API
base_url <- "https://chroniclingamerica.loc.gov/search/pages/results/"
lccn <- "sn84026965"  # The Anderson Intelligencer's LCCN
keyword <- "textile"  # Search keyword

# Output CSV path
output_path <- "/Users/jenjones/Digitalhistory/Anderson-textiles 
_news/anderson_articles.csv"

# fetch article 
fetch_articles <- function(base_url, lccn, keyword = NULL, page_limit = 5) {
  all_results <- data.frame()
  
  for (page in 1:page_limit) {
    query_url <- paste0(
      base_url, "?lccn=", lccn,

  # AI (2025, March 24). Copilot

      if (!is.null(keyword)) paste0("&andtext=", keyword) else "",
      "&format=json&page=", page
    )
    print(paste("Fetching data from:", query_url))  # Check data
    
    
    # Get the text in UTF-8
    response <- GET(query_url)
    if (status_code(response) == 200) {
      parsed_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)


      # Check and if null
      if (!is.null(parsed_data$totalItems)) {
        print(paste("Total items available:", parsed_data$totalItems))
      }
      
      # Results
      if (!is.null(parsed_data$items) && length(parsed_data$items) > 0) {
        batch <- as.data.frame(parsed_data$items) %>%
          select(
            title = "title",               # Title of the article
            date = "date",                 # Publication date
            url = "url",                   # URL of the newspaper page
            ocr_eng = "ocr_eng"            # Text content (OCR) # suggestion by Copilot
          )
        # AI (2025, March 24). Copilot.
        all_results <- bind_rows(all_results, batch)
        print(paste("Fetched", nrow(batch), "records from page", page))
      } else {
        print("No more results found on this page.")
        break
      }
    } else {
      warning(paste("Request failed for page:", page, "Status code:", status_code(response)))
      break
    }
  }
  
  return(all_results)
}

# AI (2025, March 24). Copilot. -gsub use for cleaning

# Clean OCR Text
clean_text <- function(text) {
  if (is.null(text)) return(NA) 

    cleaned_text <- gsub("[[:space:]]+", " ", cleaned_text)
    cleaned_text <- gsub("[\r\n]+", " ", text)


        return(trimws(cleaned_text))             # Clean up
}

# Save to csv file
scrape_articles <- function(base_url, lccn, keyword, page_limit = 5) {
  
  results <- fetch_articles(base_url, lccn, keyword, page_limit)
  
  if (nrow(results) > 0) {
    print("Cleaning article text...")


    results$cleaned_text <- sapply(results$ocr_eng, Text_Content)
    
  # Keep Columns
    write.csv(results %>% select(title, Date, url, cleaned_text), "anderson_articles.csv", row.names = FALSE)
    print("Results saved to 'anderson_articles.csv'")
  } else {
    print("No articles found. Check your query parameters or try a broader search.")
  }
  
  return(results)
}

# Scrapping
page_limit <- 20  # Increase this number for more data
results <- scrape_articles(base_url, lccn, keyword, page_limit)


