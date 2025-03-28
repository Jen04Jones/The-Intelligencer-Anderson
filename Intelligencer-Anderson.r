#Main Scraping

# Libraries 
library(httr)
library(jsonlite)
library(dplyr)

# API and variables
base_url <- "https://chroniclingamerica.loc.gov/search/pages/results/"
lccn <- "sn84026965"  # The Anderson Intelligencer's LCCN
keyword <- "textile"  # Search keyword

# Output path
 output_path <- "anderson_articles.csv" #columns by date


    write.csv(results %>% select(title, date, url, cleaned_text),
        output_path, row.names = FALSE)
          print(paste("Results saved to", output_path))


# Fetching Data
fetch_articles <- function(base_url, lccn, keyword = NULL, page_limit = 5) {
    all_results <- data.frame()
  
  for (page in 1:page_limit) {
    query_url <- paste0(
      base_url, "?lccn=", lccn,  #AI, March 28, 2025 help with lccn.
        if (!is.null(keyword)) paste0("&andtext=", keyword) else "",
         "&format=json&page=", page
    )
   print(paste("Fetching data from:", query_url))  # AI, March 28, 2025 helped Debugging
    
       response <- GET(query_url)
        if (status_code(response) == 200) {
            parsed_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
      
   # Check and print total available items
              if (!is.null(parsed_data$totalItems)) {
                print(paste("Total items available:", parsed_data$totalItems))
      }
      
      # Extract data if results are available
      if (!is.null(parsed_data$items) && length(parsed_data$items) > 0) {
        batch <- as.data.frame(parsed_data$items) %>%
          select(
            title = "title",               # Title of the article
            date = "date",                 # Publication date
            url = "url",                   # URL of the newspaper page
            ocr_eng = "ocr_eng"            # Text content (OCR) added here #AI, March 28, 2025 help with orc
          )
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

# AI March 27 2025 Copilot, help with gsub use. 
clean_text <- function(text) {
    if (is.null(text)) return(NA)  # Handle NULL values
  # Replace line breaks and unnecessary whitespace
      cleaned_text <- gsub("\n", " ", text)     # Replace newline characters with space
        cleaned_text <- gsub("\\s+", " ", cleaned_text) # Remove excessive spaces
          return(trimws(cleaned_text))             # Trim trailing spaces
}

# Main function to scrape articles and save as CSV
scrape_articles <- function(base_url, lccn, keyword, page_limit = 5) {
 
  results <- fetch_articles(base_url, lccn, keyword, page_limit)
  
  if (nrow(results) > 0) {
    print("Cleaning article text...")
   
    results$cleaned_text <- sapply(results$ocr_eng, clean_text)
    
   
 write.csv(results %>% select(title, date, url, cleaned_text), "anderson_articles_cleaned.csv", row.names = FALSE)
         print("Results saved to 'anderson_articles_cleaned.csv'") #Shouold make columns to separate articles by date. 
  }        else {
              print("No articles found.")
  }
  
  return(results)
}


page_limit <- 25 
  results <- scrape_articles(base_url, lccn, keyword, page_limit)

#print(head(results)) #For Testing
