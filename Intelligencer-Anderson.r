#Main Scraping

library(dplyr)

# API and variables
base_url <- "https://chroniclingamerica.loc.gov/search/pages/results/"
lccn <- "sn84026965"  # The Anderson Intelligencer's LCCN
keyword <- "textile"  # Search keyword

# Output
output_path <- "github.com/Jen04Jones/Anderson-Mills/anderson_articles.csv"

write.csv(results %>% select(title, date, url, cleaned_text),
          output_path, row.names = FALSE)
print(paste("Results saved to", output_path))


# Fetching Data
fetch_articles <- function(base_url, lccn, keyword = NULL, page_limit = 5) {
  all_results <- data.frame()
  
  for (page in 1:page_limit) {
    query_url <- paste0(
      base_url, "?lccn=", lccn,
      if (!is.null(keyword)) paste0("&andtext=", keyword) else "",
      "&format=json&page=", page
    )
    print(paste("Fetching data from:", query_url))  # Debugging message
    
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
            ocr_eng = "ocr_eng"            # Text content (OCR)
          )
        all_results <- bind_rows(all_results, batch)
        print(paste("Fetched", nrow(batch), "records from page", page))
      } else {
        print("No more results found
