# Scrape cheese data from www.cheese.com
# Load packages -----------------------------------------------------------
pacman::p_load(rvest,
               tidyr,
               purrr,
               stringr)

# Required scraping functions ---------------------------------------------
# Extract fat content from a cheese page
get_fat_content <- function(cheese_html) {
  fat_content <- cheese_html %>%
    html_nodes(css = ".fa-sliders + p") %>%
    html_text() %>%
    stringr::str_extract(pattern = "\\d+") %>%
    as.numeric()
  if (length(fat_content) == 0) fat_content <- NA
  fat_content
}

# Extract countries of origin from a cheese page
get_origin_countries <- function(cheese_html) {
  origin_countries <- cheese_html %>%
    html_nodes(css = ".fa-flag + p a") %>%
    html_text()
  if (length(origin_countries) == 0) origin_countries <- NA
  origin_countries
}

# Extract milk types from a cheese page
get_milk_types <- function(cheese_html) {
  milk_types <- cheese_html %>%
    html_nodes(css = ".fa-flask + p a") %>%
    html_text()
  if (length(milk_types) == 0) milk_types <- NA
  milk_types
}

# Extract flavors from a cheese page
get_flavors <- function(cheese_html) {
  flavors <- cheese_html %>%
    html_nodes(css = ".fa-spoon + p") %>%
    html_text() %>%
    stringr::str_extract_all(pattern = "(?<=[:space:])\\w+") %>%
    unlist()
  if (length(flavors) == 0) flavors <- NA
  flavors
}

cheese_scraper <- function(cheese_name) {
  # Need to format the cheese names to match the format used in the urls
  formatted_cheese_name <- cheese_name %>%
    iconv(to = "ASCII//TRANSLIT") %>% # Get rid of character accents
    str_to_lower() %>% # Make everything lowercase
    str_replace_all(pattern = "[’`'()&\"]", "") %>% # Remove other special chars
    str_replace_all(pattern = "[:space:]+", "-") # Replace spaces with "-"

  cheese_url <- paste0("https://www.cheese.com/", formatted_cheese_name)

  # Try to load the cheese page and extract the data
  # If it doesn't work, set everything as NA rather than throwing an error
  tryCatch(
    expr = {
      cheese_html <- read_html(cheese_url)

      milk_types <- get_milk_types(cheese_html)
      flavors <- get_flavors(cheese_html)
      origin_countries <- get_origin_countries(cheese_html)
      fat_content <- get_fat_content(cheese_html)

      list(
        name = cheese_name,
        origin_countries = origin_countries,
        milk_types = milk_types,
        flavors = flavors,
        fat_content = fat_content
      )
    },
    error = function(e) {
      warning(paste0("Not able to find ", cheese_url))
      list(
        name = cheese_name,
        origin_countries = NA,
        milk_types = NA,
        flavors = NA,
        fat_content = NA
      )
    }
  )
}

# Get the names of all the cheeses on a particular page
get_cheese_names_per_page <- function(page_url) {
  page_html <- read_html(page_url)
  cheese_names <- page_html %>%
    html_nodes(css = "h3 a") %>%
    html_text(trim = TRUE)
}

# Get the names of all the cheeses starting with a particular letter
get_cheeses_per_letter <- function(letter) {
  page_url <- paste0("https://cheese.com/alphabetical/?per_page=100&i=",
                     letter,
                     "&page=1")
  page_html <- read_html(page_url)

  # First, we need to know how many pages there are for this letter
  page_num <- page_html %>%
    html_nodes(css = "#id_page label") %>%
    html_text(trim = TRUE) %>%
    as.numeric() %>%
    tail(1) # select the last (largest) page number

  # If there are no additional pages, page_num should be 1
  if (length(page_num) == 0) page_num <- 1

  # Generate page urls for each of the page numbers
  page_urls <- paste0("https://cheese.com/alphabetical/?per_page=100&i=",
                      letter,
                      "&page=",
                      1:page_num)

  # Extract all cheese names for each of the page urls
  cheese_names <- map(.x = page_urls,
                      get_cheese_names_per_page) %>% unlist()
  cheese_names
}

# Function to extract data from the cheese_list and put it into a data frame
cheese_list_to_df <- function(cheese_list) {
  # Find all unique flavors (remove NA)
  unique_flavors <- map(.x = cheese_list, "flavors") %>%
    unlist() %>%
    unique()
  unique_flavors <- unique_flavors[!is.na(unique_flavors)]

  # Find all unique milks (remove NA)
  unique_milks <- map(.x = cheese_list, "milk_types") %>%
    unlist() %>%
    unique() %>%
    str_to_lower()
  unique_milks <- unique_milks[!is.na(unique_milks)]

  # Use 1 and 0 to indicate the whether a cheese has a particular flavor or
  # country of origin
  flavor_vec <- rep(0, length(unique_flavors))
  milk_type_vec <- rep(0, length(unique_milks))
  names(flavor_vec) <- unique_flavors
  names(milk_type_vec) <- unique_milks

  map_dfr(.x = cheese_list,
          .f = function(cheese) {
            cheese_name <- cheese$name
            if(length(cheese$fat_content) == 0) {
              fat_content <- NA
            } else {
              fat_content <- cheese$fat_content
            }

            flavors <- cheese$flavors
            flavor_vec[which(unique_flavors %in% flavors)] <- 1
            milk_types <- cheese$milk_types
            milk_type_vec[which(unique_milks %in% milk_types)] <- 1

            origin_countries <- cheese$origin_countries

            data.frame(
              cheese_name,
              origin_countries,
              t(flavor_vec),
              t(milk_type_vec),
              fat_content,
              stringsAsFactors = FALSE
            )
          }
  )
}

# Scrape all cheeses ------------------------------------------------------
cheese_names <- map(.x = letters,
                    .f = get_cheeses_per_letter) %>% unlist()
cheese_list <- map(.x = cheese_names,
                   .f = cheese_scraper)

# Need to change the fat content for Limburger because two fat content
# amounts are listed.
cheese_list[[961]]$fat_content <- 27

# Convert the list to a data frame
cheese_data <- cheese_list_to_df(cheese_list)
saveRDS(cheese_data, file = "data/cheese_data.RDS")


# Transform and plot cheese data ------------------------------------------
cheese_data <- dplyr::rename(cheese_data, country = origin_countries)
cheese_data <- filter(cheese_data, !is.na(country))
cheese_data$country_lumped <- forcats::fct_lump(cheese_data$country, n = 20)
cheese_flavors_by_country <- cheese_data %>%
  dplyr::group_by(country_lumped) %>%
  dplyr::summarise_at(.vars = vars(burnt:umami), .funs = mean)

ggradar::ggradar(cheese_flavors_by_country)