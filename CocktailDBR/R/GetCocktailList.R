#' Obtain a list of cocktails
#' @description Either obtain a complete list of cocktails from TheCocktailDB API, or get
#'  a list of cocktails starts with certain letter. The returned data.frame includes
#'  cocktail ID, name, its type and if it is alcoholic.
#' @param all parameter all is defaulted to FALSE. When set to true, the function will
#' pull a complete list of cocktails from the API
#' @param firstLetter parameter firstLetter allows user to set pull a list of cocktails
#' start with the letter user enters. The parameter is defaulted to letter "a"
#' @examples
#' GetCocktailList(all = FALSE, firstLetter = "b")
#' GetCocktailList(all = FALSE, firstLetter = "x")
#' GetCocktailList(all = TRUE)
#' @import httr
#' @import stringr
#' @import jsonlite
#' @import dplyr
#' @export


GetCocktailList <- function(all = FALSE, firstLetter = "a"){

  requireNamespace("httr")
  requireNamespace("stringr")
  requireNamespace("jsonlite")
  requireNamespace("dplyr")

  url <- "https://www.thecocktaildb.com/api/json/v1/1/search.php?f="
  if(all == TRUE){

    datalist = c()
    for(i in 1:26){

      query_url <- paste(url, letters[i]) %>% str_remove_all(" ")
      query_response <- GET(query_url)

      if(query_response$status_code == 200){
        query_data <- content(query_response, as = "parsed")
        result <- query_data %>% toJSON() %>% fromJSON()

        result_size <- length(result$drinks)
        if(result_size == 0){
          warning_message <- paste("There's no drink starts with letter", letters[i])
          print(warning_message)
        }
        else{
          id <- result$drinks[, 1] %>% unlist
          name <- result$drinks[, 2] %>% unlist
          drink_type <- result$drinks[, 11] %>% unlist
          alcoholic <- result$drinks[, 13] %>% unlist
          datalist[[i]] <- cbind.data.frame(id, name, drink_type, alcoholic)
        }
      }
      else{
        warning("Warning: query response failure")
      }
    }

    full_list <- do.call(rbind, datalist)
    full_list

  } else {

    query_url <- paste(url, firstLetter) %>% str_remove_all(" ")
    query_response <- GET(query_url)

    if(query_response$status_code == 200) {
      query_data <- content(query_response, as = "parsed")
      result <- query_data %>% toJSON() %>% fromJSON()

      result_size <- length(result$drinks)

      if(result_size == 0)
      {
        warning("No drinks found with the letter entered")
      }
      else{
        id <- result$drinks[, 1] %>% unlist
        name <- result$drinks[, 2] %>% unlist
        drink_type <- result$drinks[, 11] %>% unlist
        alcoholic <- result$drinks[, 13] %>% unlist
        drinklist <- cbind.data.frame(id, name, drink_type, alcoholic)

        drinklist
      }
    }
    else{
      warning("Warning: query response failure")
    }
  }
}
