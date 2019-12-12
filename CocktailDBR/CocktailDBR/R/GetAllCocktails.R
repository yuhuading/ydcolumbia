#' Obtain all available cocktail information from the API
#' @description This function allows user to pull a complete list of cocktails from
#' TheCocktailDB API with all information available on the API site. The user get to
#' choose if data returns precise information. Please be aware that running this function
#' may take long time due to amount of information it's pulling
#' @param precise parameter precise ask if the user want the result in precise format, if
#' set to be TRUE, the return information will only include drink id, name, category, type
#' glass used and date last modified
#' English instructions
#' @examples
#' GetAllCocktails()
#' GetAllCocktails(precise = TRUE)
#' @import httr
#' @import stringr
#' @import jsonlite
#' @import dplyr
#' @export


GetAllCocktails <- function(precise = FALSE){

  requireNamespace("httr")
  requireNamespace("stringr")
  requireNamespace("jsonlite")
  requireNamespace("dplyr")

  url <- "https://www.thecocktaildb.com/api/json/v1/1/search.php?f="
  datalist = c()
  for(i in 1:26){
    query_url <- paste(url, letters[i]) %>% str_remove_all(" ")
    query_response <- GET(query_url)
    if(query_response$status_code == 200){
      query_data <- content(query_response, as = "parsed")
      result <- query_data %>% toJSON() %>% fromJSON()

      result_size <- length(result$drinks)
      if(result_size == 0){
      }
      else{
        id <- result$drinks[, 1] %>% unlist
        name <- result$drinks[, 2] %>% unlist
        datalist[[i]] <- cbind.data.frame(id, name)
      }
    }
    else{
      warning("Warning: query response failure (stage 1)")
    }
  }
  full_list <- do.call(rbind, datalist)
  id_list <- full_list %>% select(1) %>% mutate(index = 1:n())
  ####
  full_frame <- data.frame()
  for (i in 1:nrow(id_list)){
    url <- "https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i="
    query_url <- paste(url, id_list[i,1]) %>% str_remove_all(" ")
    query_response <- GET(query_url)
    if(query_response$status_code == 200) {
      query_data <- content(query_response, as = "parsed")
      result <- fromJSON(toJSON(query_data))
      result_size <- length(result$drinks)

      if(result_size == 0){
        warning("Query failed, please confirm the cocktail id entered is correct")
      }
      else{
        datarow <- data.frame()
        for(j in 1:ncol(result$drinks)){
          size <- length(result$drinks[[j]])
          if(size == 0){
            full_frame[i, j] <- "NA"
          }
          else{
            full_frame[i, j] <- result$drinks[[j]] %>% unlist
          }
        }
      }
    }
    else{
      warning("Warning: query response failure (stage 2)")
    }
  }
  new_names <- colnames(result$drinks) %>% str_remove("str")
  colnames(full_frame) <- new_names

  if(precise == TRUE){
    precise_info <- full_frame %>% select(1:2, 11, 13:15, 53)
    precise_info
  }
  else{
    full_frame
  }
}
