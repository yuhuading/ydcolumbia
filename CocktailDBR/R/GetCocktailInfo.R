#' Search for cocktail information based on cocktail IDs
#' @description This function allows user to search for cocktail information based on
#' cocktail IDs. User is able to choose result format in either precise version or complete
#' version
#' @param cocktailID asks the user to key in the cocktail ID, the ID list can be obtained
#' by the GetCocktailList function. Default value of ID is set to "11007" (margrita)
#' @param precise parameter precise ask if the user want the result in precise format, if
#' set to be TRUE, the return information will only include drink id, name, tag, type and
#' English instructions
#' @examples
#' GetCocktailInfo(15266, precise = FALSE)
#' GetCocktailInfo(14167, precise = TRUE)
#' @import httr
#' @import stringr
#' @import jsonlite
#' @import dplyr
#' @export


GetCocktailInfo <- function(cocktailID = 11007, precise = FALSE){

  requireNamespace("httr")
  requireNamespace("stringr")
  requireNamespace("jsonlite")
  requireNamespace("dplyr")

  url <- "https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i="
  query_url <- paste(url, cocktailID) %>% str_remove_all(" ")

  query_response <- GET(query_url)

  if(query_response$status_code == 200) {
    query_data <- content(query_response, as = "parsed")
    result <- fromJSON(toJSON(query_data))

    result_size <- length(result$drinks)
    if(result_size == 0){
      warning("Query failed, please confirm the cocktail id entered is correct")
    }
    else{
      full_info <- data.frame()
      for(i in 1:ncol(result$drinks)){
        size <- length(result$drinks[[i]])
        if(size == 0){
          full_info[1, i] <- "NA"
        }
        else{
          full_info[1, i] <- result$drinks[[i]] %>% unlist
        }
      }
      new_names <- colnames(result$drinks) %>% str_remove("str")
      colnames(full_info) <- new_names

      if(precise == TRUE){
        precise_info <- full_info %>% select(id = idDrink, name = Drink, tags = Tags,
                                             alcoholic = Alcoholic, instructions = Instructions)
        precise_info
      }
      else{
        full_info
      }
    }
  }
  else{
    warning("Warning: query response failure")
  }
}
