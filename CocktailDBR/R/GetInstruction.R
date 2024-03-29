#' Obtain instruction for certain cocktails
#' @description This function allows user to obtain instruction for certain cocktails
#' based on the local data.frame contains the latest list of cocktails from pulled from
#' the API. User can do search for multiple cocktails at a time, and the instruction returned
#' may be in other languages as well
#' @param id asks user to enter cocktail IDs to search, the cocktail IDs are numberical IDs
#' obtained from the API and is also available in the package's included data.frame. Do a
#' c() command to search for multiple cocktails at a time
#' @param language This parameter allows user to choose the instruction language, allowed entries
#' include English (EN), Spanish (ES), German (DE), French (FR) and Chinese (ZH). The parameter is
#' defaulted to English (EN)
#' @param precise parameter precise ask if the user want the result in precise format, if
#' set to be TRUE, the return information will only include drink id, name, category, type
#' glass used and instruction
#' @examples
#' GetInstruction(language = "DE")
#' GetInstruction(id = "15266", language = "EN")
#' GetInstruction(id = c("17840", "15266"))
#' @import dplyr
#' @export


GetInstruction <- function(id = "15266", language = "EN", precise = TRUE) {

  requireNamespace("dplyr")

  data("allCocktails", package = "CocktailDBR")

  result <- c()
  result <- allCocktails %>% filter(idDrink %in% id) %>% select(-3, -10, -52)

  if(language == "EN"){
    result <- result %>% select(-3:-7, -14:-18)
  }
  else if(language == "ES"){
    result <- result %>% select(-2, -4:-7, -13, -15:-18)
  }
  else if(language == "DE"){
    result <- result %>% select(-2:-3, -5:-7, -13:-14, -16:-18)
  }
  else if(language == "FR"){
    result <- result %>% select(-2:-4, -6:-7, -13:-15, -17:-18)
  }
  else if(language == "ZH"){
    result <- result %>% select(-2:-5, -13:-16)
  }
  else{
    warning("Language entered is not available, search result defaulted to English")
    result <- result %>% select(-3:-7, -14:-18)
  }

  if(precise == TRUE) {
    result <- result %>% select(1:2, 4, 6:8)
    result
    }
  else{
    result
  }
}
