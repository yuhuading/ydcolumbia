#' Obtain cocktail summaries
#' @description This function allows user to obtain summaries of cocktails based on the
#' local data.frame contains the latest list of cocktails from pulled from the API
#' @param summaryOption parameter allows user to choose from four different summary types. The
#' valid summary types are following: FirstLetter, IsAlcoholic, Category and Ingredients.
#' The parameter is defaulted to FirstLetter.
#' FirstLetter: provides summary over number of cocktails starts with 26 letters
#' IsAlcoholic: provides summary over number of cocktails by alcoholic type
#' Category: provides summary over number of cocktails by category
#' Ingredients: collects all ingredients used in all cocktails and returns 20 most
#' common ingredients
#' @examples
#' CocktailSummary("Ingredients")
#' CocktailSummary("Category")
#' @import tidyr
#' @import dplyr
#' @export



CocktailSummary <- function(summaryOption = "FirstLetter"){

  requireNamespace("tidyr")
  requireNamespace("dplyr")

  data("allCocktails", package = "CocktailDBR")

  if(summaryOption == "FirstLetter"){
    table(str_extract(allCocktails$Drink, "."))
  }
  else if(summaryOption == "IsAlcoholic"){
    table(allCocktails$Alcoholic)
  }
  else if(summaryOption == "Category"){
    table(allCocktails$Category)
  }
  else if(summaryOption == "Ingredients"){
    allCocktails %>% select(22:36) %>% gather("ingredients") %>%
      select(-ingredients) %>% filter(value != "NA") %>% group_by(value) %>%
      summarise(count = n()) %>% arrange(desc(count)) %>% head(20)
  }
  else{
    warning("Please select a valid summary option")
  }
}
