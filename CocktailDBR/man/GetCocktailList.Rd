% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetCocktailList.R
\name{GetCocktailList}
\alias{GetCocktailList}
\title{Obtain a list of cocktails}
\usage{
GetCocktailList(all = FALSE, firstLetter = "a")
}
\arguments{
\item{all}{parameter all is defaulted to FALSE. When set to true, the function will
pull a complete list of cocktails from the API}

\item{firstLetter}{parameter firstLetter allows user to set pull a list of cocktails
start with the letter user enters. The parameter is defaulted to letter "a"}
}
\description{
Either obtain a complete list of cocktails from TheCocktailDB API, or get
 a list of cocktails starts with certain letter. The returned data.frame includes
 cocktail ID, name, its type and if it is alcoholic.
}
\examples{
GetCocktailList(all = FALSE, firstLetter = "b")
GetCocktailList(all = FALSE, firstLetter = "x")
GetCocktailList(all = TRUE)
}
