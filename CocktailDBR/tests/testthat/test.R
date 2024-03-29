# Test for Package CocktailDBR

test_that(
  "test if warning message functions properly",
  {
    expect_warning(GetCocktailList(all = FALSE ,firstLetter = "u"),
                   "No drinks found with the letter entered")
    expect_warning(CocktailSummary("123"),
                   "Please select a valid summary option")
    expect_warning(GetInstruction(language = "TEST"),
                   "Language entered is not available, search result defaulted to English")
  }
)

test_that(
  "test if function itself works with default argument", {
    expect_equal(GetCocktailList(), GetCocktailList(all = FALSE, firstLetter = "a"))
    expect_equal(GetCocktailInfo(), GetCocktailInfo(cocktailID = 11007, precise = TRUE))
    expect_equal(CocktailSummary(), CocktailSummary(summaryOption = "FirstLetter"))
    expect_equal(GetInstruction(), GetInstruction(id = "15266", language = "EN", precise = TRUE))
  }
)

