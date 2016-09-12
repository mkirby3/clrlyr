# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

head2 <- function(dat, n = 6L) {
  head(as.data.frame(dat), n)
}

tail2 <- function(dat, n = 6L) {
  tail(as.data.frame(dat), n)
}

# define prettify_cols() function - clean up column names
prettify_cols <- function(x, locale = "") {
  x <- gsub("_", " ", x)
  x <- tolower(x)
  x <- stringi::stri_trans_totitle(
    x, opts_brkiter = stringi::stri_opts_brkiter(locale = locale))
  x
} # end prettify_cols() fxn

# define as_factor_to_chr() fxn - converts all factor columns in a df to chr
as_character_df <- function(dat) {

  is_factor_cols <- purrr::map_lgl(dat, is.factor)

  if (any(is_factor_cols, na.rm = TRUE)) {

    dat[, is_factor_cols] <- purrr::map_df(dat[, is_factor_cols, drop = FALSE],
                                           as.character)

    dat

  } else {
    dat
  }

} # end as_character_cols() fxn


# test git change


