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

# define hello() fxn - test function for {clrlyr} package
hello <- function() {
  print("Hello, woRld!")
}

# define head2() fxn - print top n rows from a df, including ALL columns
head2 <- function(dat, n = 6L) {
  head(as.data.frame(dat), n)
}

# define tail2() fxn - print bottom n rows from a df, including ALL columns
tail2 <- function(dat, n = 6L) {
  tail(as.data.frame(dat), n)
}

# define prettify_cols() function - prettify column names for use in graphics
prettify_cols <- function(x, locale = "") {

  if (typeof(x) != "character") {
    stop("x must be a character vector", call. = FALSE)
  }

  x <- gsub("_", " ", x)
  x <- tolower(x)
  x <- stringi::stri_trans_totitle(
    x, opts_brkiter = stringi::stri_opts_brkiter(locale = locale))
  x
} # end prettify_cols() fxn

# define coerce_df_cols_to_chr() fxn - converts all factor columns in df to chr
coerce_df_cols_to_chr <- function(dat) {

  if (typeof(dat) != "list") {
    stop("dat must be a data frame or list", call. = FALSE)
  }

  is_factor_cols <- purrr::map_lgl(dat, is.factor)

  if (any(is_factor_cols, na.rm = TRUE)) {

    dat[, is_factor_cols] <- purrr::map_df(dat[, is_factor_cols, drop = FALSE],
                                           as.character)

    dat

  } else {
    dat
  }

} # end coerce_df_cols_to_chr() fxn

# define as_date() fxn - converts character to date (w/ PDT/PST default tz)
as_date <- function(x) {

  x
} # end as_date() fxn


