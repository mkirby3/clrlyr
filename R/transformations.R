# Convenience functions for doing data science in R at Clearly
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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
as_date <- function(x, time_zone = "America/Los_Angeles") {
  x <- as.Date(x, tz = time_zone)
  x
} # end as_date() fxn

# define convert_qp_to_channel() fxn - converts query string parameters
# from a site visitor's landing page URL to defined channel groups
convert_qp_to_channel <- function(campaign, src, medium) {

  # stop function call if input vectors are not character class or have length 0
  if (length(campaign) < 1 | length(src) < 1 | length(medium) < 1) {
    stop("each input vector must have length greater than 0", call. = FALSE)
  }

  if (is.factor(campaign)) {
    campaign <- as.character(campaign)
  }

  if (is.factor(src)) {
    src <- as.character(src)
  }

  if (is.factor(medium)) {
    medium <- as.character(medium)
  }

  if (typeof(campaign) != "character" | typeof(src) != "character" |
      typeof(medium) != "character") {
    stop("each input vector must be a character vector", call. = FALSE)
  }

  # define regex for each channel group based on campaign, src, medium qps
  channel_ls <- list(campaign = data.frame(
    regex = c("^(vco(|fr)|display)$",
              "^(search|brand(|_rlsa))$",
              "^email$",
              "^affiliate(|s)$",
              "^social$",
              "^cse$",
              "^pr$"),
    channel = c("Display",
                "Paid Search",
                "Email",
                "Affiliate",
                "Paid Social",
                "CSE",
                "Referral")),
    src = data.frame(
      regex = c("_rt_dy$|_dr(_|$)|mobile|criteo",
                "^(g[cgb]s|b[cgb]s|y[cgb]s|mobile)$",
                "^(sale|transactional|refill|lifecycle|promo|abandonedcart)$",
                "_otb_|^ebates",
                "^(ls|cj)$|outbrain|retailmenot|bargainmoose|savingstory|redflagdeals",
                "^(fb|ig|tw|pn|youtube|gp|facebook|reddit|rd|tumblr)$|_yt_|facebook|-fb_",
                "\\(direct\\)"),
      channel = c("Display",
                  "Paid Search",
                  "Email",
                  "Affiliate",
                  "Affiliate",
                  "Organic Social",
                  "Direct")),
    medium = data.frame(
      regex = c("^referral$",
                "^(lowerfunnel|midfunnel|fb_rhs)$",
                "^cpc$",
                "email|responsys",
                "smcp|_yt_",
                "smco",
                "^cse(|gl|cl)$",
                "^organic$"),
      channel = c("Referral",
                  "Display",
                  "Paid Search",
                  "Email",
                  "Paid Social",
                  "Organic Social",
                  "CSE",
                  "Organic Search")))

  # coerce the vectors in each df in channel_ls to character class
  for (i in c("campaign", "src", "medium")) {
    channel_ls[[i]] <- map_df(channel_ls[[i]], as.character)
  }

  # initialize the channel_group vector - allocate space for the for loop
  channel_group <- character(length = length(campaign))

  # create list of campaign, source, medium for each raw vector
  qp_ls <- list(campaign = campaign, src = src, medium = medium)

  # convert each raw query param value to a defined channel group
  for (i in names(channel_ls)) {

    for (j in seq_along(channel_ls[[i]]$channel)) {

      channel_group[grepl(
        channel_ls[[i]]$regex, qp_ls[[i]],
        ignore.case = TRUE)] <- channel_ls[[i]]$channel

    } # end nrow(channel_ls) j loop
  } # end names(channel_ls) i loop

  # return channel_group vector
  channel_group

} # end convert_qp_to_channel() fxn

# 3) check accuracy of channel conversions
# 4) verify that order of operations is ideal

# define calc_total_rows() fxn - calculates the sum of each metric column
# grouped by each dimension column supplied to the fxn
calc_total_rows <- function(dat = NULL, total_columns = NULL) {

  # return dat
  dat

} # end calc_total_rows() fxn










