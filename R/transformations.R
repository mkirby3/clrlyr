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
  test_ls <- list(campaign = data.frame(
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

  # coerce the vectors in each df in test_ls to character class
  for (i in c("campaign", "src", "medium")) {
    test_ls[[i]] <- map_df(test_ls[[i]], as.character)
  }

  # initialize the channel_group vector

  # apply grepl(regex, [["campaign"]], ignore.case = T) <- channel to each row
  # of each data frame in test_ls

  # create character vector where all values = 'Other'
  channel_group <- rep("Other Paid", times = length(campaign))

  # convert medium=referral values to Referral
  channel_group[grepl("^referral$", medium, ignore.case=T)] <- "Referral"

  # convert qp values to Display
  channel_group[grepl("^(vco(|fr)|display)$",
                   campaign, ignore.case = T)] <- "Display"
  channel_group[grepl("_rt_dy$|_dr(_|$)|mobile|criteo",
                   src, ignore.case = T)] <- "Display"
  channel_group[grepl("^(lowerfunnel|midfunnel|fb_rhs)$",
                   medium, ignore.case = T)] <- "Display"

  # convert qp values to Paid Search
  channel_group[grepl("^(search|brand(|_rlsa))$",
                   campaign, ignore.case = T)] <- "Paid Search"
  channel_group[grepl("^(g[cgb]s|b[cgb]s|y[cgb]s|mobile)$",
                   src, ignore.case = T)] <- "Paid Search"
  channel_group[grepl("^cpc$",
                   medium, ignore.case = T)] <- "Paid Search"

  # convert qp values to Email
  channel_group[grepl("^email$",
                   campaign, ignore.case = T)] <- "Email"
  channel_group[grepl("^(sale|transactional|refill|lifecycle|promo|abandonedcart)$",
                   src, ignore.case = T)] <- "Email"
  channel_group[grepl("email|responsys",
                   medium, ignore.case = T)] <- "Email"

  # convert qp values to Affiliate
  channel_group[grepl("^affiliate(|s)$",
                   campaign, ignore.case = T)] <- "Affiliate"
  channel_group[grepl("_otb_|^ebates",
                   src, ignore.case = T)] <- "Affiliate"
  channel_group[grepl("^(ls|cj)$|outbrain|retailmenot|bargainmoose|savingstory|redflagdeals",
                   src, ignore.case = T)] <- "Affiliate"

  # convert qp values to Paid Social
  channel_group[grepl("^social$", campaign, ignore.case = T)] <- "Paid Social"
  channel_group[grepl(
    "^(fb|ig|tw|pn|youtube|gp|facebook|reddit|rd|tumblr)$|_yt_|facebook|-fb_",
    src, ignore.case = T)] <- "Paid Social"
  channel_group[grepl("smcp|_yt_", medium, ignore.case = T)] <- "Paid Social"

  # convert qp values to Organic Social
  channel_group[grepl("^(fb|ig|tw|pn|youtube|gp|facebook|reddit|rd|tumblr)$|_yt_|facebook",
                   src, ignore.case = T) &
               grepl("^referral$", medium, ignore.case = T)] <- "Organic Social"
  channel_group[grepl("smco", medium, ignore.case = T)] <- "Organic Social"

  # convert qp values to CSE
  channel_group[grepl("^cse$", campaign, ignore.case = T)] <- "CSE"
  channel_group[grepl("^cse(|gl|cl)$", medium, ignore.case = T)] <- "CSE"

  # convert qp values to Referral
  channel_group[grepl("^pr$", campaign, ignore.case = T)] <- "Referral"

  # convert qp values to Direct
  channel_group[grepl("\\(direct\\)", src, ignore.case = T)] <- "Direct"

  # convert qp values to Organic Search
  channel_group[grepl("^organic$", medium, ignore.case = T)] <- "Organic Search"

  # return channel_group vector
  channel_group

} # end convert_qp_to_channel() fxn

# 1) change convert_qp_to_channel() fxn so that it can be used in dplyr pipes
# 2) remove duplication in function - e.g. channel_group[]; grepl();
#     ignore.case = TRUE; campaign; src; medium
# 3) check accuracy of channel conversions
# 4) verify that order of operations is ideal
