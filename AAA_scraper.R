library(rvest)
library(httr)
library(dplyr)
library(readr)
library(purrr)

# ── All state/DC abbreviations ────────────────────────────────────────────────
state_abbrs <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI",
  "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
  "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
  "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
  "WV", "WI", "WY"
)

# ── Scrape all metro prices for one state ─────────────────────────────────────
scrape_state_metros <- function(state_code) {
  url <- paste0("https://gasprices.aaa.com/?state=", state_code)
  Sys.sleep(1)

  resp <- tryCatch(
    GET(url, user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36")),
    error = function(e) { message("Request failed: ", state_code); NULL }
  )
  if (is.null(resp) || http_error(resp)) {
    message("HTTP error for state: ", state_code)
    return(tibble())
  }

  page      <- read_html(content(resp, as = "text", encoding = "UTF-8"))
  accordion <- page %>% html_node(".accordion-prices.metros-js")
  if (is.null(accordion)) return(tibble())

  h3_nodes <- accordion %>% html_nodes("h3[data-title]")

  map_dfr(h3_nodes, function(h3) {
    metro_name <- h3 %>% html_text(trim = TRUE)

    rows <- h3 %>%
      html_nodes(xpath = "following-sibling::div[1]//table[contains(@class,'table-mob')]//tbody/tr")

    if (length(rows) < 5) return(NULL)

    extract_row <- function(row) {
      tds <- row %>% html_nodes("td") %>% html_text(trim = TRUE)
      if (length(tds) < 5) return(rep(NA_real_, 4))
      round(parse_number(tds[2:5]), 2)
    }

    current   <- extract_row(rows[[1]])
    yesterday <- extract_row(rows[[2]])
    week_ago  <- extract_row(rows[[3]])
    month_ago <- extract_row(rows[[4]])
    year_ago  <- extract_row(rows[[5]])

    tibble(
      metro                = metro_name,
      regular_current      = current[1],
      mid_grade_current    = current[2],
      premium_current      = current[3],
      diesel_current       = current[4],
      regular_yesterday    = yesterday[1],
      mid_grade_yesterday  = yesterday[2],
      premium_yesterday    = yesterday[3],
      diesel_yesterday     = yesterday[4],
      regular_week_ago     = week_ago[1],
      mid_grade_week_ago   = week_ago[2],
      premium_week_ago     = week_ago[3],
      diesel_week_ago      = week_ago[4],
      regular_month_ago    = month_ago[1],
      mid_grade_month_ago  = month_ago[2],
      premium_month_ago    = month_ago[3],
      diesel_month_ago     = month_ago[4],
      regular_year_ago     = year_ago[1],
      mid_grade_year_ago   = year_ago[2],
      premium_year_ago     = year_ago[3],
      diesel_year_ago      = year_ago[4]
    )
  })
}

# ── Scrape all 51 state pages ─────────────────────────────────────────────────
message("Scraping ", length(state_abbrs), " state pages...")

results <- map_dfr(state_abbrs, function(abbr) {
  message("  ", abbr)
  df <- scrape_state_metros(abbr)
  if (nrow(df) > 0) df$state <- abbr
  df
}) %>%
  mutate(date_scraped = Sys.Date(), .before = 1) %>%
  select(date_scraped, state, metro,
         regular_current,   mid_grade_current,   premium_current,   diesel_current,
         regular_yesterday, mid_grade_yesterday, premium_yesterday, diesel_yesterday,
         regular_week_ago,  mid_grade_week_ago,  premium_week_ago,  diesel_week_ago,
         regular_month_ago, mid_grade_month_ago, premium_month_ago, diesel_month_ago,
         regular_year_ago,  mid_grade_year_ago,  premium_year_ago,  diesel_year_ago)

# ── Append to CSV (write header only on first run) ────────────────────────────
csv_path    <- "aaa_gas_prices.csv"
first_write <- !file.exists(csv_path)

write_csv(results, csv_path, append = !first_write, col_names = first_write)
message("\nAppended ", nrow(results), " rows to ", csv_path, " (date: ", Sys.Date(), ")")
print(results, n = Inf)
