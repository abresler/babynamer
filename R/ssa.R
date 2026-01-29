# Download helper with browser-like headers to bypass Akamai blocking
.download_ssa_file <- function(url, destfile) {
  resp <- request(url) |>
    req_headers(
      "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.9",
      "Accept-Encoding" = "gzip, deflate, br",
      "Connection" = "keep-alive",
      "Upgrade-Insecure-Requests" = "1",
      "Sec-Fetch-Dest" = "document",
      "Sec-Fetch-Mode" = "navigate",
      "Sec-Fetch-Site" = "none",
      "Sec-Fetch-User" = "?1",
      "Cache-Control" = "max-age=0"
    ) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop(glue("Download failed with status {resp_status(resp)}"))
  }

  writeBin(resp_body_raw(resp), destfile)
  invisible(destfile)
}

.national <-
  memoise(function(url = "https://www.ssa.gov/oact/babynames/names.zip",
                            include_features = T) {
    outfile <- tempfile("download", fileext = ".zip")

    file <- .download_ssa_file(url, outfile)
    unz_files <- unzip(file, exdir = "xml")
    unz_files <- unz_files %>% str_to_lower()
    glue("US National SS names has {length(unz_files)} files") %>% message()

    unz_files <- unz_files[unz_files %>% str_detect(".txt")]

    data <-
      unz_files %>%
      map_dfr(function(x) {
        year <- x %>% str_remove_all("xml/|.txt|yob") %>% as.numeric()
        glue("Parsing {year}") %>% message()
        decade <- year %/% 10 %>% str_c("0")
        data <-
          x %>% read_csv(
            col_names = F,
            col_types = list(col_character(), col_character(), col_double())
          ) %>%
          setNames(c("name", "sex", "count"))
        data <-
          data %>%
          mutate(year = year,
                 decade = decade) %>%
          select(year, decade, everything())
        data
      })

    file %>% unlink()
    unlink("xml", recursive = T)
    unz_files %>% unlink()
    gc()
    rm(unz_files)

    data <-
      data %>%
      mutate(date = glue("{year}-12-31") %>% ymd())

    data <-
      data %>%
      group_by(year, sex) %>%
      mutate(rank = dense_rank(desc(count))) %>%
      ungroup()

    data <- data %>%
      select(decade, year, date, sex, rank, name, count)

    df_years <-
      data %>%
      group_by(year, sex) %>%
      summarise(total = sum(count)) %>%
      ungroup()

    df_decade <- data %>%
      group_by(decade, sex) %>%
      summarise(total_decade = sum(count)) %>%
      ungroup()

    data <-
      data %>%
      left_join(df_years, by = c("sex", "year")) %>%
      left_join(df_decade, by = c("sex", "decade")) %>%
      mutate(pct_year_sex = count / total,
             pct_decade = count / total_decade) %>%
      select(-c(total, total_decade))



    if (include_features) {
      df_features <- data %>%
        group_by(name, sex) %>%
        summarise(
          count_distinct_years = n_distinct(year),
          year_recent = max(year),
          year_first = min(year),
          year_most_popular_nominal = year[which.max(count)],
          year_most_popular_real =  year[which.max(pct_year_sex)],
          top_rank = min(rank),
          year_least_popular_nominal = year[which.min(count)],
          year_least_popular_real =  year[which.min(pct_year_sex)],
          worst_rank = max(rank)
        ) %>%
        ungroup() %>%
        mutate(count_characters = nchar(name))

      data <-
        data %>%
        left_join(df_features, by = c("name", "sex"))
    }

    data
  })

.state <-
  memoise(function(url = "https://www.ssa.gov/oact/babynames/state/namesbystate.zip",
                            include_features = T) {
    outfile <- tempfile("download", fileext = ".zip")

    file <- .download_ssa_file(url, outfile)
    unz_files <- unzip(file, exdir = "xml")
    glue("State SS names has {length(unz_files)} files") %>% message()
    unz_files <- unz_files %>% str_to_lower()
    unz_files <- unz_files[unz_files %>% str_detect(".txt")]

    data <-
      unz_files %>%
      map_dfr(function(x) {
        state_slug <-
          x %>% str_remove_all("xml/|.txt") %>% str_to_upper()
        glue("Parsing {state_slug}") %>% message()
        data <-
          x %>% read_csv(
            col_names = F,
            col_types = list(
              col_character(),
              col_character(),
              col_double(),
              col_character(),
              col_double()
            )
          ) %>%
          setNames(c("state", "sex", "year", "name", "count"))

        data
      })

    file %>% unlink()
    unlink("xml", recursive = T)
    unz_files %>% unlink()
    gc()
    rm(unz_files)

    data <-
      data %>%
      mutate(decade = year %/% 10 %>% str_c("0"),
             date = glue("{year}-12-31") %>% ymd())

    data <-
      data %>%
      group_by(year, state, sex) %>%
      mutate(rank = dense_rank(desc(count))) %>%
      ungroup()

    data <- data %>%
      select(state, decade, year, date, sex, rank, name, count)

    df_years <-
      data %>%
      group_by(year, state, sex) %>%
      summarise(total_state = sum(count)) %>%
      ungroup()

    df_decade <-
      data %>%
      group_by(decade, state, sex) %>%
      summarise(total_decade_state = sum(count)) %>%
      ungroup()

    data <-
      data %>%
      left_join(df_years, by = c("year", "state", "sex"))

    data <- data %>%
      left_join(df_decade, by = c("decade", "state", "sex"))

    data <-
      data %>%
      mutate(
        pct_year_state_sex = count / total_state,
        pct_decade_state_sex = count / total_decade_state
      ) %>%
      select(-c(total_state, total_decade_state))


    if (include_features) {
      df_features <-
        data %>%
        group_by(name, state, sex) %>%
        summarise(
          count_distinct_years = n_distinct(year),
          year_recent = max(year),
          year_first = min(year),
          year_most_popular_nominal = year[which.max(count)],
          year_most_popular_real =  year[which.max(pct_year_state_sex)],
          top_rank = min(rank),
          year_least_popular_nominal = year[which.min(count)],
          year_least_popular_real =  year[which.min(pct_year_state_sex)],
          worst_rank = max(rank)
        ) %>%
        ungroup() %>%
        mutate(count_characters = nchar(name))

      data <-
        data %>%
        left_join(df_features, by = c("name", "sex", "state"))
    }


    data
  })

.territory <-
  memoise(function(url = "https://www.ssa.gov/oact/babynames/territory/namesbyterritory.zip",
                            include_features = T) {
    outfile <- tempfile("download", fileext = ".zip")

    file <- .download_ssa_file(url, outfile)
    unz_files <- unzip(file, exdir = "xml")
    glue("Territory SS names {length(unz_files)} files") %>% message()
    unz_files <- unz_files %>% str_to_lower()
    unz_files <- unz_files[unz_files %>% str_detect(".txt")]

    data <-
      unz_files %>%
      map_dfr(function(x) {
        territory_slug <-
          x %>% str_remove_all("xml/|.txt") %>% str_to_upper()
        glue("Parsing {territory_slug}") %>% message()
        data <-
          x %>% read_csv(
            col_names = F,
            col_types = list(
              col_character(),
              col_character(),
              col_double(),
              col_character(),
              col_double()
            )
          ) %>%
          setNames(c("territory", "sex", "year", "name", "count"))

        data
      })

    file %>% unlink()
    unlink("xml", recursive = T)
    unz_files %>% unlink()
    gc()
    rm(unz_files)

    data <-
      data %>%
      mutate(decade = year %/% 10 %>% str_c("0"),
             date = glue("{year}-12-31") %>% ymd())

    data <-
      data %>%
      group_by(year, territory, sex) %>%
      mutate(rank = dense_rank(desc(count))) %>%
      ungroup()

    data <- data %>%
      select(territory, decade, year, date, sex, rank, name, count)

    df_years <-
      data %>%
      group_by(year, territory, sex) %>%
      summarise(total_territory = sum(count)) %>%
      ungroup()

    df_decade <-
      data %>%
      group_by(decade, territory, sex) %>%
      summarise(total_decade_territory = sum(count)) %>%
      ungroup()

    data <-
      data %>%
      left_join(df_years, by = c("year", "territory", "sex"))

    data <- data %>%
      left_join(df_decade, by = c("decade", "territory", "sex"))

    data <-
      data %>%
      mutate(
        pct_year_territory_sex = count / total_territory,
        pct_decade_territory_sex = count / total_decade_territory
      ) %>%
      select(-c(total_territory, total_decade_territory))


    if (include_features) {
      df_features <-
        data %>%
        group_by(name, territory, sex) %>%
        summarise(
          count_distinct_years = n_distinct(year),
          year_recent = max(year),
          year_first = min(year),
          year_most_popular_nominal = year[which.max(count)],
          year_most_popular_real =  year[which.max(pct_year_territory_sex)],
          top_rank = min(rank),
          year_least_popular_nominal = year[which.min(count)],
          year_least_popular_real =  year[which.min(pct_year_territory_sex)],
          worst_rank = max(rank)
        ) %>%
        ungroup() %>%
        mutate(count_characters = nchar(name))

      data <-
        data %>%
        left_join(df_features, by = c("name", "sex", "territory"))
    }


    data
  })



.babynames <-
  function(type = "national",
           include_features = T,
           nest_data = F) {
    type_slug <- str_to_lower(type)

    if (!type_slug %in% c("national", "state", "territory")) {
      stop("Type can only be national, state or territory")
    }

    # Hardcoded URLs - SSA URLs are stable and scraping the limits page is blocked
    url <- switch(
      type_slug,
      "national" = "https://www.ssa.gov/oact/babynames/names.zip",
      "state" = "https://www.ssa.gov/oact/babynames/state/namesbystate.zip",
      "territory" = "https://www.ssa.gov/oact/babynames/territory/namesbyterritory.zip"
    )


    if (type_slug == "national") {
      data <- .national(url = url, include_features = include_features)
    }

    if (type_slug == "territory") {
      data <- .territory(url = url, include_features = include_features)
    }

    if (type_slug == "state") {
      data <- .state(url = url, include_features = include_features)
    }

    data <-
      data %>%
      mutate(type = type_slug) %>%
      select(type, everything())


    if (nest_data) {
      data <-
        data %>% group_by(type) %>%
        nest() %>%
        ungroup()
    }

    data

  }


#' United States Baby Name data
#'
#' Returns detailed data about the popularity of
#' various baby names in the United States.
#'
#' Includes access to national, state, and territorial
#' data.
#'
#' @param type the type of data options include \itemize{
#' \item `national` - National popularity by state, sex and year - default
#' \item `state` - Popularity by state, sex and year
#' \item `territory` -  Popularity by territory, sex and year
#' }
#' @param include_features if `TRUE` includes features about the popularity of the name by sex
#' @return a `tibble` or a nested `tibble` if `type` length exceeds 1
#' @export
#'
#' @examples
#' \dontrun{
#' library(babynamer)
#'
#' tbl_usa <- us_baby_names(type = "national")
#' tbl_state <- us_baby_names(type = "state")
#' tbl_territory <- us_baby_names(type = "territory")
#' us_baby_names <- us_baby_names(type = c("national","state", "territory"))
#'
#'}
us_baby_names <-
  function(type = "national",
           include_features = T) {
    if (length(type) == 0) {
      stop("No type, file type can be national, state or territory")
    }

    if (length(type) > 1) {
      "More than 1 file type nesting data - you may also want to think about assigning to environment." %>% message()
      nest_data <- T
    } else {
      nest_data <- F
    }

    .babynames_safe <- possibly(.babynames, tibble())
    data <-
      type %>%
      map_dfr(function(x) {
        .babynames_safe(type = x,
                        include_features = include_features,
                        nest_data = nest_data)
      })

    data
  }
