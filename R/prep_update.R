
update_get_html <-
  function(date = lubridate::today(),
           root_url = .update_root_url,
           url_path,
           remDr) {

    out <- list()

    url <- update_url(root_url = root_url, url_path = url_path)
    if (missing(remDr)) {
      out <- list(url = url,
                  htm = xml2::read_html(url))
    } else {
      d <- remDr$client
      # Login ----
      # Fri Apr 14 10:22:10 2023
      d$navigate(url)
      title <- d$getTitle()[[1]]

      pg <- purrr::map_lgl(c(page = "^Page", update = "Update", login = "Login"), \(.x) {
        stringr::str_detect(title, .x)
      })
      pg <- names(which(pg))
      # If the permalink was not set as expected to the YYYY-MM format and page not found
      if (identical(pg, "page")) {
        # Fetch the latest post and use the URL
        post <-
          wordpressr::get_wp_posts(
            root_url,
            post_count = 1,
            after_date = lubridate::floor_date(lubridate::as_date(date), unit = "month")
          )
        url <- post$url
        d$navigate(url)
      } else if (identical(pg, "login")) {
        el <- webElem(d, "#username-1037")
        if (!isElementValid(el))
          UU::gbort("Does {url} exist on the site?")
        el$sendKeysToElement(list(Sys.getenv("wp_login")))
        el <- webElem(d, "#user_password-1037")
        el$sendKeysToElement(list(Sys.getenv("wp_password")))
        el <- webElem(d, "#um-submit-btn")
        el$clickElement()
        .url <- stringr::str_remove(d$getCurrentUrl()[[1]], "/$")
        while (.url != url && isElementValid(el)) {
          Sys.sleep(1)
          el$clickElement()
        }
      }

      .htm <- d$getPageSource()[[1]]
      out$driver <- remDr
      out$htm = .htm
      out$url <- url
    }



    return(out)
  }
.update_root_url = "https://www.patapa.dhamma.org/"
update_url <- function(url_path, root_url = .update_root_url) {
  .url <- httr::parse_url(root_url)
  .url$path <- url_path
  httr::build_url(.url)
}

update_get_sections <- function(doc) {
  if (!is.null(doc$driver) && inherits(doc$driver, "rsClientServer")) {
    doc$driver$client$refresh()
    doc$htm <- doc$driver$client$getPageSource()[[1]]
  }
  htm <- if (!inherits(doc$htm, "xml_document"))
    xml2::read_html(doc$htm)
  else
    doc$htm

  list(url = doc$url,
  goenka_quote = rvest::html_node(htm, "#goenka-quote") |> rvest::html_text() |> stringr::str_replace("^[^a-zA-Z]+", "") |> stringr::str_replace("[^a-zA-Z]+$", ""),
              sections = rvest::html_elements(htm, css = "section.update"),
              IVN = rvest::html_node(htm, "#update-ivn"),
              buddha_quote = rvest::html_node(htm, "#buddha-quote"))
}
isElementValid <- function(el) {
  UU::`%|try|%`(el$isElementDisplayed()[[1]],  FALSE)
}

webElem <- function(client, selector) {
  webElem <- NULL
  counter = 1
  while(is.null(webElem) && counter < 10) {
    webElem <-
      tryCatch({
        client$findElement(using = 'css', value = selector)
      },
      error = function(e) {
        counter <<- counter + 1
        NULL
      })
    #loop until element with name <value> is found in <webpage url>
  }
  webElem
}

selenium_start <- function() {
  docker_running <- utils::askYesNo("Did you start Docker?")
  if (!docker_running) {
    UU::gbort("Please start docker")
  }
  RSelenium::rsDriver(port = 4444L, browser = "firefox")
}

selenium_end <- function(remDr) {
  remDr$client$closeall()
  remDr$client$closeServer()
}

update_prep <- function(sections, date = lubridate::today(), template_file = dirs$extdata("update_template.html")) {
  y <- lubridate::year(date)
  m <- lubridate::month(date)
  d <- lubridate::day(date)

  update <- xml2::read_html(template_file)
  # Full Update button ----
  # Thu Dec 29 13:46:25 2022
  update_button <- update |>
    rvest::html_element(".login-button")
  xml2::xml_attr(update_button, "href") <- sections$url
  # MC Text  ----
  # Thu Dec 29 13:47:19 2022
  preview_text <- glue::glue("{month.name[m]} {y} Update")
  purrr::walk(c("title", "span.mcnPreviewText"), ~update |>
                rvest::html_element(.x) |>
                xml2::`xml_text<-`(preview_text))
  # Goenka Quote ----
  # Thu Dec 29 13:57:33 2022
  bq <- update |>
    rvest::html_element("#goenka-quote") |>
    xml2::`xml_text<-`(sections$goenka_quote)

  e <- environment()
  # Sections ----
  # Thu Dec 29 16:50:44 2022
  body <- update |>
    rvest::html_element("#templateBody")
  purrr::iwalk(sections$sections, \(.x, .y) {
    # Add header
    if (section_has$header(.x))
      xml2::xml_add_child(body, section_header(.x))
    # TODO Use mc_dom_elements to add images
    # if (section_has$img(.x))
    #   xml2::xml_add_child(body, section_image(.x))
    # Add content
    if (section_has$text_content(.x))
      xml2::xml_add_child(body, section_text(.x))

    assign("body", body, envir = e)
    if (section_has$img(.x))
      NULL

  })
  # International Vipassana Newsletter ----
  # Thu Dec 29 16:50:51 2022
  if (length(sections$IVN))
    update |>
      rvest::html_element("#update-ivn") |>
      xml2::xml_replace(.value = section_text(sections$IVN, boxed = TRUE))

  return(update)
}

update_write <- function(update, file = dirs$www("update_final", ext = "html", mkpath = TRUE)) {
  xml2::write_html(update, file = file)
  file
}



