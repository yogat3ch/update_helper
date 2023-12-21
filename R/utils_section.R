
#' Create a Mailchimp text section
#'
#' @param content \code{xml_node}
#'
#' @return \code{xml_doc}
#' @export
#'

section_text <- function(content, boxed = FALSE) {
  template = if (boxed) {
    dirs$extdata("mc_boxed_text.html")
  } else {
    dirs$extdata("mc_text.html")
  }
  htm <- xml2::read_xml(template, options = c("IGNORE_ENC"))
  .content <- rvest::html_elements(content, ".update-content")
  if (length(.content))
    content <- .content
  # Handle single nodes & multiple nodes
  if (inherits(content, "xml_node"))
    xml2::xml_add_child(rvest::html_element(htm, "td.mcnTextContent"), .value = content)
  else
    purrr::walk(content, \(.x) {
      xml2::xml_add_child(rvest::html_element(htm, "td.mcnTextContent"), .value = .x)
    })
  htm
}


section_has <- purrr::map(
  c(header = "h1",
    text_content = ".update-content",
    img = "img"),
  ~rlang::new_function(rlang::pairlist2(section= , has = .x), rlang::expr({!rlang::is_empty(rvest::html_element(section, has))})))

#' Create a Mailchimp header section
#'
#' @param content \code{xml_node}
#'
#' @return \code{xml_doc}
#' @export


section_header <- function(el, url = NULL, text = NULL) {
  template = dirs$extdata("mc_header.html")
  htm <- xml2::read_xml(template)
  if (!missing(el)) {
    if (is.null(url)) {
      # Look for the header-bg class
      url <- rvest::html_node(el, "div.header-bg") |>
        rvest::html_attr("style") |>
        stringr::str_extract("(?<=url\\(\\')[^\\)\\']+")
    }
    # If somehow missing, look by position
    if (!UU::is_legit(url)) {
      url <- rvest::html_element(el, "div.update-header") |>
        rvest::html_element("div") |>
        rvest::html_element("div") |>
        rvest::html_attr("style") |>
        stringr::str_extract("(?<=url\\(\\')[^\\)\\']+")
    }

    if (!UU::is_legit(url)) {
      # if still missing throw informative error
      header <- trimws(stringr::str_remove_all(rvest::html_text(htm), "\t") |>
        stringr::str_replace_all("\n", " "))
      UU::gbort("Failed to parse img bg url of section with header: {.code {header}}. Does the div with background attribute have a style of {.code header-bg}?")
    }

    if (is.null(text))
      text <- rvest::html_node(el, "h1") |>
        rvest::html_text()
  }
  htm |>
    rvest::html_element("table.bg") |>
    xml2::xml_set_attr("background", url)
  htm |>
    rvest::html_element("h1") |>
    xml2::`xml_text<-`(text)
  htm
}

#' Create a Mailchimp image section
#'
#' @param content \code{xml_node}
#'
#' @return \code{xml_doc}
#' @export
#
section_images <- function(el, src = NULL) {
  template = dirs$extdata("mc_img.html")
  if (!missing(el)) {
    if (is.null(src))
      src <- rvest::html_elements(el, "img") |>
        rvest::html_attr("src")
  }
  htm <- xml2::read_xml(template)
  purrr::map(src, \(.x) {
    rvest::html_element(htm, "img") |>
      xml2::`xml_attr<-`(attr = "src", value = .x)
    htm
  })

}
