#' Create a Mailchimp header with image background section
#'
#' @param bg_src \code{chr} URL of background image to use
#' @param text \code{chr} Text of the header
#'
#' @return \code{shiny.tag}
#' @export
#'
#' @examples
#' mc_bg_header()
mc_bg_header <-
  function(bg_src = "https://www.patapa.dhamma.org/wp-content/uploads/2022/12/aosm-2022.jpeg",
           text = "Annual Old Student Meeting") {
    tags$table(
      border = "0",
      cellpadding = "0",
      cellspacing = "0",
      width = "100%",
      class = "mcnBoxedTextBlock",
      style = "min-width:100%;",
      tags$tbody(class = "mcnBoxedTextBlockOuter",
                 tags$tr(
                   tags$td(
                     valign = "top",
                     class = "mcnBoxedTextBlockInner",
                     tags$table(
                       align = "left",
                       border = "0",
                       cellpadding = "0",
                       cellspacing = "0",
                       width = "100%",
                       style = "min-width:100%;",
                       class = "mcnBoxedTextContentContainer",
                       tags$tbody(tags$tr(
                         tags$td(
                           style = "padding-top:9px; padding-left:18px; padding-bottom:9px; padding-right:18px;",
                           tags$table(
                             border = "0",
                             cellspacing = "0",
                             class = "mcnTextContentContainer",
                             width = "100%",
                             style = "min-width:100% !important;",
                             tags$tbody(tags$tr(
                               tags$td(
                                 valign = "top",
                                 class = "mcnTextContent",
                                 style = "padding: 18px;color: #0E35F8;font-size: 14px;font-weight: normal;text-align: center;",
                                 tags$table(
                                   class = "bg",
                                   background = bg_src,
                                   style = "text-align: center;background-position: center;background-repeat:no-repeat;background-size:cover;text-shadow: 1px 1px #fff;height:200px;min-width:100%",
                                   tags$tbody(tags$tr(tags$td(
                                     tags$div(
                                       style = "text-align:center;width:100%;",
                                       tags$h1(style = "text-align:center;color:rgba(219,201,105);text-shadow:1px 1px rgba(255, 255, 255, .8);",
                                               text)
                                     )
                                   )))
                                 )
                               )
                             ))
                           )
                         )
                       ))
                     )
                   )
                 ))
    )
  }


#' Create a Mailchimp text content blcok
#'
#' @param content \code{shiny.tag.list}
#'
#' @return \code{shiny.tag}
#' @export
#'
#' @examples
#' mc_text_content(tags$p("My Content"))
mc_text_content <- function(content) {
  tags$table(
    border = "0",
    cellpadding = "0",
    cellspacing = "0",
    width = "100%",
    class = "mcnTextBlock",
    style = "min-width:100%;",
    tags$tbody(
      class = "mcnTextBlockOuter",
      tags$tr(
        tags$td(
          valign = "top",
          class = "mcnTextBlockInner",
          style = "padding-top:9px;",
          tags$table(
            align = "left",
            border = "0",
            cellpadding = "0",
            cellspacing = "0",
            style = "max-width:100%; min-width:100%;",
            width = "100%",
            class = "mcnTextContentContainer",
            tags$tbody(
              tags$tr(
                tags$td(
                  valign = "top",
                  class = "mcnTextContent",
                  style = "padding-top:0; padding-right:18px; padding-bottom:9px; padding-left:18px;",
                  tags$div(
                    class = "elementor-element elementor-element-06dd258 update-content elementor-widget elementor-widget-text-editor",
                    `data-id` = "06dd258",
                    `data-element_type` = "widget",
                    `data-widget_type` = "text-editor.default",
                    tags$div(
                      class = "elementor-widget-container",
                      content
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

mc_imgs <- function(imgs) {
  srcs <- purrr::map(imgs, rvest::html_attr, name = "src")

  #TODO split into groups of twos
  tags$table(
    border = "0",
    cellpadding = "0",
    cellspacing = "0",
    width = "100%",
    class = "mcnCodeBlock",
    tags$tbody(
      class = "mcnTextBlockOuter",
      tags$tr(
        rlang::exec(tagList,
                  !!!purrr::map(srcs, \(.x) {
                    tags$td(
                      valign = "top",
                      class = "mcnTextBlockInner",
                      tags$img(
                        src = .x,
                        width = "100%"
                      )
                    )
                  }))
      )
    )
  )
}
