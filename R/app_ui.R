#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("Email Update Generator"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            textInput(
              "url",
              label = "URL Path to Update",
              value = update_url(url_path = c(lubridate::year(lubridate::today()), tolower(format(lubridate::today(), "%B-%Y-update"))))
            )
          ),
          fluidRow(
            actionButton(
              "generate",
              label = "Generate Email Update",
              icon = shiny::icon("file-pen")
            ),
            downloadButton(
              "download",
              label = "Download Update HTML"
            )
          ),
          fluidRow(
            h3("Help"),
            p("If the update doesn't come out as expected there are a couple of options:"),
            tags$ol(
              tags$li(
                "Try editing the update directly in Elementor to fix any issues and regenerate."
              ),
              tags$li(
                "Download the update HTML and make edits in ", tags$a(href = "https://code.visualstudio.com/download", target = "_blank", "Visual Studio Code")
              ),
              tags$li(
                "Reach out to me for assistance"
              )
            )
          )
        ),
        mainPanel(
          uiOutput("preview")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )


  tags$head(
    favicon(),
    OpenMCE::use_editor(),
    tags$style(src = "www/css/custom.min.css"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "update_helper"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
