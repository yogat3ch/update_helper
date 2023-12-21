#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  file_path <- reactiveVal(UU::path_strip_shiny(dirs$www("update_template.html")))

  observeEvent(input$generate, {
    doc <- update_get_html(url_path = input$url)
    sections <- update_get_sections(doc)
    update <- update_prep(sections)
    fp <- update_write(update)
    file_path(UU::path_strip_shiny(fp))
  })



  output$preview <- renderUI({
    input$tab
    tags$iframe(src = file_path(), id = "preview_frame")
  })


  output$download <- downloadHandler("update_final.html", content = function(file) {
    file.copy(dirs$www("update_final.html"), file)
  }, contentType = "text/html")
}
