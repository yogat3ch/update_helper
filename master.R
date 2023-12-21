remDr <- selenium_start()

doc <- update_get_html(url_path = c(lubridate::year(date), tolower(format(lubridate::today(), "%B-%Y-update"))))
sections <- update_get_sections(doc)
update <- update_prep(sections)
update_write(update)
if (exists("remDr"))
  selenium_end(remDr)
