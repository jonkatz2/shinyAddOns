checkboxInputSpan <- function (inputId, label, value = FALSE, width = NULL) 
{
    inputTag <- shiny::tags$input(id = inputId, type = "checkbox")
    if (!is.null(value) && value) 
        inputTag$attribs$checked <- "checked"
    shiny::span(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", shiny::validateCssUnit(width), ";"), shiny::span(class = "checkbox", 
        shiny::tags$label(inputTag, shiny::tags$span(label))))
}

