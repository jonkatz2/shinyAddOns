inumericInput <- function (inputId, label, label2=NULL, value, min = NA, max = NA, step = NA, 
    label.style='float:left;padding:0.5em 1em 0.5em 0em;', input.style='width:75%;border-radius:4px;', container.style="margin-bottom:0px;", container=list(outer=shiny::div, inner=shiny::span)) 
{
    inputTag <- shiny::tags$input(id = inputId, type = "number", class = "form-control", style=input.style, value = shiny:::formatNoSci(value))
    if (!is.na(min)) 
        inputTag$attribs$min = min
    if (!is.na(max)) 
        inputTag$attribs$max = max
    if (!is.na(step)) 
        inputTag$attribs$step = step
    
    if(length(container) == 1) {
        container <- c(container, list(inner=shiny::span)) 
        names(container)[1] <- 'outer'
    }
        
    container[['outer']](class = "form-group shiny-input-container", style=container.style,
        if(!is.null(label2)) shiny::tags$label(label2),
        container[['inner']]( 
            if(label != '') shiny::tags$label(style=
                label.style, label, `for` = inputId
            ), 
            inputTag
        )
    )
}


itextInput <- function (inputId, label, label2=NULL, value = "", width = NULL, placeholder = NULL, label.style='float:left;padding:0.5em 1em 0.5em 0em;', input.style='width:75%;border-radius:4px;', container.style="margin-bottom:0px;", container=list(outer=shiny::div, inner=shiny::span)) 
{
    container[['outer']](class = "form-group shiny-input-container", style=container.style,
        if(!is.null(label2)) shiny::tags$label(label2),
        container[['inner']]( 
            if(label != '') shiny::tags$label(style=
                label.style, label, `for` = inputId
            ),
            shiny::tags$input(
                style = input.style,
                id = inputId, 
                type = "text", class = "form-control", value = value, 
                placeholder = placeholder
            )
        )
    )
}

iradioButtons <- function (inputId, label, choices, selected = NULL, inline = FALSE, 
    width = NULL, padding='0.5em 1em 0.5em 0em', thin=FALSE, container=shiny::div) 
{
    choices <- shiny:::choicesWithNames(choices)
    selected <- if (is.null(selected)) 
        choices[[1]]
    else {
        shiny::validateSelected(selected, choices, inputId)
    }
    if (length(selected) > 1) 
        stop("The 'selected' argument must be of length 1")
    options <- shiny:::generateOptions(inputId, choices, selected, inline, 
        type = "radio")
    divClass <- "form-group shiny-input-radiogroup shiny-input-container"
    if (inline) 
        divClass <- paste(divClass, "shiny-input-container-inline")
    divStyle <- NULL
    if (!is.null(width)) 
        divStyle <- paste0("width: ", shiny::validateCssUnit(width), ";")
    if(thin) 
        divStyle <- paste0(divStyle, "margin-bottom:0px;")
    
    container(id = inputId, style = divStyle, class = divClass, 
        shiny::tags$span(style=paste0("padding:", padding, ";"),
            if(label != '') label, options)
        )
}


