#' popoverInput
#' 
#' Place a help icon with popover text next to a shiny input function
#' 
#' 
#' @param fnType function or function name (character) of desired input, e.g. textInput or 'textInput'
#' @param id InputID for shiny element (character).
#' @param label Input label (character).
#' @param \dots Additional named arguments to input function.
#' @param helpText Text to place in popover (character).
#' @param divStyle Inline CSS for main div returned (character or NULL).
#' @param helpStyle Inline CSS for the help icon (e.g. padding to align it vertically with input; character or NULL).
#' @param popTrigger Trigger for popover. In c('hover','focus','click','manual').
#' @param colWidths Length 2 numeric vector specifying width of help button column and width of input function column. Should sum to 12.
#' @return HTML code. 
#' @note Requires Eric Bailey's shinyBS package (available from CRAN).
#' @examples
#'    popoverInput(
#'        checkboxGroupInput, 
#'        'lyrOpts', 
#'        'Options:',
#'        choices=c(
#'            'Paste URL'='url',
#'            'Clip to Layer'='clip', 
#'            'Projection Ref'='projection', 
#'            'No Data'='empty'
#'            ), 
#'        selected=c('clip','projection'), 
#'        inline=TRUE,
#'        helpText="To download a layer from a website check \"Paste URL\". You may also clip or project all rasters to match this one. If the layer contains only boundary or projection information check \"No Data\".",
#'        divStyle='padding-top:1em;padding-bottom:1em;',
#'        helpStyle='padding-top:0.5em;'
#'    )
#' @keywords manip
#' @export
#' 
#' 

popoverInput <- function(
    fnType,
    id,
    label,
    ...,
    helpText,
    divStyle=NULL,
    helpStyle='padding-top:1.5em',
    popTrigger='hover',
    colWidths=c(2,10)
) {
    fnParams <- list(...)
    shiny::div(style=divStyle,
        shiny::column(width=colWidths[1],
            shiny::div(style=helpStyle, 
                shiny::actionLink(paste0(id,'Help'),'',icon=icon('question-circle'))
            ),
            shinyBS::bsPopover(id=paste0(id,'Help'), title=NULL, content=helpText, trigger=popTrigger, options=list(container='body'))
        ),
        shiny::column(width=colWidths[2], 
            do.call(fnType,c(list(inputId=id,label=label), fnParams))
        ) 
    )
}
