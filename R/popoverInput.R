#' popoverInput
#' 
#' Place a help icon with popover text next to a shiny input function using an actionLink 
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
#' @author Jon Katz
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
    helpStyle='padding-top:1.5em;text-align:center;',
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


#' popoverInput2
#' 
#' Place a help icon with popover text next to a shiny input function. Both the help icon and the input element float left.
#' 
#' 
#' @param fnType function or function name (character) of desired input, e.g. textInput or 'textInput'
#' @param id InputID for shiny element (character).
#' @param label Input label (character).
#' @param \dots Additional named arguments to input function.
#' @param helpText Text to place in popover (character). Omit to align an helpless input with a helpful input.
#' @param fa.icon Icon name from font awesome, default is "fa-question-circle".
#' @param fa.color Color of icon; defaults to bootstrap link color.
#' @param padding.top Padding to align the help icon vertically with input label.
#' @param popTrigger Trigger for popover. In c('hover','focus','click','manual').
#' @param container HTML container to hold the input, either \code{div} or \code{span}
#' @param cStyle Inline CSS for main container returned (character or NULL).
#' @return HTML code. 
#' @author Jon Katz
#' @note Requires Eric Bailey's shinyBS package (available from CRAN).
#' @examples
#'    popoverInput2(
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
#'        padding.top="1em"
#'    )
#' @keywords manip
#' @export
#' 
#' 


popoverInput2 <- function(
    fnType,
    ...,
    helpText,
    fa.icon='fa-question-circle',
    fa.color='#337ab7',
    padding.top='0.3em',
    popTrigger='hover click',
    container=shiny::div,
    cStyle='clear:both;'
) {
    fnParams <- list(...)
    if(missing(helpText)) {
        container(style="padding-left:1.3em;",
            do.call(fnType, fnParams)
        )
    } else {
        rid <- paste0(rName(),'Help')
        if(identical(fnType, checkboxInput)) {
            container(style=cStyle,
                div(style='float:left;', shiny::HTML(paste0('<i id=', rid, ' class="fa ', fa.icon,'" style="float:left; padding:',padding.top, ' 0.3em; color:', fa.color,'";></i>')),
                shinyBS::bsPopover(id=rid, title=NULL, content=helpText, trigger=popTrigger, options=list(container='body'))),
                do.call(fnType, fnParams)
            )
        } else {
            container(style=cStyle,
                shiny::HTML(paste0('<i id=', rid, ' class="fa ', fa.icon,'" style="float:left; padding:',padding.top, ' 0.3em; color:', fa.color,'";></i>')),
                shinyBS::bsPopover(id=rid, title=NULL, content=helpText, trigger=popTrigger, options=list(container='body')),
                do.call(fnType, fnParams)
            )
        }
    }
}
