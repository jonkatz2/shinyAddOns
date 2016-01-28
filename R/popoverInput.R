# A Function to place a help icon with popover text next to a shiny input function
# Requires Eric Bailey's shinyBS package (available from CRAN) 

# fnType: function or function name (character) of desired input, e.g. textInput or 'textInput'
# id: inputID (character)
# label: input label (character)
# \dots: additional named arguments to input function
# helpText: character, text to place in popover
# divStyle: character or NULL, inline CSS for main div returned
# helpStyle: character or NULL, inline CSS for the help icon (e.g. padding to align it vertically with input)
# popTrigger: character, trigger for popover. in c('hover','focus','click','manual') 
# colWidths: length 2 numeric vector specifying width of help button column and width of input function column. Should sum to 12.

# EXAMPLE
#    popoverInput(
#        checkboxGroupInput, 
#        'lyrOpts', 
#        'Options:',
#        choices=c(
#            'Paste URL'='url',
#            'Clip to Layer'='clip', 
#            'Projection Ref'='projection', 
#            'No Data'='empty'
#            ), 
#        selected=c('clip','projection'), 
#        inline=TRUE,
#        helpText="To download a layer from a website check \"Paste URL\". You may also clip or project all rasters to match this one. If the layer contains only boundary or projection information check \"No Data\".",
#        divStyle='padding-top:1em;padding-bottom:1em;',
#        helpStyle='padding-top:0.5em;'
#    )

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
    div(style=divStyle,
        column(width=colWidths[1],
            div(style=helpStyle, 
                actionLink(paste0(id,'Help'),'',icon=icon('question-circle'))
            ),
            bsPopover(id=paste0(id,'Help'), title=NULL, content=helpText, trigger=popTrigger, options=list(container='body'))
        ),
        column(width=colWidths[2], 
            do.call(fnType,c(list(inputId=id,label=label), fnParams))
        ) 
    )
}
