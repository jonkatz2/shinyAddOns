#' 
#' bsModal2
#' 
#' \code{bsModal2} is used within the UI to create a modal window. 
#' 
#' @param id A unique identifier for the modal window
#' @param title The title to appear at the top of the modal
#' @param trigger The id of a button or link that will open the modal.
#' @param \dots UI elements to include within the modal
#' @param class Class to customize look via CSS. \code{NULL} for the default \code{shinyBS} look, or a named vector or list with one or more classes applied to fade, content, header, close (the ``x'' at upper right), title, body, footer, and btn elements.  
#' @param size Optional What size should the modal be? (small or large)
#' @author Eric Bailey
#' @details See \code{\link[shinyBS]{Modals}} for more information about how to use the parent function, \code{bsModal}, with the rest of the Modals family. The \code{class} argument was added by Jon Katz.
#' 
#' @note Run \code{bsExample("Modals")} for an example of \code{bsModal} functionality. 
#' @seealso \href{http://getbootstrap.com/}{Twitter Bootstrap 3}
#' @keywords manip
#' @export
#' 
#' 

bsModal2 <- function (id, title, trigger, ..., class=NULL, size) 
{
    if (!missing(size)) {
        if (size == "mega") {
            size = "modal-mega"
        }
        else if (size == "large") {
            size = "modal-lg"
        }
        else if (size == "small") {
            size = "modal-sm"
        }
        size <- paste("modal-dialog", size)
    } else {
        size <- "modal-dialog"
    }
    user.class <- list("fade"=character(), "content"=character(), "header"=character(), "close"=character(), "title"=character(), "body"=character(), "footer"=character(), "btn"=character())
    if(!is.null(class)) {
        for(i in names(class)) user.class[[i]] <- tryCatch(class[[i]], error=function(e) NULL)
    } 
    bsTag <- shiny::tags$div(class = paste("modal sbs-modal fade", user.class[["fade"]]), 
        id = id, tabindex = "-1", `data-sbs-trigger` = trigger, 
        shiny::tags$div(class = size, shiny::tags$div(class = paste("modal-content", user.class[["content"]]), 
            shiny::tags$div(class = paste("modal-header", user.class[["header"]]), shiny::tags$button(type = "button", 
                class = paste("close", user.class[["close"]]), `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), 
                shiny::tags$h4(class = paste("modal-title", user.class[["title"]]), title)), 
            shiny::tags$div(class = paste("modal-body", user.class[["body"]]), list(...)), 
            shiny::tags$div(class = paste("modal-footer", user.class[["footer"]]), shiny::tags$button(type = "button", 
                class = paste("btn btn-default", user.class[["btn"]]), `data-dismiss` = "modal", 
                "Close")))))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

