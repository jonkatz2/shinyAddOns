#' 
#' bsModal
#' 
#' \code{bsModal} is used within the UI to create a modal window. 
#' 
#' @param id A unique identifier for the modal window
#' @param title The title to appear at the top of the modal
#' @param trigger The id of a button or link that will open the modal.
#' @param \dots UI elements to include within the modal
#' @param class Class to customize look via CSS
#' @param size Optional What size should the modal be? (small or large)
#' @author Eric Bailey
#' @details See \code{\link[shinyBS]{Modals}} for more information about how to use \code{bsModal} with the rest of the Modals family. The \code{class} argument was added by Jon Katz.
#' 
#' @note Run \code{bsExample("Modals")} for an example of \code{bsModal} functionality. 
#' @seealso \href{http://getbootstrap.com/}{Twitter Bootstrap 3}
#' @keywords manip
#' @export
#' 
#' 

bsModal <- function (id, title, trigger, ..., class=NULL, size) 
{
    if (!missing(size)) {
        if (size == "large") {
            size = "modal-lg"
        }
        else if (size == "small") {
            size = "modal-sm"
        }
        size <- paste("modal-dialog", size)
    } else {
        size <- "modal-dialog"
    }
    bsTag <- shiny::tags$div(class = paste("modal sbs-modal fade", class), 
        id = id, tabindex = "-1", `data-sbs-trigger` = trigger, 
        shiny::tags$div(class = size, shiny::tags$div(class = paste("modal-content", class), 
            shiny::tags$div(class = paste("modal-header", class), shiny::tags$button(type = "button", 
                class = paste("close", class), `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), 
                shiny::tags$h4(class = "modal-title", title)), 
            shiny::tags$div(class = paste("modal-body", class), list(...)), 
            shiny::tags$div(class = paste("modal-footer", class), shiny::tags$button(type = "button", 
                class = paste("btn btn-default", class), `data-dismiss` = "modal", 
                "Close")))))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

