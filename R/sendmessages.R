#' sendmessages
#'
#' @description \code{sendmessages} Send a message in console and a notification in shiny if it's running.
#'
#' @param text A string to be printed.
#' @param type Type of the message between "info", "warning", "danger", "success", and "gears".
#' @param duration Number of seconds to display the message before it disappears. Use NULL to make the message not automatically disappear.
#'
#'
#' @importFrom shiny isRunning showNotification
#' @importFrom htmltools HTML
#'


sendmessages = function(text,
                        type,
                        duration = 4
                        ){

  match.arg(type, c("info", "warning", "error", "success", "gears"))

  message(text)

  if(shiny::isRunning()){

    if(type == "info"){
      showNotification(tagList(icon("info"), HTML(paste0("&nbsp;",text))), type = "default", duration = duration)
    }
    if(type == "gears"){
      showNotification(tagList(icon("gears"), HTML(paste0("&nbsp;",text))), type = "default", duration = duration)
    }
    if(type == "warning"){
      showNotification(tagList(icon("circle-exclamation"), HTML(paste0("&nbsp;",text))), type = "warning", duration = duration)
    }
    if(type == "error"){
      showNotification(tagList(icon("circle-xmark"), HTML(paste0("&nbsp;",text))), type = "error", duration = duration)
    }
    if(type == "success"){
      showNotification(tagList(icon("check"), HTML(paste0("&nbsp;",text))), type = "message", duration = duration)
    }
  }
}
