#' Detects platform R is run on
#'
#'
#' @return `osx`, `linux`, or `windows`
#' @export
#'
#' @examples
get_os <- function(){
  # https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/

  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' spawn_cmd
#'
#' Runs a command while continously polling for the console output and forwarding it to the R console (processx based wrapper for R system/system2)
#'
#' @param args Character vector, first item the main command, subsequent items are arguments
#'
#' @return Entire console output of command
#' @export
#'
#' @examples
spawn_cmd = function(args){

  # funciton to execute a command on the console
  # args should be vector with first entry as command, other entries arguments

  # spawn_cmd(c("ping", "www.google.com"))

  # From https://stackoverflow.com/questions/62902042/simultaneously-save-and-print-r-system-call-output

  # detect if on windows or not
  os <- get_os()
  if(os == "windows"){
    proc <- processx::process$new("cmd.exe", c("/c", "call", args), stdout = "|")
  }
  # else if(os == "osx"){
  #   proc <- processx::process$new("zsh", c("-c", args), stdout = "|")
  # }else if(os == "linux"){
  #   #proc <- processx::process$new("bash", c("-c", args), stdout = "|")
  #   proc <- processx::process$new(args[1], args[-1], stdout = "|")
  # }
  else{
    proc <- processx::process$new(args[1], args[-1], stdout = "|")
  }
  output <- character(0)
  while (proc$is_alive()) {
    Sys.sleep(1)
    now <- Sys.time()
    tmstmp <- sprintf("# [%s]", format(now, format = "%T"))
    thisout <- proc$read_output_lines()
    if (length(thisout)) {
      output <- c(output, thisout)
      #message(tmstmp, " New output!\n", paste("#>", thisout))
      message(paste0(thisout, collapse = "\n"))
    }# else message(tmstmp)


  }
  output

}
