#' Install the anystyle and anystyle-cli ruby gems
#'
#' @return
#' @export
#'
#' @examples
install_anystyle = function(){

  spawn_cmd(c("gem", "install", "anystyle-cli"))
  spawn_cmd(c("gem", "install", "anystyle"))
}


#' Install the anystyle and anystyle-cli ruby gems
#'
#' @return
#' @export
#'
#' @examples
uninstall_anystyle = function(){

  spawn_cmd(c("gem", "uninstall", "anystyle-cli"))
  spawn_cmd(c("gem", "uninstall", "anystyle"))
}
