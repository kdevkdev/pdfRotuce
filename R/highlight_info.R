hgl_error = function(text){

  cat("ERROR!!", crayon::bgRed(crayon::white(crayon::bold(text))))
  stop(text)
}

hgl_warn = function(text){

  cat(crayon::red("WARN!"), crayon::yellow(crayon::bgBlue(crayon::bold(text))), "\n")
}
hgl_warn_S = function(text){

  cat(crayon::red(crayon::bold("SERIOUS WARNING - PLEASE TAKE ACTION !!!!!")), crayon::white(crayon::bgRed(crayon::bold(crayon::italic(text)))), "\n")
}

hgl_note = function(text){

  cat("NOTE:", crayon::cyan(crayon::italic(text)), "\n")
}
