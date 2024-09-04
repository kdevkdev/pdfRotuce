# function to esscape special chars in rmd or possibly latex
rmd_char_escape = function(mrkdwn){

  # cumulatively replace chars
  mrkdwn = stringr::str_replace_all(mrkdwn, pattern = "\\$", replacement = "\\\\$")
  mrkdwn = stringr::str_replace_all(mrkdwn, pattern = "\\@", replacement = "\\\\@")
  mrkdwn = stringr::str_replace_all(mrkdwn, pattern = "\\%", replacement = "\\\\%")
}
# double escape version
rmd_char_descape = function(mrkdwn){

  # cumulatively replace chars
  mrkdwn = stringr::str_replace_all(mrkdwn, pattern = "\\$", replacement = "\\\\\\\\$")
  mrkdwn = stringr::str_replace_all(mrkdwn, pattern = "\\@", replacement = "\\\\\\\\@")
  mrkdwn = stringr::str_replace_all(mrkdwn, pattern = "\\%", replacement = "\\\\\\\\%")
}

