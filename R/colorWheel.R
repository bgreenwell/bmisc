## Color wheel function
colorWheel <- function(str, cex = 0.75) {
  cols <- colors()[grep(str, colors())]
  pie(rep(1, length(cols)), labels = cols, col = cols, cex = cex)
  cols
}

