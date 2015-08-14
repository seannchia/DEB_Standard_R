sourceDir <- function(path, trace = TRUE,...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(nm=="sourceDir.R" | nm=="main.R"){
      next
    }
    if(trace){
      cat(nm,":")
    }
    source(file.path(path, nm), ...)
    if(trace){
      cat("\n")
    }
  }
}