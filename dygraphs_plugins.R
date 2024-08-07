# dygraphs plug-ins
# source: https://rstudio.github.io/dygraphs/gallery-plugins.html



dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
   dyPlugin(
      dygraph = dygraph,
      name = "Crosshair",
      path = system.file("plugins/crosshair.js", 
                         package = "dygraphs"),
      options = list(direction = match.arg(direction))
   )
}


dyUnzoom <-function(dygraph) {
   dyPlugin(
      dygraph = dygraph,
      name = "Unzoom",
      path = system.file("plugins/unzoom.js", package = "dygraphs")
   )
}