#' Run phylosophy app
#' @export
#' @return A shiny application - GUI version of phylosophy
#' @examples
#' ?runPhylosophy
#' \dontrun{
#' runPhylosphy()
#' }

runPhylosophy <- function(){
  appDir <- system.file("phylosophy", package = "phylosophy")
  if (appDir == "") {
    stop(
      "Could not find apps directory. Try re-installing `phylosophy`.",
      call = FALSE
    )
  }
  
  shiny::runApp(
    appDir,
    launch.browser = TRUE,
    display.mode = "normal"
  )
}
