#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom polished polished_config secure_ui secure_server
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  addResourcePath("www", system.file("app/www", package = "ExecDashv2"))

  polished_config(
    app_name = "ExecDash",
    api_key = "NRhyuNSpdTcQssG14iCRPPp10z0HEkWwyY",
    cookie_expires = NULL
  )

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = polished::secure_ui(app_ui,
                               sign_in_page_ui = polished::sign_in_ui_default(
                                 color = "#e6af2e",
                                 company_name = "Starita Solutions",
                                 logo_top = tags$img(
                                   src = "www/SS_Bulb_Gold.png",
                                   alt = "Starita Solutions Logo",
                                   style = "width: 125px; margin-top: 30px; margin-bottom: 30px;"
                                 ),
                                 logo_bottom = tags$img(
                                   src = "www/SS_Logo_Black.png",
                                   alt = "Starita Solutions Logo",
                                   style = "width: 200px; margin-bottom: 15px; padding-top: 15px;"
                                 ),
                                 icon_href = "www/SS_Bulb_Gold.png",
                                 background_image = "www/bg_bggenerator_com.jpg"
                               )),
      server = polished::secure_server(app_server),
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

