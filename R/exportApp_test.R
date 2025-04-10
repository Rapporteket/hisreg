#' Shiny app with database export functionality
#'
#' @export
exportApp_test <- function() {
  ui <- shiny::navbarPage(
    id = "navbarpage",
    title = shiny::div(shiny::a(shiny::includeHTML(
      system.file(
        "www/logo.svg",
        package = "rapbase"
      )
    )
    ),
    "Simple export app"),
    windowTitle = "Simple export app",
    theme = "rap/bootstrap.css",

    shiny::tabPanel(shiny::p("Hent data", title='Rapporteketadministrasjonens side'),
             value = "Rapporteketadministrasjon",
             shiny::selectInput("registryName",
                                label = "Velg register",
                                choices = c("norgast", "rygg")),
             shiny::tabsetPanel(
               shiny::tabPanel(
                 title = "Info",
                 rapbase::navbarWidgetInput("navbar-widget", selectOrganization = FALSE),
                 shiny::sidebarLayout(
                   shiny::sidebarPanel(shiny::uiOutput("metaControl")),
                   shiny::mainPanel(
                     shiny::uiOutput("n_lines"),
                     shiny::htmlOutput("metaData"))
                 )
               ),
               shiny::tabPanel(
                 title = "Eksport",
                 value = "exportPanel",
                 shiny::sidebarLayout(
                   shiny::sidebarPanel(
                     shiny::uiOutput("exportSidebarPanel")
                   ),
                   shiny::mainPanel(
                     shiny::uiOutput("exportMainPanel")

                   )
                 )
               )
             )
    )
  )
  server <- function(input, output, session) {
    # user <- rapbase::navbarWidgetServer(
    #   id = "navbar-widget",
    #   orgName = "hisreg",
    #   caller = "hisreg"
    # )

    # shiny::observeEvent(user$role(), {
    #   if (user$role() == "SC") {
    #     shiny::showTab(inputId = "navbarpage", target = "exportPanel")
    #   } else {
    #     shiny::hideTab(inputId = "navbarpage", target = "exportPanel")
    #   }
    # })

    # User control
    output$exportSidebarPanel <- shiny::renderUI({
      # if (user$role() == "SC") {
        rapbase::exportUCInput("export")
      # } else {
      #   return(NULL)
      # }
    })

    rapbase::exportUCServer("export", shiny::req(input$registryName))

    # User guide
    output$exportMainPanel <- shiny::renderUI({
      # if (user$role() == "SC") {
        rapbase::exportGuideUI("exportGuide")
      # } else {
      #   return(NULL)
      # }
    })

    rapbase::exportGuideServer("exportGuide", shiny::req(input$registryName))

    ## Metadata
    meta <- shiny::reactive({
      rapbase::describeRegistryDb(registryName = shiny::req(input$registryName))
    })

    meta2 <- shiny::reactive({
      rapbase::nlinesRegistryDb(registryName = shiny::req(input$registryName))
    })

    output$metaControl <- shiny::renderUI({
      tabs <- names(meta())
      selectInput("metaTab", "Velg tabell:", tabs)
    })

    output$metaDataTable <- DT::renderDataTable(
      meta()[[input$metaTab]], rownames = FALSE,
      options = list(lengthMenu=c(25, 50, 100, 200, 400))
    )

    output$metaData <- shiny::renderUI({
      DT::dataTableOutput("metaDataTable")
    })

    output$n_lines <- shiny::renderUI({
      h4(paste0(input$metaTab, " har ",
                meta2()[[shiny::req(input$metaTab)]]$n_lines, " linjer"))
    })

  }
  shiny::shinyApp(ui, server)
}
