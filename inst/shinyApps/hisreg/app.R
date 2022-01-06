library(shiny)
library(hisreg)
library(tidyverse)
library(shinyalert)
library(shinyjs)
library(kableExtra)
library(DT)
library(htmltools)
library(rapbase)
library(lubridate)

system.file(
  "shinyApps/hisreg/R/startside.R",
  package = "hisreg"
) %>%
  source(encoding = "UTF-8")
system.file(
  "shinyApps/hisreg/R/dataOgVar.R",
  package = "hisreg"
) %>%
  source(encoding = "UTF-8")
system.file(
  "shinyApps/hisreg/R/modFordelinger.R",
  package = "hisreg"
) %>%
  source(encoding = "UTF-8")
system.file(
  "shinyApps/hisreg/R/modGjennomsnitt.R",
  package = "hisreg"
) %>%
  source(encoding = "UTF-8")
system.file(
  "shinyApps/hisreg/R/ModTabeller.R",
  package = "hisreg"
) %>%
  source(encoding = "UTF-8")
# system.file(
#   "shinyApps/hisreg/R/ModTabeller.R",
#   package = "hisreg"
# ) %>%
#   source(encoding = "UTF-8")
system.file(
  "shinyApps/hisreg/R/modul_admtab.R",
  package = "hisreg"
) %>%
  source(encoding = "UTF-8")

#------------App UI----------------

ui <- shiny::tagList(
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  shiny::navbarPage(
    title = div(a(includeHTML(system.file("www/logo.svg",
                                          package = "rapbase"))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    shiny::tabPanel("Startside",
                    rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                 organization = uiOutput("appOrgName"),
                                                 addUserInfo = TRUE),
                    tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                    startsideUI("startside")
    ),
    tabPanel("Fordelinger",
             modFordelingerUI("mod1")),
    shiny::navbarMenu("Gjennomsnitt",
                      shiny::tabPanel("Per sykehus",
                                      modGjennomsnittUI("mod2")
                      ),
                      shiny::tabPanel("Per intervensjonstype",
                                      modGjennomsnittUI("mod3")),
                      shiny::tabPanel("Før og etter intervensjon, per sykehus",
                                      modGjennomsnittUI("mod4", varValg = varValgGjenFE)),
                      shiny::tabPanel("Før og etter intervensjon, per intervensjonstype",
                                      modGjennomsnittUI("mod5", varValg = varValgGjenFE))
    ), #navbarMenu,

    tabPanel("Administrative tabeller",
             tabellUI("tab")),
    tabPanel("Nye administrative tabeller",
             admtab_UI("tab_ny")),
    tabPanel(
      "Datadump", dataDumpUI("dataDumpHisreg")
    ),
    shiny::tabPanel(
      "Eksport",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::exportUCInput("hisregExport")
        ),
        shiny::mainPanel(
          rapbase::exportGuideUI("hisregExportGuide")
        )
      )
    )


  )#navbarPage
)#taglist

#----------------App server------------------------

server <-  function(input, output, session) {

  # print(names(allevar))
  # print(dim(allevar))

  reshID <- reactive({
    ifelse(onServer,as.numeric(rapbase::getUserReshId(session)),601031)
  })
  userRole <- reactive({
    ifelse(onServer, rapbase::getUserRole(session), 'SC')
  })
  if (onServer){
    rapbase::appLogger(session, msg = "Hisreg: Shiny app starter")
  }

  observe(
    if (userRole() != "SC") {
      shinyjs::hide(
        selector =  ".dropdown-menu li:nth-child(1)")
      shinyjs::hide(
        selector =  ".dropdown-menu li:nth-child(3)")
    } else {
      shiny::callModule(modGjennomsnitt, "mod2", rID = reshID(), ss = session,
                        add_int = TRUE, add_enh = FALSE, fun = "PS")
      shiny::callModule(modGjennomsnitt, "mod4", rID = reshID(), ss = session,
                        add_int = TRUE, add_enh = FALSE, fun = "FEPS")
    }
  )
  shiny::callModule(startside, "startside", usrRole=userRole())
  shiny::callModule(modFordelinger, "mod1", rID = reshID(), role = userRole(),
                    ss = session)

  shiny::callModule(modGjennomsnitt, "mod3", rID = reshID(), ss = session,
                    add_int = FALSE, add_enh = FALSE, fun = "PI")

  shiny::callModule(modGjennomsnitt, "mod5", rID = reshID(), ss = session,
                    add_int = FALSE, add_enh = TRUE, fun = "FEPI")
  shiny::callModule(tabell, "tab", ss = session)
  shiny::callModule(admtab, "tab_ny", skjemaoversikt=SkjemaOversikt_ny) # , skjemaoversikt=SkjemaOversikt_ny
  shiny::callModule(dataDump, "dataDumpHisreg", mainSession = session,
                    reshID = reshID(), userRole = userRole())

  ##########################################################################################################
  # Eksport  ###############################################################################################
  # brukerkontroller
  rapbase::exportUCServer("hisregExport", "hisreg")

  ## veileding
  rapbase::exportGuideServer("hisregExportGuide", "hisreg")

  ##########################################################################################################

  #Navbarwidget
  output$appUserName <- shiny::renderText(rapbase::getUserFullName(session))
  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = "Den er grei!")
  })
}

shinyApp(ui, server)
