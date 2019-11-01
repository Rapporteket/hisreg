library(shiny)
library(hisreg)
library(tidyverse)
library(shinyalert)
library(shinyjs)
library(kableExtra)
library(DT)
library(htmltools)
library(rapbase)
library(ggthemes)

source("www/dataOgVar.R", encoding = "UTF-8")
source("www/modFordelinger.R", encoding = "UTF-8")
source("www/modGjennomsnitt.R", encoding = "UTF-8")
source("www/ModTabeller.R", encoding = "UTF-8")

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

    shiny::tabPanel("Fordelinger",
      rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                   organization = uiOutput("appOrgName"),
                                   addUserInfo = TRUE),
      modFordelingerUI("mod1")
    ),
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

      tabPanel("Tabeller",
               tabellUI("tab"))


  )#navbarPage
)#taglist

#----------------App server------------------------

server <-  function(input, output, session) {
  reshID <- shiny::reactive({
    ifelse(onServer, as.numeric(rapbase::getShinyUserReshId(session,
      testCase = TRUE)), 601031)
  })
  userRole <- reactive({
    ifelse(onServer, rapbase::getShinyUserRole(session, testCase = TRUE), "SC")
  })

  shiny::callModule(modFordelinger, "mod1", rID = reshID())
  shiny::callModule(modGjennomsnitt, "mod2", rID = reshID(),
                    add_int = TRUE, add_enh = FALSE, fun = "PS")
  shiny::callModule(modGjennomsnitt, "mod3", rID = reshID(),
                    add_int = FALSE, add_enh = FALSE, fun = "PI")
  shiny::callModule(modGjennomsnitt, "mod4", rID = reshID(),
                    add_int = TRUE, add_enh = FALSE, fun = "FEPS")
  shiny::callModule(modGjennomsnitt, "mod5", rID = reshID(),
                    add_int = FALSE, add_enh = TRUE, fun = "FEPI")
  shiny::callModule(tabell, "tab")

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
