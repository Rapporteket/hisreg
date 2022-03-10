
headerFooter <- function(data) {
  data <- as.data.frame.matrix(data)
  dataStr <- dim(data)
  hr <-  c("", names(data))
  fr <-  c("sum", data[dataStr[1], 1:dataStr[2]])
  sketch <- htmltools::tags$table(
    tableHeader(names = hr),
    tableFooter(names = fr))
  return(sketch)
}

tabellUI <- function(id, datoStart = "2008-01-01",
                     datoSlutt = Sys.Date(), forltype = typInt) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3, div( id= ns("sbPanel"),
      shiny::dateRangeInput(ns("dato"),
                            "Tidsperiode:",
                            language = "no",
                            separator = "til",
                            start = datoStart,
                            end = datoSlutt,
                            format = "yyyy-mm-dd"),
      shiny::uiOutput(ns("tabui"))),
      shiny::actionLink(inputId=ns("nullstill"),
                        style="color:black" ,
                        label = "Nullstill Valg")
    ),
    shiny::mainPanel(width = 9,
      shiny::tabsetPanel(id = ns("tab"),
        shiny::tabPanel("Antall unike pasienter/Pasientforløp",
                        value = "forlPas",
           shiny::h3(textOutput(ns("txt1")),
                     style = "text-align:center"),
           shiny::uiOutput(ns("tidsIntervall")),
           DT::DTOutput(ns("Tabell1")),
           shiny::downloadButton(ns("lastNedTabell1"),
                                "Last ned tabell")
        ),
        shiny::tabPanel("Antall skjema",
          value = "skjema",
           shiny::h3(textOutput(ns("txt2")),
                     style = "text-align:center"),
           DT::DTOutput(ns("Tabell2")),
           shiny::downloadButton(ns("lastNedTabell2"),
                                 "Last ned tabell")
        )
      )
    )
  )
}

tabell <- function(input, output, session, ss, SkjemaOversikt, RegData) {
  output$tabui <- shiny::renderUI({
    ns <- session$ns
    if (input$tab == "forlPas") {
      tagList(
        shiny::sliderInput(ns("ald"),
                         label = "Alder",
                         min = 0,
                         max = 120,
                         value = c(0, 120)),
        shiny::selectInput(ns("kjo"),
                         "Kjønn",
                         choices = c("Begge" = 99,
                                     "Kvinne" = 0,
                                     "Mann" = 1),
                         selected = 99),
        shiny::selectInput(ns("tidenh"),
                         "Velg tidsenhet",
                         choices = c("Måned" = "maaned",
                                     "År" = "aar"),
                         selected = "aar"),
        shiny::selectInput(ns("forl"),
                         label = "Forløpstype",
                         choices = typInt1,
                         selected = typInt1,
                         multiple = TRUE,
                         selectize = TRUE),

          shiny::radioButtons(ns("skjemarad"),
                          "",
                          choices = c("Forløp" = "MCEID",
                                      "Pasient" = "PasientID"),
                          inline = TRUE))
    }else if (input$tab ==  "skjema") {
      tagList(
        shiny::selectInput(ns("status"),
                         label = "Skjema status",
                         choices = c("Ferdigstilt" = 1,
                                     "I kladd" = 99),
                         selected = 1),
       shiny::selectInput(ns("typeDato"),
                         label = "Dato for filtrering",
                         choices = c("OpprettetDato" =
                                       "OpprettetDato",
                                     "HovedDato" =
                                       "HovedDato",
                                     "Dato fra arbeidsliste" =
                                       "REGISTRATION_DATE")))
    }

  })
  observeEvent(req(input$nullstill), {shinyjs::reset("sbPanel")})
  output$tidsIntervall <- renderUI({
    ns <- session$ns
    ic <- icon("calendar-alt")
    st <- "color : grey ; background-color:white "
    if (input$tab == "forlPas" & req(input$tidenh) == "maaned") {
      tagList(
        shiny::fluidRow(
          column(3,offset = 9,
            shiny::actionButton(ns("tre"), "3 mnd", ic, style = st, width = "30%"),
            shiny::actionButton(ns("seks"), "6 mnd", ic, style = st,width = "30%"),
            shiny::actionButton(ns("et"), "1 år", ic, style =st,width = "30%")
          )
        )
      )
    }
  })
  forloptxt <- reactive({

      if (req(input$skjemarad) == "MCEID") {
        "registrerte pasientforløp"
      } else {
        "unike pasienter"
      }

  })
  tidenhtxt <- reactive({
    if (req(input$tidenh) == "maaned") {
        "måned"
      }else {
        "år"
      }

  })
  output$txt1 <- renderText({
    paste0("Antall ",
           forloptxt(),
           " per ",
           tidenhtxt(),
           " per avdeling")
    })
  observeEvent(input$tre,{
    valgtDato <- as.Date(max(input$dato)) -
      lubridate::day(as.Date(max(input$dato))) + 1

    shiny::updateDateRangeInput(
      session,
      inputId = "dato",
      start = valgtDato %m-% months(3),
    )
  })
  observeEvent(input$seks,{
    valgtDato <- as.Date(max(input$dato)) -
      lubridate::day(as.Date(max(input$dato))) + 1

    shiny::updateDateRangeInput(
      session,
      inputId = "dato",
      start = valgtDato %m-% months(6),
    )
  })
  observeEvent(input$et,{
    valgtDato <- as.Date(max(input$dato)) -
      lubridate::day(as.Date(max(input$dato))) + 1

    shiny::updateDateRangeInput(
      session,
      inputId = "dato",
      start = valgtDato %m-% months(12),
    )
  })


  tabellData <- reactive({
     if (req(input$tab) == "forlPas") {

         hisregForlPasTabell(RegData,
                        tidFra = min(req(input$dato)),
                        tidTil = max(req(input$dato)),
                        aldmin = req(input$ald[1]),
                        aldmax = req(input$ald[2]),
                        kjoen = req(as.numeric(input$kjo)),
                        tidenh = req(input$tidenh),
                        IDType = req(input$skjemarad),
                        frlType = req(as.numeric(input$forl)))

     } else if (req(input$tab) == "skjema") {

        hisregSkjemaTabell(
          SkjemaOversikt %>%
            dplyr::filter( AvdRESH != 999002), #fjerner Roskilede
          tidFra = min(req(input$dato)),
          tidTil = max(req(input$dato)),
          status = req(input$status),
          typeDato = req(input$typeDato)
          )

    }
    })
  output$lastNedTabell1 <- downloadHandler(
    filename = function() {
      if (req(input$skjemarad) == "PasientID") {
        paste0("pasienttabell", req(input$tidenh), Sys.time(), ".csv")
      } else {
        paste0("forlopstabell", req(input$tidenh), Sys.time(), ".csv")
      }
    },
    content = function(file) {
      tab <- tabellData()
     write.csv2(tab, file, row.names = T)
    }
  )
  output$lastNedTabell2 <- downloadHandler(
    filename = function() {
      if (req(input$status) == "1") {
        paste0("ferdistilteskjema", Sys.time(), ".csv")
      } else {
        paste0("Uferdigeskjema", Sys.time(), ".csv")
      }
    },
    content = function(file) {
      tab <- tabellData()
      write.csv2(tab, file, row.names = T)
    }
  )
  observe({
    if (!is.null(tabellData())) {
    cont <- headerFooter(tabellData())
    subS <- dim(tabellData())[1] - 1
    output$Tabell1 <-  renderDT(
      tabellData() [1:subS, ] %>%
        DT::datatable(
          container = cont,
          selection = "none",
          extensions = "FixedHeader",
          options = list(pageLength = 50,
                         fixedHeader = TRUE,
                         lengthChange = FALSE,
                         dom = "t"))
    )}
   })
  observe({
    if (!is.null(tabellData())) {
    cont <- headerFooter(tabellData())
    subS <- dim(tabellData())[1] - 1
    output$Tabell2 <-  renderDT(
      tabellData()[1:subS, ] %>%
        DT::datatable(
          container = cont,
          selection = "none",
          extensions = "FixedHeader",
          options = list(pageLength = 50,
                         fixedHeader = TRUE,
                         lengthChange = FALSE,
                         dom = "t"))
    )}
  })
  shiny::observe({
    if (rapbase::isRapContext()) {
      if (input$tab == "forlPas") {
        mld <- paste(
          "Hisreg: tabell", input$skjemarad
        )
      } else if (input$tab == "skjema") {
        mld <- paste(
          "Hisreg: tabell - skjematabell"
        )
      }
      rapbase::repLogger(
        session = ss,
        msg = mld
      )
      mldNlSkj <- paste(
        "Hisreg: nedlasting skjematabell",
        input$varSel
      )
      mldNlFol <- paste(
        "Hisreg: nedlasting", input$skjemarad," tabell"
      )
      shinyjs::onclick(
        "lastNedTabell1",
        rapbase::repLogger(
          session = ss,
          msg = mldNlFol
        )
      )
      shinyjs::onclick(
        "lastNedTabell2",
        rapbase::repLogger(
          ss,
          msg = mldNlSkj
        )
      )
    }
  })

}
