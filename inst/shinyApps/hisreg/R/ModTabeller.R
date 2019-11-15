
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
    shiny::sidebarPanel(width = 3,
      shiny::dateRangeInput(ns("dato"),
                            "Tidsperiode:",
                            start = datoStart,
                            end = datoSlutt,
                            format = "yyyy-mm-dd"),
      shiny::uiOutput(ns("tabui"))
    ),
    shiny::mainPanel(width = 9,
      shiny::tabsetPanel(id = ns("tab"),
        shiny::tabPanel("Pasient og forløpstabeller",
                        value = "forlPas",
           shiny::h3(textOutput(ns("txt1")),
                     style = "text-align:center"),
           DT::DTOutput(ns("Tabell1")),
           shiny::downloadButton(ns("lastNedTabell1"),
                                "Last ned tabell")
        ),
        shiny::tabPanel("Skjematabeller",
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

tabell <- function(input, output, session) {
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
                                     "År" = "aar")),
        shiny::selectInput(ns("forl"),
                         label = "Forløpstype",
                         choices = typInt1,
                         selected = typInt1,
                         multiple = TRUE,
                         selectize = TRUE),

          shiny::radioButtons(ns("skjemarad"),
                          "",
                          choices = c("Forløp" = "m_mceid",
                                      "Pasient" = "PasientID"),
                          inline = TRUE))
    }else if (input$tab ==  "skjema") {
      tagList(
        shiny::selectInput(ns("status"),
                         label = "Skjema status",
                         choices = c("Ferdigstilt" = 1,
                                     "Ikke ferdige" = 99),
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
  forloptxt <- reactive({

      if (req(input$skjemarad) == "m_mceid") {
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
  tabellData <- reactive({
     if (req(input$tab) == "forlPas") {
       as.data.frame.matrix(
         hisregForlPasTabell(RegData,
                        tidFra = req(input$dato[1]),
                        tidTil = req(input$dato[2]),
                        aldmin = req(input$ald[1]),
                        aldmax = req(input$ald[2]),
                        kjoen = req(as.numeric(input$kjo)),
                        tidenh = req(input$tidenh),
                        IDType = req(input$skjemarad),
                        frlType = req(as.numeric(input$forl)))
      )
     } else if (req(input$tab) == "skjema") {
      as.data.frame.matrix(
        hisregSkjemaTabell(SkjemaOversikt,
                          tidFra = min(req(input$dato)),
                          tidTil = max(req(input$dato)),
                          status = req(input$status),
                          typeDato = req(input$typeDato))
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
     write.csv2(tab, file, row.names = F)
    }
  )
  output$lastNedTabell2 <- downloadHandler(
    filename = function() {
      if (req(input$status) == "1") {
        paste0("ferdistilteskjema", Sys.time(), ".csv")
      } else {
        paste0("ikkeferdigeskjema", Sys.time(), ".csv")
      }
    },
    content = function(file) {
      tab <- tabellData()
      write.csv2(tab, file, row.names = F)
    }
  )
  observe({
    cont <- headerFooter(tabellData())
    subS <- dim(tabellData())[1] - 1

    output$Tabell1 <-  renderDT(
      as.data.frame.matrix(tabellData()) [1:subS, ] %>%
        DT::datatable(
          container = cont,
          selection = "none",
          extensions = "FixedHeader",
          options = list(pageLength = 50,
                         fixedHeader = TRUE,
                         lengthChange = FALSE,
                         dom = "t"))
    )
   })
  observe({
    cont <- headerFooter(tabellData())
    subS <- dim(tabellData())[1] - 1

    output$Tabell2 <-  renderDT(
      as.data.frame.matrix(tabellData())[1:subS, ] %>%
        DT::datatable(
          container = cont,
          selection = "none",
          extensions = "FixedHeader",
          options = list(pageLength = 50,
                         fixedHeader = TRUE,
                         lengthChange = FALSE,
                         dom = "t"))
    )
  })

}
