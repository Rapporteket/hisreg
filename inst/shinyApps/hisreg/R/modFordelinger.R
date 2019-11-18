#hjelpefunksjon som lager topp og bunntekst til tabellen
cont <- function(enh) {
  if (enh == 1) {
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Kategori"),
          th(class = "dt-center", colspan = 3, "Din avdeling"),
          th(class = "dt-center", colspan = 3, "Landet forøveig")
        ),
        tr(
          lapply(rep(c("Antall", "N", "Andel"), 2), th)
        )
      )
    ))
  } else{
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          lapply(c("Kategori", "Antall", "N", "Andel"), th)
        )
      )
    ))
  }
}

#modul UI
modFordelingerUI <- function(id, varValg = varValgFordeling) {
  ns <- shiny::NS(id)
  enhvalg <- c("Egen avd. mot landet forøvrig" = 1,
               "Hele landet" = 0, "Egen avdeling" = 2)

  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::selectInput(ns("varSel"), label = "Velg variabel",
                         choices = varValg, selected = varValg[[1]]),
      shiny::selectInput(ns("typInt"), label = "Type intervensjon",
                         choices = typInt, selected = 99),
      shiny::selectInput(ns("enhSel"), label = "Velg enhe(er)",
                         choices = enhvalg, selected = enhvalg[[1]]),
      shiny::dateRangeInput(ns("dateRan"), label = "Tidsperiode",
                            min = "2011-01-01", max = Sys.Date(),
                            start = "2011-01-01", end = Sys.Date()),
      shiny::selectInput(ns("kjoSle"), label = "Kjønn", choices = kjoenn,
                         selected = 99),
      shiny::sliderInput(ns("aldSli"), label = "Alder", min = 0,
                         max = 130, value = c(0, 130)),
      shiny::uiOutput(ns("figfil"))
    ),
    shiny::mainPanel(width = 9,
      shiny::fluidRow(
        shiny::column(8,
          tabsetPanel(id = ns("tabs"),
            tabPanel("Figur", value = "fig",
              shiny::plotOutput(ns("figur"),
                                width = "95%",
                                height = "600px"),
              downloadButton(ns("lastNedBilde"),
                             "Last ned bilde")),
            tabPanel("Tabell", value = "tab",
                     DT::DTOutput(ns("tabell")),
                     downloadButton(ns("lastNed"),
                                    "Last ned tabell")
                     )))
      )
    )#mainpanel
  )#sidebarLayout
}

#moduleserver

modFordelinger <- function(input, output, session, rID) {

  output$figfil <- shiny::renderUI({
    ns <- session$ns

    if (input$tabs == "fig") {
      shiny::selectInput(ns("outfil"), label = "Velg bildeformat",
        choices = c("pdf", "png", "jpg", "bmp", "tif", "svg"))
    }
  })

  data <- reactive({
    hisreg::hisregFigAndeler(RegData = RegData,
                             valgtVar = input$varSel,
                             datoFra = input$dateRan[1],
                             datoTil = input$dateRan[2],
                             minald = input$aldSli[1],
                             maxald = input$aldSli[2],
                             reshID = rID,
                             enhetsUtvalg = input$enhSel,
                             forlop1 = input$typInt,
                             erMann = as.numeric(input$kjoSle))
  })

  #data til en dataramme
  df <- shiny::reactive({
  if (is.table(data()$Andeler$Hoved)) {
    if (input$enhSel == 1) {
       data.frame("Kategori" = as.data.frame(data()$Andeler$Hoved)[, 1],
                  "Antall." = rep(data()$Antall$Hoved,
                                  length(data()$Andeler$Hoved)),
                  "N." = round((as.data.frame(data()$Andeler$Hoved)[, 2] /
                                  100) * data()$Antall$Hoved),
                  "Andel." = round(as.data.frame(data()$Andeler$Hoved)[, 2] /
                                     100, digits = 3),
                  "Antall" = rep(data()$Antall$Rest,
                                 length(data()$Andeler$Hoved)),
                  "N" = round((as.data.frame(data()$Andeler$Rest)[, 2] / 100) *
                                data()$Antall$Rest),
                  "Andel" = round(as.data.frame(data()$Andeler$Rest)[, 2] /
                                    100, digits = 3))
    }  else {
      data.frame("Kategori" = as.data.frame(data()$Andeler$Hoved)[, 1],
                 "Antall." = rep(data()$Antall$Hoved,
                                 length(data()$Andeler$Hoved)),
                 "N." = round((as.data.frame(data()$Andeler$Hoved)[, 2] / 100) *
                                 data()$Antall$Hoved),
                 "Andel." = round(as.data.frame(data()$Andeler$Hoved)[, 2] / 100
                                  , digits = 3))
      }

  } else{
    if (input$enhSel == 1) {
      data.frame("Kategori" = data()$Grtxt,
                 "Antall." = rep(data()$Antall$Hoved,
                                 length(data()$Andeler$Hoved)),
                 "N." =  round((data()$Andeler$Hoved / 100) *
                                  data()$Antall$Hoved),
                 "Andel." = round(data()$Andeler$Hoved / 100,
                                   digits = 3),
                 "Antal" = rep(data()$Antall$Rest,
                                length(data()$Andeler$Hoved)),
                 "N" =  round((data()$Andeler$Rest / 100) *
                                data()$Antall$Rest),
                 "Andel" = round(data()$Andeler$Rest / 100,
                                  digits = 3))
    } else {
      data.frame("Kategori" = data()$Grtxt,
                 "Antall." = rep(data()$Antall$Hoved,
                                 length(data()$Andeler$Hoved)),
                 "N." =  round((data()$Andeler$Hoved / 100) *
                                 data()$Antall$Hoved),
                 "Andel." = round(data()$Andeler$Hoved /
                                    100, digits = 3))
    }
  }

  })

  #andelsfigurer
  output$figur <- renderPlot({
    hisreg::hisregFigAndeler(RegData = RegData, valgtVar = input$varSel,
                             datoFra = input$dateRan[1],
                             datoTil = input$dateRan[2],
                             minald = input$aldSli[1],
                             maxald = input$aldSli[2],
                             reshID = rID, enhetsUtvalg = input$enhSel,
                             forlop1 = input$typInt,
                             erMann = as.numeric(input$kjoSle))
    if (onServer) {
      # msgFigAndVis <- paste(
      #   "Hisreg: fordelingsfigur, viser andeler av ",
      #   input$varSel
      #)
      raplog::repLogger(
        session,
        msg = "msgFigAndVis"
      )
    }
  })

  #tabell
  observe({
    cont <- cont(input$enhSel)
    output$tabell <- DT::renderDT(
      # if (onServer) {
      #   msgTabAndVis <- paste(
      #     "Hisreg: fordelingstabell, viser andeler av ",
      #     input$varSel
      #   )
      #   raplog::repLogger(
      #     session,
      #     msg = msgTabAndVis
      #   )
      # }
      if (input$enhSel == 1) {
        df() %>% datatable(selection = "none",
                           container = cont, rownames = FALSE,
                          options = list(dom = "t")) %>%
                          DT::formatPercentage(c("Andel", "Andel."), digits = 1)
      } else{
        df() %>% datatable(selection = "none",
                           container = cont, rownames = FALSE,
                          options = list(dom = "t")) %>%
          DT::formatPercentage("Andel.", digits = 1)
      }
    )
  })
  output$lastNed <- downloadHandler(
    filename = function() {
      if (onServer) {
        msgFigAndNed <- paste(
          "Hisreg: nedlasting av fordelingsfigur, viser andeler av ",
          input$varSel
        )
        raplog::repLogger(
          session,
          msg = msgFigAndNed
        )
      }
      paste0(input$varSel, Sys.time(), ".csv")
    },
    content = function(file) {
      tab <- df()
      write.csv2(tab, file, row.names = F)
    }
  )
  output$lastNedBilde <- downloadHandler(
    filename = function(){
      if (onServer) {
        msgTabAndNed <- paste(
          "Hisreg: nedlasting av fordelingstabell, viser andeler av ",
          input$varSel
        )
        raplog::repLogger(
          session,
          msg = msgTabAndNed
        )
      }
      paste0(input$varSel, Sys.time(), '.', input$outfil)
    },

    content = function(file){
      hisreg::hisregFigAndeler(RegData = RegData, valgtVar = input$varSel,
                               datoFra = input$dateRan[1],
                               datoTil = input$dateRan[2],
                               minald = input$aldSli[1],
                               maxald = input$aldSli[2],
                               reshID = rID, enhetsUtvalg = input$enhSel,
                               forlop1 = input$typInt,
                               outfile = file,
                               erMann = as.numeric(input$kjoSle))
     }
  )



}
