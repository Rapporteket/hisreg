#hjelpefunksjon som lager topp og bunntekst til tabellen
cont <- function(enh, usRole) {
  if (usRole == "SC") {
    avd = "Valgt avdeling"
  } else {
    avd = "Din avdeling"
  }
  if (enh == 1) {
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Kategori"),
          th(class = "dt-center", colspan = 3, avd),
          th(class = "dt-center", colspan = 3, "Landet forøveig")
        ),
        tr(
          lapply(rep(c("N", "Antall", "Andel"), 2), th)
        )
      )
    ))
  } else{
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          lapply(c("Kategori", "N", "Antall", "Andel"), th)
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
      div(id = ns("sbPanel"),
      shiny::selectInput(ns("varSel"), label = "Velg variabel",
                         choices = varValg, selected = varValg[[1]]),
      shiny::selectInput(ns("typInt"), label = "Type intervensjon",
                         choices = typInt, selected = 99),
      shiny::uiOutput(ns("SC")),
      shiny::selectInput(ns("enhSel"), label = "Velg enhe(er)",
                         choices = enhvalg, selected = enhvalg[[1]]),
      shiny::dateRangeInput(ns("dateRan"), label = "Tidsperiode",
                            language = "no", separator = "til",
                            min = "2011-01-01", max = Sys.Date(),
                            start = "2011-01-01", end = Sys.Date()),
      shiny::selectInput(ns("kjoSle"), label = "Kjønn", choices = kjoenn,
                         selected = 99),
      shiny::sliderInput(ns("aldSli"), label = "Alder", min = 0,
                         max = 130, value = c(0, 130)),
      shiny::uiOutput(ns("figfil"))),
      shiny::actionLink(inputId=ns("nullstill"),
                        style="color:black" ,
                        label = "Nullstill Valg")
    ),
    shiny::mainPanel(width = 9,
      shiny::fluidRow(
        shiny::column(6,
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

modFordelinger <- function(input, output, session, rID, role, ss) {

  output$figfil <- shiny::renderUI({
    ns <- session$ns

    if (input$tabs == "fig") {
      shiny::selectInput(ns("outfil"), label = "Velg bildeformat",
        choices = c("pdf", "png", "jpg", "bmp", "tif", "svg"))
    }
  })
  output$SC <- renderUI({
    ns <- session$ns
    if (role == "SC") {
      shiny::selectInput(ns("shSelect"), label = "Velg Avdeling",
                         choices = avdValg, selected = rID )
    }
  })

  resh <- reactive(
    if (role == "SC"){
      req(input$shSelect)
    } else {
      rID
    }
  )

  observeEvent(req(input$nullstill), {shinyjs::reset("sbPanel")})

  data <- reactive({
    hisreg::hisregFigAndeler(RegData = RegData,
                             valgtVar = input$varSel,
                             datoFra = min(input$dateRan),
                             datoTil = max(input$dateRan),
                             minald = input$aldSli[1],
                             maxald = input$aldSli[2],
                             reshID = resh(),
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

  output$figur <- renderPlot(
    hisreg::hisregFigAndeler(RegData = RegData, valgtVar = input$varSel,
                             datoFra = min(input$dateRan),
                             datoTil = max(input$dateRan),
                             minald = input$aldSli[1],
                             maxald = input$aldSli[2],
                             reshID = resh(), enhetsUtvalg = input$enhSel,
                             forlop1 = input$typInt,
                             erMann = as.numeric(input$kjoSle))
  )

  #tabell
  observe({
    cont <- cont(input$enhSel, role)
    output$tabell <- DT::renderDT(
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
      paste0(input$varSel, Sys.time(), ".csv")
    },
    content = function(file) {
      tab <- df()
      write.csv2(tab, file, row.names = F)
    }
  )

  output$lastNedBilde <- downloadHandler(

    filename = function(){
      paste0(input$varSel, Sys.time(), '.', input$outfil)
    },

    content = function(file){
      hisreg::hisregFigAndeler(RegData = RegData, valgtVar = input$varSel,
                               datoFra = min(input$dateRan),
                               datoTil = max(input$dateRan),
                               minald = input$aldSli[1],
                               maxald = input$aldSli[2],
                               reshID = resh(), enhetsUtvalg = input$enhSel,
                               forlop1 = input$typInt,
                               outfile = file,
                               erMann = as.numeric(input$kjoSle))
     }
  )
  #logging
  shiny::observe({
    if (onServer) {
      if (input$tabs == "fig") {
        mld <- paste(
          "Hisreg: figur - fordeling. variabel -",
          input$varSel
        )
      } else if (input$tabs == "tab") {
        mld <- paste(
          "Hisreg: tabell - fordeling. variabel -",
          input$varSel
        )
      }
      rapbase::repLogger(
        session = ss,
        msg = mld
      )
      mldNLF <- paste(
        "Hisreg: nedlasting figur - fordeling. variabel",
        input$varSel
      )
      mldNLT <- paste(
        "Hisreg: nedlasting tabell - fordeling. variabel",
        input$varSel
      )
      shinyjs::onclick(
        "lastNedBilde",
        rapbase::repLogger(
          session = ss,
          msg = mldNLF
        )
      )
      shinyjs::onclick(
        "lastNed",
        rapbase::repLogger(
          ss,
          msg = mldNLT
        )
      )
    }
  })


}
