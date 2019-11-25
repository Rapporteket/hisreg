contGjen <- function(fun) {
  if (fun == "FEPS") {
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Sykehus"),
          th(rowspan = 2, "N"),
          th(class = "dt-center", colspan = 2, "Preintervensjon"),
          th(class = "dt-center", colspan = 2, "Postintervensjon")
        ),
        tr(
          lapply(rep(c("Gjennomsnitt", "95% Konfidensintervall"), 2), th)
        )
      )
    ))
  } else{
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Intervensjonstype"),
          th(rowspan = 2, "N"),
          th(class = "dt-center", colspan = 2, "Preintervensjon"),
          th(class = "dt-center", colspan = 2, "Postintervensjon")
        ),
        tr(
          lapply(rep(c("Gjennomsnitt", "95% KI"), 2), th)
        )
      )
    ))
  }
}


modGjennomsnittUI <- function(id, varValg = varValgGjenPer) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::selectInput(ns("varSel"), label = "Velg variabel",
                         choices = varValg, selected = varValg[[1]]),
      shiny::uiOutput(ns("intrv")),
      shiny::uiOutput(ns("enh")),
      shiny::dateRangeInput(ns("dateRan"),
                            label = "Tidsperiode",
                            min = "2011-01-01",
                            max = Sys.Date(),
                          start = "2011-01-01",
                          end = Sys.Date()),
      shiny::selectInput(ns("kjoSle"),
                         label = "Kjønn",
                         choices = kjoenn,
                         selected = 99),
      shiny::sliderInput(ns("aldSli"),
                         label = "Alder",
                         min = 0,
                         max = 130,
                         value = c(0, 130)),
      shiny::uiOutput(ns("figfil"))
    ),
    shiny::mainPanel(width = 9,
      fluidPage(
        column(width = 6,
          shiny::tabsetPanel(id = ns("tabs"),
            shiny::tabPanel("Figur", value = "fig",
              shiny::plotOutput(ns("figur"),
                                height = "600px"),
              downloadButton(ns("lastNedBilde"),
                           "Last ned bilde")),
            shiny::tabPanel("Tabell",
                            value = "tab",
              shiny::h3(shiny::textOutput(ns("text")),
                        style = "text-align:center"),
              DT::DTOutput(ns("tabell")),
              downloadButton(ns("lastNedTab"),
                             "Last ned tabell")
            )
          )
        )
      )
    )#mainPanel
  )#sidebarlayout
}


modGjennomsnitt <- function(input, output, session, rID = 601031,
                        add_int = T, add_enh = T, fun = "PS", ss) {

  output$figfil <- shiny::renderUI({
    ns <- session$ns

    if (input$tabs == "fig") {
      shiny::selectInput(ns("outfil"), label = "Velg bildeformat",
                         choices = c("pdf", "png", "jpg", "bmp", "tif", "svg"))
    }
  })

  output$enh <- shiny::renderUI({
    ns <- session$ns
    if (add_enh) {
      shiny::selectInput(ns("enhSel"), label = "Velg enhet",
                         choices = c("Hele landet" = 0,
                                     "Egen avdeling" = 2),
                         selected = 0)
    }
  })

  output$intrv <- shiny::renderUI({
    ns <- session$ns
    if (add_int) {
      shiny::selectInput(ns("typInt"),
                         label = "Type intervensjon",
                         choices = typInt, selected = 99)
    }
  })

  #figurer, PS - per sykehus,PI - per intervensjonstype,
  #FEPI - før-etter per intervensjonstype
  #FEPS - før og etter, per sykehus
  shiny::observe({
    if (fun == "PS") {
      output$figur <- shiny::renderPlot({
        hisreg::hisregFigGjsnGrVar(RegData,
                                   valgtVar = req(input$varSel),
                                   datoFra = req(input$dateRan[1]),
                                   datoTil = req(input$dateRan[2]),
                                   reshID = rID,
                                   erMann = req(as.numeric(input$kjoSle)),
                                   minald = req(input$aldSli[1]),
                                   maxald = req(input$aldSli[2]),
                                   forlop1 = req(as.numeric(input$typInt)))
      })


    } else if (fun == "PI") {
      output$figur <- shiny::renderPlot({
        hisreg::hisregFigGjsnGrVar(RegData,
                                   valgtVar = req(input$varSel),
                                   datoFra = req(input$dateRan[1]),
                                   datoTil = req(input$dateRan[2]),
                                   reshID = rID,
                                   gr_var = "Intervensjon",
                                   erMann = as.numeric(req(input$kjoSle)),
                                   minald = req(input$aldSli[1]),
                                   maxald = req(input$aldSli[2]))
      })

    }else if (fun == "FEPI") {
      output$figur <- shiny::renderPlot({
        hisreg::hisregFigGjsnPrePostGrVar(RegData,
                                valgtVar = req(input$varSel),
                                datoFra = req(input$dateRan[1]),
                                datoTil = req(input$dateRan[2]),
                                reshID = rID,
                                gr_var = "Intervensjon",
                                erMann = as.numeric(req(input$kjoSle)),
                                minald = req(input$aldSli[1]),
                                maxald = req(input$aldSli[2]),
                                enhetsUtvalg =  as.numeric(req(input$enhSel)))
      })

    }else if (fun == "FEPS") {
      output$figur <- shiny::renderPlot({
        hisreg::hisregFigGjsnPrePostShus(RegData,
                                       valgtVar = req(input$varSel),
                                       datoFra = req(input$dateRan[1]),
                                       datoTil = req(input$dateRan[2]),
                                       reshID = rID,
                                       forlop1 = as.numeric(req(input$typInt)),
                                       gr_var = "SykehusNavn",
                                       erMann = as.numeric(req(input$kjoSle)),
                                       minald = req(input$aldSli[1]),
                                       maxald = req(input$aldSli[2]))
      })

    } else {
      output$figur <- NULL
    }


   })

  data <- shiny::reactive({
    if (fun == "PS") {
      hisreg::hisregFigGjsnGrVar(RegData,
                                 valgtVar = req(input$varSel),
                                 datoFra = req(input$dateRan[1]),
                                 datoTil = req(input$dateRan[2]),
                                 reshID = rID,
                                 erMann = req(as.numeric(input$kjoSle)),
                                 minald = req(input$aldSli[1]),
                                 maxald = req(input$aldSli[2]),
                                 forlop1 = req(as.numeric(input$typInt)))
    } else if (fun == "PI") {
      hisreg::hisregFigGjsnGrVar(RegData,
                               valgtVar = req(input$varSel),
                               datoFra = req(input$dateRan[1]),
                               datoTil = req(input$dateRan[2]),
                               reshID = rID,
                               gr_var = "Intervensjon",
                               erMann = as.numeric(req(input$kjoSle)),
                               minald = req(input$aldSli[1]),
                               maxald = req(input$aldSli[2]))
    }else if (fun == "FEPI") {
      hisreg::hisregFigGjsnPrePostGrVar(RegData,
                                valgtVar = req(input$varSel),
                                datoFra = req(input$dateRan[1]),
                                datoTil = req(input$dateRan[2]),
                                reshID = rID,
                                gr_var = "Intervensjon",
                                erMann = as.numeric(req(input$kjoSle)),
                                minald = req(input$aldSli[1]),
                                maxald = req(input$aldSli[2]),
                                enhetsUtvalg = as.numeric(req(input$enhSel)))
    }else if (fun == "FEPS") {
      hisreg::hisregFigGjsnPrePostShus(RegData,
                                       valgtVar = req(input$varSel),
                                       datoFra = req(input$dateRan[1]),
                                       datoTil = req(input$dateRan[2]),
                                       reshID = rID,
                                       forlop1 = as.numeric(req(input$typInt)),
                                       gr_var = "SykehusNavn",
                                       erMann = as.numeric(req(input$kjoSle)),
                                       minald = req(input$aldSli[1]),
                                       maxald = req(input$aldSli[2]))

    } else {
      output$figur <- NULL
    }
  })

  df <- shiny::reactive({
    if (fun == "PS") {
      data.frame("sykehus" = names(data()$Antall),
                 "N" = as.data.frame(data()$Antall)[, 2],
                 "Gjennomsnitt" = round(data()$Andeler,
                                        digit = 1),
                 "konfint" = paste0("(", round(data()$CIN,
                                              digit = 1),
                                    " , ", round(data()$CIO,
                                                 digit = 1), ")"))
    }else if (fun == "PI") {
      data.frame("Intervensjonstype" = names(data()$Antall),
                 "N" = as.data.frame(data()$Antall)[, 2],
                 "Gjennomsnitt" = round(data()$Andeler, digit = 1),
                 "konfint" = paste0("(", round(data()$CIN, digit = 1),
                                    " , ", round(data()$CIO, digit = 1), ")"))
    }else if (fun == "FEPI") {
      data.frame("Intervensjonstype" = data()$Grtxt1,
                 "N" = data()$Grtxt2,
                 "Før" = round(as.data.frame(t(data()$Andeler))[[1]],
                               digits = 2),
                 "fkonfint" = paste0("(",
                                     round(as.data.frame(t(data()$CIN))[[1]],
                                           digits = 1), " , ",
                                     round(as.data.frame(t(data()$CIO))[[1]],
                                           digits = 1), ")"),
                 "Etter" = round(as.data.frame(t(data()$Andeler))[[2]],
                                 digits = 2),
                 "ekonfint" = paste0("(",
                                     round(as.data.frame(t(data()$CIN))[[2]],
                                           digits = 1), " , ",
                                     round(as.data.frame(t(data()$CIO))[[2]],
                                           digits = 1), ")")
                 )
    }else if (fun == "FEPS") {
      data.frame("Sykehus" = data()$Grtxt1,
                 "N" = data()$Grtxt2,
                 "Før" = round(as.data.frame(t(data()$Andeler))[[1]],
                               digits = 2),
                 "fkonfint" = paste0("(",
                                     round(as.data.frame(t(data()$CIN))[[1]],
                                           digits = 1), " , ",
                                     round(as.data.frame(t(data()$CIO))[[1]],
                                           digits = 1), ")"),
                 "Etter" = round(as.data.frame(t(data()$Andeler))[[2]],
                                 digits = 2),
                 "ekonfint" = paste0("(",
                                     round(as.data.frame(t(data()$CIN))[[2]],
                                               digits = 1), " , ",
                                     round(as.data.frame(t(data()$CIO))[[2]],
                                           digits = 1), ")")
                 )
    }
  })

  output$text <- renderText({
    data()$tittel
  })
  observe({
    container <- contGjen(fun)
    output$tabell <- DT::renderDT(
      if (fun == "PI" | fun == "PS") {
        df() %>%
        dplyr::filter(N > 10) %>%
        DT::datatable(selection = "none",
                      rownames = FALSE,
                      colnames = c("95% Konfidensintervall" = "konfint"),
                      options = list(dom = "t"))
      }else{
        df() %>%
         dplyr::filter(N >= 5) %>%
          DT::datatable(selection = "none",
                       rownames = FALSE,
                       options = list(dom = "t"),
                       container = container)
      }
    )
  })
  output$lastNedTab <- downloadHandler(
    filename = function() {
      paste0(input$varSel, fun, Sys.time(), ".csv")
    },
    content = function(file) {
      dataf <- df()
      write.csv2(dataf, file, row.names = F)
    }
  )
  output$lastNedBilde <- downloadHandler(
    filename = function() {
      paste0(input$varSel, Sys.time(), ".", input$outfil)
    },
    content = function(file){
      if (fun == "PS") {
        hisreg::hisregFigGjsnGrVar(RegData,
                                   valgtVar = req(input$varSel),
                                   datoFra = req(input$dateRan[1]),
                                   datoTil = req(input$dateRan[2]),
                                   reshID = rID,
                                   erMann = req(as.numeric(input$kjoSle)),
                                   minald = req(input$aldSli[1]),
                                   maxald = req(input$aldSli[2]),
                                   forlop1 = req(as.numeric(input$typInt)),
                                   outfile = file)
      } else if (fun == "PI") {
          hisreg::hisregFigGjsnGrVar(RegData,
                                     valgtVar = req(input$varSel),
                                     datoFra = req(input$dateRan[1]),
                                     datoTil = req(input$dateRan[2]),
                                     reshID = rID,
                                     gr_var = "Intervensjon",
                                     erMann = as.numeric(req(input$kjoSle)),
                                     minald = req(input$aldSli[1]),
                                     maxald = req(input$aldSli[2]),
                                     outfile = file)
      }else if (fun == "FEPI") {
           hisreg::hisregFigGjsnPrePostGrVar(RegData,
                                  valgtVar = req(input$varSel),
                                  datoFra = req(input$dateRan[1]),
                                  datoTil = req(input$dateRan[2]),
                                  reshID = rID,
                                  gr_var = "Intervensjon",
                                  erMann = as.numeric(req(input$kjoSle)),
                                  minald = req(input$aldSli[1]),
                                  maxald = req(input$aldSli[2]),
                                  enhetsUtvalg =  as.numeric(req(input$enhSel),
                                  outfile = file))
      }else if (fun == "FEPS") {
          hisreg::hisregFigGjsnPrePostShus(RegData,
                                 valgtVar = req(input$varSel),
                                 datoFra = req(input$dateRan[1]),
                                 datoTil = req(input$dateRan[2]),
                                 reshID = rID,
                                 forlop1 = as.numeric(req(input$typInt)),
                                 gr_var = "SykehusNavn",
                                 erMann = as.numeric(req(input$kjoSle)),
                                 minald = req(input$aldSli[1]),
                                 maxald = req(input$aldSli[2]),
                                 outfile = file)
      }

    }
  )
  shiny::observe({
    if (onServer) {
      if (input$tabs == "fig") {
        mld <- paste(
          "Hisreg: figur - gjennomsnitt.", fun, " variabel -",
          input$varSel
        )
      } else if (input$tabs == "tab") {
        mld <- paste(
          "Hisreg: tabell - gjennomsnitt.", fun, " variabel -",
          input$varSel
        )
      }
      raplog::repLogger(
        session = ss,
        msg = mld
      )
      mldNLF <- paste(
        "Hisreg: nedlasting figur - gjennomsnitt.", fun," variabel",
        input$varSel
      )
      mldNLT <- paste(
        "Hisreg: nedlasting tabell - gjennomsnitt.", fun, " variabel",
        input$varSel
      )
      shinyjs::onclick(
        "lastNedBilde",
        raplog::repLogger(
          session = ss,
          msg = mldNLF
        )
      )
      shinyjs::onclick(
        "lastNedTab",
        raplog::repLogger(
          ss,
          msg = mldNLT
        )
      )
    }
  })

}
