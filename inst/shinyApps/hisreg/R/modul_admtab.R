# Modul for Administrative tabeller-fane i Hisreg sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @return Modulfunksjoner til Administrative tabeller


admtab_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
                 id = ns("id_adm_panel"),
                 dateRangeInput(inputId=ns("datovalg_adm"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                                max = Sys.Date(), start  = Sys.Date() %m-% months(12), end = Sys.Date(), separator = " til "),
                 shinyjs::hidden(selectInput(inputId = ns("adm_tidsenhet"), label = "Velg tidsenhet",
                                             choices = c('Måneder'=1, 'År'=2))),
                 shiny::uiOutput(ns("tab_mnd")),
                 shiny::uiOutput(ns("tab_aar")),
                 selectInput(inputId = ns("regstatus"), label = "Skjemastatus", choices = c('Ferdigstilt'=1, 'I kladd'=0)),
                 shinyjs::hidden(selectInput(inputId = ns("regstatus_tid"), label = "Skjemastatus",
                             choices = c('Ferdige forløp'=1, 'Ferdig basisreg. oppfølging ikke ferdigstilt'=2,
                                         'Basisreg. i kladd'=3))),
                 selectInput(inputId = ns("forlopstype"), label = "Forløpstype",
                             choices = c('Kirurgisk'=1, 'Medisinsk'=2, 'Kirurgisk og medisinsk'=3,
                                         'Ingen intervensjon bestemt av lege'=4, 'Ingen intervensjon bestemt av pasient'=5),
                             selected = 1:3, multiple = T),
                 tags$hr(),
                 actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(id= ns("admtabeller"),
                          tabPanel("Antall skjema", value = "id_ant_skjema",
                                   h2('Innregistreringer i Hisreg etter skjematype', align='center'),
                                   br(),
                                   br(),
                                   DTOutput(ns("Tabell_adm1")), downloadButton(ns("lastNed_adm1"), "Last ned tabell")
                          ),
                          tabPanel("Registreringer over tid", value = "id_ant_tid",
                                   h2('Avdelingsvis oversikt over forløp i Hisreg over tid', align='center'),
                                   br(),
                                   h4("Alle datofiltreringer gjøres på intervensjonsdato"),
                                   br(),
                                   br(),
                                   DTOutput(ns("Tabell_adm2")), downloadButton(ns("lastNed_adm2"), "Last ned tabell")
                          )
    )
    )
  )
}


admtab <- function(input, output, session, skjemaoversikt){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_adm_panel")
  })

  observe(
    if (input$admtabeller == "id_ant_skjema") {
      shinyjs::hide(id = 'adm_tidsenhet')
      shinyjs::hide(id = 'tab_mnd')
      shinyjs::hide(id = 'tab_aar')
      shinyjs::hide(id = 'regstatus_tid')
      shinyjs::show(id = 'datovalg_adm')
      shinyjs::show(id = 'regstatus')
    } else if (input$admtabeller == "id_ant_tid") {
      shinyjs::hide(id = 'datovalg_adm')
      shinyjs::hide(id = 'regstatus')
      shinyjs::show(id = 'adm_tidsenhet')
      shinyjs::show(id = 'tab_mnd')
      shinyjs::show(id = 'tab_aar')
      shinyjs::show(id = 'regstatus_tid')
    }
  )

  output$tab_mnd <- shiny::renderUI({
    ns <- session$ns
    req(input$adm_tidsenhet == '1')
    tagList(
      shinyWidgets::airDatepickerInput(inputId=ns("datovalg_adm_tid_mnd"), label = "Vis til og med måned: ", minDate = '2014-01-01',
                                       maxDate = Sys.Date(), value = Sys.Date(), view = "months", minView = 'months',
                                       dateFormat = "MM yyyy", language="da"),
      sliderInput(inputId=ns("ant_mnd"), label = "Antall måneder", min = 1, max = 24, value = 12, step = 1)
    )
  })

  output$tab_aar <- shiny::renderUI({
    ns <- session$ns
    req(input$adm_tidsenhet == '2')
    tagList(
      shinyWidgets::airDatepickerInput(inputId=ns("datovalg_adm_tid_aar"), label = "Vis til og med år: ", minDate = '2014-01-01',
                                       maxDate = Sys.Date(), value = Sys.Date(), view = "years", minView = 'years',
                                       dateFormat = "yyyy", language="da"),
      sliderInput(inputId= ns("ant_aar"), label = "Antall år", min = 1, max = 10, value = 5, step = 1)
    )
  })




  antskjema <- function() {

    if (!is.null(input$forlopstype)) {
      skjemaoversikt <- skjemaoversikt[skjemaoversikt$ForlopsType1Num %in% as.numeric(input$forlopstype), ]
    }
    ant_skjema <- skjemaoversikt[ , c("Sykehusnavn", "Skjemanavn", "HovedDato", "SkjemaStatus", "ForlopsType1Num")] %>%
      filter(HovedDato >= input$datovalg_adm[1] & HovedDato <= input$datovalg_adm[2]) %>%
      filter(SkjemaStatus == input$regstatus) %>%
      select("Sykehusnavn", "Skjemanavn") %>%
      table() %>%
      addmargins(1) %>%
      as.data.frame.matrix() %>%
      as_tibble(rownames = "Sykehusnavn")
    ant_skjema <- ant_skjema[, c("Sykehusnavn", "Registrering", "Preinterv pasient", "Preinterv lege", "Intervensjon",
                                 "Kontroll pas 3mnd", "Kontroll lege 3mnd", "Kontroll pas 6mnd", "Kontroll lege 6mnd")]

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)
  }

  output$Tabell_adm1 = renderDT(
    datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
              container = antskjema()$sketch,
              rownames = F,
              options = list(pageLength = 40)
    )
  )


  output$lastNed_adm1 <- downloadHandler(
    filename = function(){
      paste0('Regoversikt', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- antskjema()$ant_skjema
      write.csv2(TabellData, file, row.names = F, fileEncoding = "Latin1")
    }
  )




andre_adm_tab <- function() {

  skjemaoversikt$SkjemaStatus <- as.numeric(skjemaoversikt$SkjemaStatus)
  skjemaoversikt_forlop <-
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Registrering",
                         c("ForlopsID", "HovedDato", "Sykehusnavn", "AvdRESH", "SkjemaStatus", "ForlopsType1", "ForlopsType1Num")],
          skjemaoversikt[skjemaoversikt$Skjemanavn == "Preinterv pasient",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_prepas"), all = T) %>%
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Preinterv lege",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_predok"), all = T) %>%
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Intervensjon",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_interv"), all = T) %>%
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Kontroll pas 3mnd",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_kontrpas3"), all = T) %>%
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Kontroll lege 3mnd",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_kontrdok3"), all = T) %>%
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Kontroll pas 6mnd",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_kontrpas6"), all = T) %>%
    merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "Kontroll lege 6mnd",
                         c("SkjemaStatus", "ForlopsID")],
          by = "ForlopsID", suffixes = c("", "_kontrdok6"), all = T) %>%
    merge(intervention[, c("MCEID" , "TYPE_INTERVENTION")], by.x = "ForlopsID", by.y = "MCEID", all.x = T)

  if (!is.null(input$forlopstype)) {
    skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$ForlopsType1Num %in% as.numeric(input$forlopstype), ]
  }

  skjemaoversikt_forlop$statusbasis <- 0
  skjemaoversikt_forlop$statusbasis[rowSums(skjemaoversikt_forlop[, c("SkjemaStatus", "SkjemaStatus_prepas", "SkjemaStatus_predok", "SkjemaStatus_interv")])==4] <- 1
  skjemaoversikt_forlop$statusoppf3 <- 0
  skjemaoversikt_forlop$statusoppf3[rowSums(skjemaoversikt_forlop[, c("SkjemaStatus_kontrpas3", "SkjemaStatus_kontrdok3")], na.rm = T)==2] <- 1
  skjemaoversikt_forlop$statusoppf6 <- 0
  skjemaoversikt_forlop$statusoppf6[rowSums(skjemaoversikt_forlop[, c("SkjemaStatus_kontrpas6", "SkjemaStatus_kontrdok6")], na.rm = T)==2] <- 1
  skjemaoversikt_forlop$statusoppf <- 0
  skjemaoversikt_forlop$statusoppf[which(skjemaoversikt_forlop$ForlopsType1Num == 1)] <-
    skjemaoversikt_forlop$statusoppf6[which(skjemaoversikt_forlop$ForlopsType1Num == 1)]
  skjemaoversikt_forlop$statusoppf[which(skjemaoversikt_forlop$ForlopsType1Num == 2)] <-
    skjemaoversikt_forlop$statusoppf3[which(skjemaoversikt_forlop$ForlopsType1Num == 2)]
  skjemaoversikt_forlop$statusoppf[which(skjemaoversikt_forlop$ForlopsType1Num == 3)] <-
    skjemaoversikt_forlop$statusoppf3[which(skjemaoversikt_forlop$ForlopsType1Num == 3)] +
    skjemaoversikt_forlop$statusoppf6[which(skjemaoversikt_forlop$ForlopsType1Num == 3)]
  skjemaoversikt_forlop$statusoppf[which(skjemaoversikt_forlop$statusoppf==2)] <- 1

  if (input$adm_tidsenhet == 1) {
    req(input$datovalg_adm_tid_mnd)

    tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
    fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)) %>% floor_date(unit="months")
    aux <- skjemaoversikt_forlop
    aux <- aux[aux$HovedDato >= fraDato & aux$HovedDato <= tilDato, ]

    aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))

    ant_skjema <- switch (input$regstatus_tid,
                          '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$statusbasis ==1 & aux$statusoppf==1) , c('Sykehusnavn', 'mnd')]))),
                          '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$statusbasis ==1 & aux$statusoppf==0) , c('Sykehusnavn', 'mnd')]))),
                          # '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$statusbasis ==1 & is.na(aux$statusoppf)) , c('Sykehusnavn', 'mnd')]))),
                          '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$statusbasis ==0) , c('Sykehusnavn', 'mnd')])))
    ) %>% as_tibble(rownames = 'Sykehusnavn')
  }

  if (input$adm_tidsenhet == 2) {
    req(input$datovalg_adm_tid_aar)
    tilDato <- as.Date(paste0(input$datovalg_adm_tid_aar))
    fraDato <- tilDato %m-% years(input$ant_aar) %>% floor_date(unit="years")
    aux <- skjemaoversikt_forlop
    aux <- aux[aux$HovedDato >= fraDato & aux$HovedDato <= tilDato, ]

    aux$mnd <- factor(format(aux$HovedDato, format='%Y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar), by="year"), "%Y"))

    ant_skjema <- switch (input$regstatus_tid,
                          '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                          '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                          # '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                          '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
    ) %>% as_tibble(rownames = 'Sykehusnavn')
  }

  sketch <- htmltools::withTags(table(
    tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
    tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
  list(ant_skjema=ant_skjema, sketch=sketch)

}

output$Tabell_adm2 = renderDT(
  datatable(andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
            container = andre_adm_tab()$sketch,
            rownames = F,
            options = list(pageLength = 40)
  )
)

output$lastNed_adm2 <- downloadHandler(
  filename = function(){
    paste0('Regoversikt_tid', Sys.time(), '.csv')
  },

  content = function(file){
    TabellData <- andre_adm_tab()$ant_skjema
    write.csv2(TabellData, file, row.names = F, fileEncoding = "Latin1")
  }
)


# shiny::observe({
#   if (rapbase::isRapContext()) {
#     if (req(input$admtabeller) == "id_ant_skjema") {
#       mld_adm1 <- paste0(
#         "NoRGast: Admin. tabell: Antall skjema, dato ",
#         input$datovalg_adm[1], ' til ', input$datovalg_adm[2])
#     }
#     if (req(input$admtabeller) == "id_ant_tid") {
#       mld_adm1 <- paste0(
#         "NoRGast: Admin. tabell: Antall skjema pr ",
#         c('måned', 'år')[as.numeric(input$adm_tidsenhet)], ". ",
#         c('Ferdige forløp', 'Oppfølging i kladd', 'Ferdig basisreg. oppfølging mangler',
#           'Basisreg. i kladd')[as.numeric(input$regstatus_tid)])
#     }
#     raplog::repLogger(
#       session = hvd_session,
#       msg = mld_adm1
#     )
#
#     shinyjs::onclick(
#       "lastNed_adm1",
#       raplog::repLogger(
#         session = hvd_session,
#         msg = paste0("NoRGast: nedlasting tabell: Antall skjema, dato ",
#                      input$datovalg_adm[1], ' til ', input$datovalg_adm[2])
#       )
#     )
#     shinyjs::onclick(
#       "lastNed_adm2",
#       raplog::repLogger(
#         session = hvd_session,
#         msg = paste0("NoRGast: nedlasting tabell: Antall skjema pr ",
#                      c('måned', 'år')[as.numeric(input$adm_tidsenhet)], ". ",
#                      c('Ferdige forløp', 'Oppfølging i kladd', 'Ferdig basisreg. oppfølging mangler',
#                        'Basisreg. i kladd')[as.numeric(input$regstatus_tid)])
#       )
#     )
#   }
# })





}




# shiny::sidebarLayout(
#   sidebarPanel(width = 3,
#     id = ns("id_adm_panel"),
#     conditionalPanel(condition = paste0("input['", ns('admtabeller'), "'] == 'id_ant_skjema'"),
#                      dateRangeInput(inputId=ns("datovalg_adm"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
#                                     max = Sys.Date(), start  = Sys.Date() %m-% months(12), end = Sys.Date(), separator = " til "),
#                      selectInput(inputId = ns("regstatus"), label = "Skjemastatus",
#                                  choices = c('Ferdigstilt'=1, 'I kladd'=0))#,
#     ),
#     conditionalPanel(condition = paste0("input['", ns("admtabeller"), "'] == 'id_ant_tid'"),
#                      selectInput(inputId = ns("adm_tidsenhet"), label = "Velg tidsenhet",
#                                  choices = c('Måneder'=1, 'År'=2)),
#                      conditionalPanel(condition = paste0("input['", ns("adm_tidsenhet"), "'] == '1'"),
#                                       norgast::dateInput2(inputId=ns("datovalg_adm_tid_mnd"), label = "Vis til og med måned: ", min = '2014-01-01',
#                                                           max = Sys.Date(), value = Sys.Date(), minview = 'months', format = "MM yyyy", language="no"),
#                                       sliderInput(inputId=ns("ant_mnd"), label = "Antall måneder", min = 1, max = 24, value = 12, step = 1)),
#                      conditionalPanel(condition = paste0("input['", ns("adm_tidsenhet"), "'] == '2'"),
#                                       norgast::dateInput2(inputId=ns("datovalg_adm_tid_aar"), label = "Vis til og med år: ", min = '2014-01-01',
#                                                           max = Sys.Date(), value = Sys.Date(), minview = 'years', format = "yyyy", language="no"),
#                                       sliderInput(inputId= ns("ant_aar"), label = "Antall år", min = 1, max = 10, value = 5, step = 1)),
#                      selectInput(inputId = ns("regstatus_tid"), label = "Skjemastatus",
#                                  choices = c('Ferdige forløp'=1, 'Oppfølging i kladd'=2, 'Ferdig basisreg. oppfølging mangler'=3,
#                                              'Basisreg. i kladd'=4))
#     ),
#     selectInput(inputId = ns("forlopstype"), label = "Forløpstype",
#                 choices = c('Kirurgisk'=1, 'Medisinsk'=2, 'Kirurgisk og medisinsk'=3,
#                             'Ingen intervensjon bestemt av lege'=4, 'Ingen intervensjon bestemt av pasient'=5),
#                 selected = 1:3, multiple = T),
#     tags$hr(),
#     actionButton(ns("reset_input"), "Nullstill valg")
#   ),
#   mainPanel(tabsetPanel(id= ns("admtabeller"),
#                         tabPanel("Antall skjema", value = "id_ant_skjema",
#                                  h4(tags$b(tags$u('Denne tabellen gir en avdelingsvis oversikt over innregistreringer i Hisreg:'))),
#                                  # h4(tags$b('Ferdige forløp '), 'viser antall forløp med alle intervensjons -og oppfølgingsskjema ferdigstilt.'),
#                                  # h4(tags$b('Oppfølging i kladd '), 'viser antall forløp med ferdigstilte intervensjonsskjema og oppfølging i kladd.'),
#                                  # h4(tags$b('Ferdig basisreg. oppfølging mangler '), 'viser antall forløp med ferdigstilte intervensjonsskjema og ikke påbegynt eller slettet oppfølging'),
#                                  # h4(tags$b('Basisreg. i kladd '), 'viser antallet basisregistreringer i kladd.'),
#                                  br(),
#                                  br(),
#                                  DTOutput(ns("Tabell_adm1")), downloadButton(ns("lastNed_adm1"), "Last ned tabell")),
#                         tabPanel("Registreringer over tid", value = "id_ant_tid",
#                         #          DTOutput(ns("Tabell_adm2")), downloadButton(ns("lastNed_adm2"), "Last ned tabell")
#                         )
#   )
#   )
#
# )
