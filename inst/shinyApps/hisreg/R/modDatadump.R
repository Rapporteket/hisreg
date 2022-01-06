dataDumpUI <- function(id) {
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(id=ns("sidebarDataDump"),
                        shiny::selectInput(
                          ns("ddselect"),
                          "Valg datadump:",
                          choices =
                            list("AlleVarNum","AlleVar","ForlopsOversikt", "Followups",
                                 "FollowupsNum", "SkjemaOversikt"),
                          selected = "AlleVarNum"
                        ),
                        shiny::dateRangeInput(
                          ns("ddDateRange"),
                          "Tidsperiode",
                          start = "2008-01-01" ,
                          end = Sys.Date() ,
                          min = "2008-01-01" ,
                          max = Sys.Date(),
                          separator = "til",
                          language = "no"
                        ),
                        fluidRow(
                          column(6, offset = 6,
                                 shiny::downloadButton(
                                   style="float:right;margin-top:5px",
                                   ns("dataDumpNedLasting"),
                                   label = "Last ned!",
                                 ))
                        )),
    shiny::mainPanel(
      fluidRow( column(width = 10,
                       shiny::tags$h3('Datadump - Hisreg', align='center'),
                       shiny::tags$hr(),
                       shiny::tags$p(
                         'Her kan du laste ned forskjellige varianter av datadump for Hisreg. Lokale brukere vil bare kunne laste ned
                         data for egen avdeling. Merk at all datofiltrering gjøres på Hoveddato, som i de fleste tilfeller sammenfaller
                         med intervensjonsdato. Dvs. at et valgt datointervall for oppfølgingsdata ikke vil gi deg oppfølgingene som
                         er utført i perioden, men oppfølgingene som finnes for en intervensjon i gitt periode.'
                       ),
                       shiny::tags$h5(
                         shiny::tags$b(shiny::tags$u(
                           'Forklaring til de ulike datadump-typene:'
                         ))),
                       shiny::tags$div(class = "container",
                                       shiny::tags$h5(
                                         shiny::tags$b('AlleVar '),
                                         'inneholder alle kliniske variabler i registeret og benytter
            etikettene til kategoriske variabler.'
                                       ),
                                       shiny::tags$h5(
                                         shiny::tags$b('AlleVarNum '),
                                         'inneholder alle kliniske variabler i registeret og benytter
            tallkodene til kategoriske variabler.'
                                       ),
                                       shiny::tags$h5(
                                         shiny::tags$b('ForlopsOversikt '),
                                         'inneholder en del administrative data relevant for forløpene.'),
                                       shiny::tags$h5(
                                         shiny::tags$b('Followups '),
                                         'inneholder data fra oppfølgingsskjema og benytter
            etikettene til kategoriske variabler.'
                                       ),
                                       shiny::tags$h5(
                                         shiny::tags$b('FollowupsNum '),
                                         'inneholder data fra oppfølgingsskjema og benytter
            tallkodene til kategoriske variabler.'
                                       ),
                                       shiny::tags$h5(
                                         shiny::tags$b('SkjemaOversikt '), '
            er en oversikt over status til alle registreringer i registreret,
            også uferdige.')
                       )
      ))
    )
  )
}

dataDump <- function(input, output, session, userRole, reshID, mainSession){

  #add HovedDato to allevar and allevarnum
  AddHovedDatoVariabels <-   reactive({
    switch (input$ddselect,
            "AlleVarNum" = ", ForlopsOversikt.ForlopsID, ForlopsOversikt.HovedDato, ForlopsOversikt.AvdRESH ",
            "AlleVar" = ", ForlopsOversikt.ForlopsID, ForlopsOversikt.HovedDato, ForlopsOversikt.AvdRESH ",
            "Followups" = ", ForlopsOversikt.ForlopsID, ForlopsOversikt.HovedDato, ForlopsOversikt.AvdRESH ",
            "FollowupsNum" = ", ForlopsOversikt.ForlopsID, ForlopsOversikt.HovedDato, ForlopsOversikt.AvdRESH ",
            "ForlopsOversikt" = "",
            "SkjemaOversikt" = ""
    )
  })
  AddHovedDatoJoin <-   reactive({
    switch (input$ddselect,
            "AlleVarNum" = "INNER JOIN ForlopsOversikt
            ON AlleVarNum.m_mceid = ForlopsOversikt.ForlopsID ",
            "AlleVar" = "INNER JOIN ForlopsOversikt
            ON AlleVar.m_mceid = ForlopsOversikt.ForlopsID ",
            "Followups" = "INNER JOIN ForlopsOversikt
            ON Followups.c_mceid = ForlopsOversikt.ForlopsID ",
            "FollowupsNum" = "INNER JOIN ForlopsOversikt
            ON FollowupsNum.c_mceid = ForlopsOversikt.ForlopsID ",
            "ForlopsOversikt" = "",
            "SkjemaOversikt" = ""
    )
  })

  qry <-   reactive({
    # if (userRole ==  "SC") {
      paste0(
        "SELECT ", input$ddselect, ".* ", AddHovedDatoVariabels(),
        " FROM ", input$ddselect, " ", AddHovedDatoJoin())
    # } else {
    #   paste0(
    #     "SELECT ",input$ddselect, ".* ", AddHovedDatoVariabels(),
    #     " FROM ", input$ddselect, " ", AddHovedDatoJoin(), " WHERE ",
    #     input$ddselect, ".AvdRESH = ", reshID
    #   )
    # }
  })


  output$dataDumpNedLasting <- shiny::downloadHandler(
    filename = function(){
      paste0(input$ddselect,"Hisreg",Sys.Date(),".csv")
    },
    content = function(file){
      dataDump <- rapbase::LoadRegData(
        registryName = "hisreg",
        query = qry(),
        dbType = "mysql"
      )
      dataDump <- dataDump %>%
        dplyr::filter(
          HovedDato %>%
            dplyr::between(
              min(input$ddDateRange),
              max(input$ddDateRange)
            )
        )
      if (userRole !=  "SC") {
        dataDump <- dataDump[which(dataDump$AvdRESH == reshID), ]
      }
      write.csv2(dataDump, file, fileEncoding = "Latin1", row.names = F)
    }
  )
  shinyjs::onclick(
    "dataDumpNedLasting",
    rapbase::repLogger(
      session = mainSession,
      msg = paste0(
        "Hisreg: datadump ", input$ddselect, " ",
        min(input$ddDateRange),"-",max(input$ddDateRange)
      )
    )
  )
}
