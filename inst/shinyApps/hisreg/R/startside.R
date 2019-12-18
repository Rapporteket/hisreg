startsideUI <- function(id){
  ns <- NS(id)
  shiny::bootstrapPage(
    div(class = "container",
        div(class = "panel panel-default",
            div(class = "panel-heading" , style = "background-color : #E0E0E0 ",
                h2('Velkommen til Rapporteket - Hisreg', align='center')),
            div(class = "panel-body",style = "background-color:#F0F0F0",
                div(class="panel-text",
                    br(),
                    # h4(tags$b('Her skal XXX og YYYY formulere kloke og reflekterte meldinger til Rapportekets brukere. En foreløpig variant er gitt under:')),
                    # br(),
                    h4('Du er nå inne på Rapporteket for Hisreg, registerets resultattjeneste.
                Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
                    h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig. Alle figurer og
                tabeller kan lastes ned.'),
                    br(),

                    h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                    div(class = "container", style ="margin-right:(@gutter / 10)" ,
                        h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.'),
                        h4(id = ns("SC1"), tags$b('Gjennomsnitt per sykehus '), 'viser gjennomsnitt av valgt variabel per sykehus opp mot landsgjennomsnittet.'),
                        h4(tags$b('Gjennomsnitt per intervensjonstype'), ' viser gjennomsnitt av valgt variabel per intervensjonstype opp mot landsgjennomsnittet.'),
                        h4(id = ns("SC2"),tags$b('Gjennomsnitt før og etter, per sykehus'), ' viser gjennomsnitt av valgt variabel før og etter intervensjon per sykehus.'),
                        h4(tags$b('Gjennomsnitt før og etter, per intervensjontype'), ' viser gjennomsnitt av valgt variabel før og etter intervensjon per intervensjonstype.'),
                        h4("Konfidensintervaller er inkludert i alle figurer og tabeller med gjennomsnittsverdier."),
                        h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer og pasientforløp/unike pasienter.')
                    ),
                    br(),
                    br(),
                    #h3('HER KAN MAN F.EKS. VISE ANTALL REGISTRERINGER SISTE X MND.'),
                    br(),
                    br(),
                    div(class="container",
                        fixedRow(
                          column(width = 4, offset = 1,
                                 h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                                    a("Hisreg", href="https://www.kvalitetsregistre.no/registers/565/resultater"),
                                    target="_blank", align='center' )),
                          column(width = 4,offset = 2,
                                 h4('Mer informasjon om registeret finnes på muskelregisteret sin hjemmeside: ', align='center',
                                    a("www.hisreg.no", href="http://www.hisreg.no", target="_blank"))
                          )
                        )
                    )
                )
            )
        )))
}

startside <- function(input, output,session, usrRole){
  observe(
  if (usrRole != "SC") {
    shinyjs::hide("SC1")
    shinyjs::hide("SC2")
  }
  )
}
