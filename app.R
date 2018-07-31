# ===== Set up Session ============================================================================

packs <- c('shiny','rsconnect','mailR','rmarkdown','knitr','kableExtra','tinytex','profvis','dplyr','stringr','testthat','readr')

invisible(lapply(packs, require, character.only = TRUE))
rm(packs)


# ===== User Interface ============================================================================

ui <- fluidPage(
  # *Input() functions,
  # *Output() functions
  fluidRow(
    column(width = 3,
           h3("Actions"),
           submitButton("SendMail", "Send the mail"),
           br(),
           submitButton("SFcsv", "Get the SFcsv"),
           br(), 
           submitButton("DBScsv", "Get the DBScsv"),
           br(), 
           submitButton("Aktualisieren", "Aktualisieren"),
           h3("Important Data"),
           textInput("Partneremail",tags$b("Partneremail"),value = "John Doe <versicherungscheck360.com>"),
           textInput("CC-Mail","CC-Mail"),
           selectInput("Anwaltskanzlei","Anwaltskanzlei",choices = c("MZS","Wirth","von Fedak")),
           textInput("Policennr","Policennr")),
    
           
    
    
    column(2,
           h3("Mark the true statements"),
           checkboxInput("Erfüllungsanspruch", "Erfüllungsanspruch", value = FALSE),
           checkboxInput("Rechtsschutz", "Rechtsschutz", value = FALSE),
           checkboxInput("Antrag", "Antrag vorhanden", value = FALSE),
           checkboxInput("Police", "Police vorhanden", value = FALSE),
           checkboxInput("EAPolice", "EA laut Police", value = FALSE),
           checkboxInput("EAAntrag", "EA laut Antrag", value = FALSE),
           checkboxInput("Auszahlungen", "Auszahlungen vorgesehen", value = FALSE),
           checkboxInput("Beitragsverlauf", "Ist der Beitragsverlauf klar", value = FALSE)),
    
    column(2,
           h3("Numeric Values"),
           numericInput("Anspruch", tags$b("Anspruch"), value = NA),
           numericInput("RKW", tags$b("Rückkaufswert"), value = NA),
           numericInput("EA", tags$b("Erfüllungsanspruch"), value = NA),
           numericInput("EingezahlteBeiträge", "Eingezahlte Beiträge", value = NA),
           numericInput("Regelbeitrag", "Regelbeitrag", value = NA),
           selectInput("Beitragsfrequenz","Beitragsfrequenz",choices = c("Einmalbeitrag","5+7 (Feeder)","Jährlich","Vierteljährlich","Jährlich","Sonstige"),selected = "Sonstige")),
    
    
    
    
    column(2,
           h3("Kundendaten"),
           textInput("Nachname","Nachname"),
           textInput("Vorname","Vorname"),
           selectInput("Anrede","Anrede",choices = c("Frau","Mann","keine Angabe"),selected = "keine Angabe"),
           textInput("Titel","Titel"),
           textInput("Nachname","Nachname")),
    
    column(2,
           dateInput("Policenbeginn","Policenbeginn", 
                     value = "1990-01-01"),
           dateInput("Kündigungsdatum","Kündigungsdatum", 
                     value = Sys.Date()),
           textInput("Straße & Hausnr.","Straße & Hausnr."),
           textInput("Postleitzahl","Postleitzahl"),
           textInput("Stadt","Stadt"),
           textInput("Land","Land",value = "Deutschland"))
    

           
  ),
  plotOutput(outputId = 'graph',width = 645-129, height = 690-138),
  htmlOutput(outputId = "emailbody1",inline=TRUE),
  htmlOutput(outputId = "emailbody2",inline=TRUE)
  
  
  #Nachname	Vorname	Titel	Geschlecht	Anspruch	RKW	Anwaltskanzlei	Rechtsschutz	Erfüllungsanspruch	Erfüllungsanspruch in EUR	Partneremail	CC.Email	Policennr	Policenstatus	Produktname	Versicherungsgesellschaft	benoetigteDaten	Policenbeginn	Kündigungsdatum	Bemerkung	Straße	Postleitzahl	Stadt	Land	E.Mail	Antrag vorhanden	Police vorhanden	EA.Antrag	EA.Police	Auszahlungen vorhanden	Beitragsverlauf klar	fehlende Daten	Dynamik.p.a.	Eingezahlte.Beiträge	Regelbeitrag	Beitragsfrequenz	Anspruch.ohne.Nutzungsentschädigung	Rechtsschutznr	RKW_Datum	Firma

  #actionButton("buttonGetSFcsv")
  #actionButton("buttonGetDBScsv")
  
)

# ===== Server ====================================================================================

server <- function(input,output) {
  # Rules:
    # Save objects to display output$
      # output$hist <- # code
  
    # Build objects to display with render*()
      # Use render*() function that creates the type of output you wish to make
  
    # Use input values with input$
  
  output$graph <- renderPlot({ 
    Kundenanspruch <- ifelse(input$Rechtsschutz && !input$Erfüllungsanspruch, round(input$Anspruch*0.8),
                             ifelse(!input$Rechtsschutz && !input$Erfüllungsanspruch,round(input$Anspruch*0.65),
                                    ifelse(input$Rechtsschutz && input$Erfüllungsanspruch,round(input$EA*0.8),
                                           round(input$EA*0.65))))
    Erfolgshonorar <- ifelse(input$Rechtsschutz && !input$Erfüllungsanspruch, round(input$Anspruch*0.1),
                             ifelse(!input$Rechtsschutz && !input$Erfüllungsanspruch,round(input$Anspruch*0.35),
                                    ifelse(input$Rechtsschutz && input$Erfüllungsanspruch,round(input$EA*0.2),
                                           round(input$EA*0.35))))    
    Vertragswerte <- c(input$RKW,0,0)
    Rückerstattungen <- c(input$RKW,Erfolgshonorar,Kundenanspruch)
    m1 <- matrix(c(Vertragswerte, Rückerstattungen),nrow = 3)
    barplot(height = m1,names.arg = c("Rückkaufswert","Rückkaufswert & Rückerstattung"),space = 1.5,
            main = paste("Ihre pot. Rückerstattung (",input$Policennr,"):",sep =""),las=1,
            col=c("darkblue","grey","darkgreen"),
            args.legend = list(x="topleft"),
            legend.text = c("Rückkaufswert","Erfolghonorar","Ihr potentieller Anspruch"))
    text(y = input$RKW/2, x = 2, labels = paste(round(input$RKW),"€"), col = "white")
    text(y = input$RKW/2, x = 4.5, labels = paste(round(input$RKW),"€"), col = "white")
    text(y = input$RKW+Erfolgshonorar/2, x = 4.5, labels = paste(Erfolgshonorar,"€"), col = "black")
    text(y = input$RKW+Erfolgshonorar + Kundenanspruch/2, x = 4.5, labels = paste(Kundenanspruch,"€"), col = "white")
    
    
  })

  
}

# ===== shinyApp ==================================================================================

shinyApp(ui = ui, server = server)

#name the file app.R for the server
#or split into server.R and ui.R

