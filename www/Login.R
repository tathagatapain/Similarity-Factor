#### Log in module ###
USER <- reactiveValues(Logged = Logged)

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "Username:"),
      passwordInput("passwd", "Password:"),
      br(),

      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        Id.username <- which(PASSWORD$UN == Username)
        Id.password <- which(PASSWORD$Pass    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER$Logged <- TRUE
          } 
        } else  {
          # tags$head(
          #   tags$style(HTML("
          #                   p {
          #                   color;red;
          #                   } 
          #                   "))
          #   )
          #div(style="color:red", "User name or password failed!")
         "User name or password failed!"
          #HTML(paste("This text is ", tags$span(style="color:red", "red"), sep = ""))
        }
      } 
    }
  }
})