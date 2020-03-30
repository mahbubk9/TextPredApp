pageWithSidebar(
  headerPanel("Text Prediction App"),
  sidebarPanel(
    textInput("n", "Enter Your Text Below:",""),
    br(),
    actionButton("goButton", "Go!"),
    p("Click on the Go button to get upto 5 next word prediction.")
  ),
  mainPanel(
    verbatimTextOutput("nText")
  )
)