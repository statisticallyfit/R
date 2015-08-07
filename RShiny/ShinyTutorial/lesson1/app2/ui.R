# tutorial link: shiny.rstudio.com/tutorial/lesson2/

shinyUI(fluidPage(
  titlePanel("My Shiny App"),
  
  sidebarLayout(#position="right",
    sidebarPanel("sidebar panel"),
    mainPanel(
      h1("First level title"),
      h2("Second  level title"),
      h3("Third  level title"), 
      h4("Fourth level title"), 
      h5("Fifth level title"),
      h6("Sixth  level title"),
      
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'Monaco'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph.")
    )
  )
))