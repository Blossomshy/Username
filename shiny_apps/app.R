library(shiny)

check_username=function(username,first_name=NA,last_name=NA,topic_word=NA){
  gchara=c(0:9,"-","_",LETTERS,letters)
  x=strsplit(username,split = "")
  x=x[[1]]
  y=x[length(x)]
  sum=sum(x%in%(1:9))
  s1="Please input your username."
  s2="Please do not use characters other than 0-9, a-z, A-Z, - and _."
  s3="Please consider removing the trailing numbers."
  s4="Please consider improving it a bit."
  s5="This username is too long."
  s6="This is a good username to use."
  i1=ifelse(length(x)>30,s5,s6)
  i2=ifelse(y%in%(1:9),s3,i1)
  i3=ifelse(sum==length(x),s4,i2)
  i4=ifelse(FALSE%in%(x%in%gchara),s2,i3)
  suggests=ifelse(length(x)>0,i4,s1)
  return(suggests)
}

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      textInput("username","Please input your username.",value = ""),   
      textInput("last_name","Please input your last_name",value = ""),
      textInput("first_name","Please input your first_name",value = "")
    ),
    mainPanel(
      
      h3("Your username"),
      verbatimTextOutput("username"),
      br(),
      
      h3("Our evaluation"),
      verbatimTextOutput("evaluation"),
      br(),
      
      h3("Our suggestions"),
      verbatimTextOutput("suggestions"),
      br()
      
    )
    
  )
  
)

server = function(input,output){
  output$username=renderText({
    input$username
  })
  output$evaluation=renderText({
    check_username(input$username)
  })
  output$suggestions=renderText({
    paste0(input$first_name,input$last_name)
  })
}

shinyApp(ui=ui,server=server,options=list(display.mode="showcase"))

