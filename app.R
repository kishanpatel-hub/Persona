#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(googleAuthR)
library(DT)
library(googleID)
library(RMySQL)
library(plotly)
library(twitteR)
library(ROAuth)
library(ggplot2)
library(rio)
library (tm)
library(dplyr)
library(tidytext)
library(rtweet)

# save credentials
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "exjWo5tA6JaBJEPPYWeH4lyFP"
consumerSecret = "BRS10CeQR1BW4quWKFT2WqDE45FVHo1N5ixJcX7VkF3Z2JTy0P"

accessToken = "774226848200871937-6YBBeN0pvXU12eTWDOm7CbbvCM9ulrX"
accessSecret = "WFqsPnXsubXlrMIaLy69zBjhhHxGBnQkBpb8JcmeN9UVU"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessSecret)

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "341245573152-o6mguk9igike360vd10euml9tuul39g2.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "kbnyuLgULIsYTSuC8LcmJjbB")



# Define UI for application that draws a histogram

ui <- tagList(
      
  useShinyjs(),
  sidebarLayout(position = "right",
                sidebarPanel(
                  tags$style(".well {background-color:white ; width:200px;  height:20px; margin-top:10px; margin-left:270px; border:0px;}"),
                  
                  googleAuthUI("gauth_login")
                  
                  ),
                mainPanel(
                  img(src="logo.png",height=170,width=342)
                )
                
  ),
  navbarPage(
  title = "",
  id= "navbar",
  
  tabPanel("Home", 
       
         
            fluidPage(
              fluidRow(
                column(3,
                       textOutput("welcome"),
                      
                         
                       hidden(
                         
                         div(id='hid1',
                       textInput("twitter_id","Enter Twitter ID:"),
                       textOutput("msg"),
                       actionButton("page1", "Generate Report" )
                  
                       
                       
                       )),
                       tags$head(
                         tags$style(HTML('#page1{background-color:#E0E0E0  ; 
                                         margin-bottom:20px;margin-right:50px;
                                         font: bold 18px/20px Georgia, serif;}',
                                         '#Preview{background-color:#E0E0E0  ; 
                                         margin-bottom:20px;margin-right:50px;
                                         font: bold 18px/20px Georgia, serif;}',
                                         '#welcome{   
                                         margin-top:30px;margin-bottom:20px;
                                         font: bold 22px/20px Georgia, serif;}',
                                         '#twitter_id{   
                                         margin-bottom:20px;
                                         font: bold 18px/20px Georgia, serif;}',
                                         '#msg{   
                                         margin-bottom:20px;
                                         font: 18px/20px Georgia, serif; color:red}'
                                         
                                          ))
                         )
                    
                       
                ),
                column(9,
                       img(src="dims.jpg" ,style= "margin-top:80px")
                )
              )
            )
              
       
        
           ),
           
  navbarMenu("Preview",
  tabPanel(title="Report based on User\'s Profile", value="Tab2",
           
           # plotlyOutput("plot"),
          
           fluidPage(
             fluidRow(
               
               column(8,
                      h2("Personality Report Based on User's Profile", align="center"),
                      h3("Personal Information:" ),
                      htmlOutput('user_detail1'),
                      htmlOutput('img1'),
                      tags$head(tags$style(HTML("#user_detail1{
                                           font: 15px Georgia, serif;
                                                margin-top:20px;
                                                }",
                                                 'h2{   
                                                margin-top:40px; margin-bottom:60px;
                                                font: bold 26px/20px Georgia, serif;}',
                                                'h3{   
                                                 margin-top:50px; margin-bottom: 20px;
                                                font: bold 20px/20px Georgia, serif;}'
                                                
                                                )
                         )
                      ),
                      h3("Personality Traits Rating: "),
                      plotlyOutput('plot1'),
                      h3("OCEAN Personality Indicator:"),
                      img(src="Ocean.jpe", height=550, width=587, style= "margin-top:20px; margin-left:150px; margin-bottom:50px;"),
                      plotlyOutput('plot2'),
                     
                      h3("Positive Negative Tweet Ratio:"),
                      plotlyOutput('plot3'),
                      # tableOutput("df"),
                    
                      tags$head(
                        tags$style(HTML('#downloadReportHTML{background-color:#E0E0E0; 
                        margin-bottom:10px;margin-left:10px; margin-right:10px;
                      font: 18px/20px Georgia, serif;}',
                                        '#page2{background-color:#E0E0E0; 
                     margin-bottom:20px;margin-right:50px;
                      font: bold 18px/20px Georgia, serif;}',
                                        '#Preview2{background-color:#E0E0E0; 
                     margin-bottom:20px;margin-right:50px;
                                        font: bold 18px/20px Georgia, serif;}'))
                      )
                      
                     
                      
               ),
               column(1),
               column(3,
                      actionButton("page2", "Know What People Think about you", style="float:center"),
                      
                      img(src="fact.png",height=325,width=325,style= "margin-top:80px; margin-left:20px;")  
                    
                      )
               
             )
           )
            

  ),
  tabPanel(title="Report based on Public Opinion ", value="Tab3",
           
           
           fluidPage(
             fluidRow(
              
               column(8,
                      h2("Personality Report Based on Public Opinion", align="center"),
                      
                      h3("Personal Information:" ),
                      htmlOutput('user_detail2'),
                      htmlOutput('img2'),
                      tags$head(tags$style(HTML("#user_detail2{
                                           font: 15px Georgia, serif;
                                                margin-top:20px;
                                           }"
                         ))
                      ),
             h3("Personality Traits Rating: "),
             plotlyOutput('plot1.1'),
             h3("OCEAN Personality Indicator:"),
             img(src="Ocean.jpe", height=550, width=587, style= "margin-top:20px; margin-left:150px; margin-bottom: 50px;"),
             plotlyOutput('plot2.1'),
             
             h3("Positive Negative Tweet Ratio:"),
             plotlyOutput('plot3.1'),
             tags$style(HTML('#download_ReportHTML{background-color:#E0E0E0; 
                            margin-bottom:10px;margin-left:10px; margin-right:10px;
                             font: 18px/20px Georgia, serif ; }',
                             '#download_ReportHTML_f{background-color:#E0E0E0; 
                            margin-bottom:0px;margin-left:10px; margin-right:10px;
                             font: 18px/20px Georgia, serif;}'))
             
            
            
            # tableOutput("df_1") 
               ),
            column(1),
            column(3,
                   img(src="a_fact.png",height=325,width=325,style= "margin-top:130px; margin-left:20px;")  
                   )
            
             
             )
           )
           
          
     
  )),
  navbarMenu("Downloads", tabPanel( value="Tab7",downloadButton('downloadReportHTML','User\'s Profile')),
 
             
             tabPanel( value="Tab8",downloadButton('download_ReportHTML','Public Opinion')),
             tabPanel( value="Tab9",downloadButton('download_ReportHTML_f','Full Report'))
  
             
             
  
  
  ),
  tabPanel(title="History", value="Tab4",
           
           
           fluidPage(
             fluidRow(
               column(12,
                    
                  DT::dataTableOutput('hist') 
                  #tableOutput('hist')  
               )
               
             )
           )
           
           
  ),
  tabPanel(title="Feedback", value="Tab5",
           
           
           fluidPage(
             fluidRow(
               column(4,
                      sliderInput("Feedback", "Feedback:",
                                  min = 0, max = 10,
                                  value = 0),
                     
                      textAreaInput("comment","Comment:"),
                      
                      actionButton("c_submit", "Submit" )
                      ),
               column(4,
                      
                       htmlOutput('feed'),
                      
                    
                      
                      hidden (
                        div(id='hid3',htmlOutput('thanks'))
                      )
                      
                      ),
               column(4,
                      
                      htmlOutput('feed1'),
                      
                      
                      
                      hidden (
                        div(id='hid4',htmlOutput('thanku'))
                      )
                      
               )
               
                      )
               )
           
           
           
    ),
  tabPanel(title="About us", value="Tab6",
           
           
           fluidPage(
             fluidRow(
               column(12,
                      h2("About Persona:"),
                      htmlOutput('about'),
                      h2("Team:"),
                      tags$head(tags$style(HTML('h2{   
                                                margin-top:20px; margin-bottom:10px;
                                                font: bold 26px/20px Georgia, serif;}',
                                                '#about{   
                                                margin-top:10px; margin-bottom:20px; line-height: 20px;
                                                font: 20px/20px Georgia, serif;}'
                                                
                      )
                      )
                      ),
             fluidRow(
               column(3,
               img(src="kishan1.png",height=300,width=300,style= "margin-top:0px; margin-left:20px;")
                
                      
               ),
               column(3,
                      h4("Kishan Patel"),
                      h4("Data Analyst at iken Solutions"),
                      
                      h4("Kishan19972@gmail.com"),
                      h4("+917405122981 "),
                      a(img(src="linkedin.png",height=50,width=50,style= "margin-top:10px;"),href="https://www.linkedin.com/in/kishan-p-930968a2"),
                      tags$head(tags$style(HTML("h4{
                                                
                                                font: bold 20px/20px Georgia, serif;
                                               
                                                }"
                                          )))
                      
     
               ),
               column(3,
                      img(src="vatsal.png",height=300,width=300,style= "margin-top:0px; margin-left:20px;")
                      
                      
               ),
               column(3,
                      h4("Vatsal Sanghvi"),
                      h4("Data Analyst at iken Solutions"),
                      
                      h4("vatsalsanghvi13@gmail.com"),
                      h4("+919601667199"),
                      a(img(src="linkedin.png",height=50,width=50,style= "margin-top:10px;"),href="https://www.linkedin.com/in/vatsal-sanghvi-9a319299/"),
                      tags$head(tags$style(HTML("h4{
                                                font: 20px Georgia, serif;
                                                margin-top:20px;  
                                                }"
                      ))))
               )
               
               
             ))
           )
           
           
           
  )
  
),tags$style(type = 'text/css', HTML('.navbar { background-color: #FFFFFF; }
             .navbar-default .navbar-brand{color: white;}
                               .tab-panel{ background-color: red; color: white}
                               .navbar-default .navbar-nav > .active > a, 
                               .navbar-default .navbar-nav > .active > a:focus, 
                               .navbar-default .navbar-nav > .active > a:hover {
                               color: #555;
                               background-color: #E0E0E0;
                               }'))

)

server <- function(input, output, session) {
  ## Global variables needed throughout the app
  rv <- reactiveValues(
    login = FALSE
  )
  
  observe({
    hide(selector = "#navbar li a[data-value=Tab2]")
    hide(selector = "#navbar li a[data-value=Tab3]")
    hide(selector = "#navbar li a[data-value=Tab4]")
    hide(selector = "#navbar li a[data-value=Tab5]")
     hide(selector = "#navbar li a[data-value=Tab7]")
     hide(selector = "#navbar li a[data-value=Tab8]")
     hide(selector = "#navbar li a[data-value=Tab9]")
    hide(selector = "#navbar li:nth-child(3)")
    hide(selector = "#navbar li:nth-child(2)")
  })
  

  
  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-primary",approval_prompt = "force")
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    
    with_shiny(get_user_info, shiny_access_token = accessToken())
    
    
    ## Display user's Google display name after successful login
  })
  
  
  output$welcome <- renderText({
  
    G_id <- as.numeric(userDetails()$id)
    
    name <- as.character(userDetails()$displayName)
    
    email <- as.character(userDetails()$emails$value)
    welcome <- paste("Welcome,  ",name)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    query <- sprintf(
      #  "INSERT ignore INTO users (ID, name, email) VALUES (%s,'%s','%s')", id, name, email
      
      "INSERT INTO login_activity (G_ID,name,email) SELECT * FROM (SELECT %s,'%s','%s') AS tmp
      WHERE NOT EXISTS ( SELECT G_ID FROM login_activity WHERE G_ID = %s) LIMIT 1", G_id,name,email,G_id
    )
    
    dbGetQuery(mydb, query)
    query1 <- sprintf(
      "Select id from login_activity where G_ID = %s",G_id
    )

    id <-  dbGetQuery(mydb, query1)
    
    
    query3 <- sprintf(
      
      "INSERT INTO feedback (ID) SELECT * FROM (SELECT %s) AS tmp
      WHERE NOT EXISTS ( SELECT ID FROM feedback WHERE ID = %s) LIMIT 1", id,id
      
    )
    
    dbGetQuery(mydb, query3)
    
    dbDisconnect(mydb)
    
    if(nchar(name) != 0)
    {
     
    toggle('hid1')
    toggle(selector = "#navbar li a[data-value=Tab5]")
    toggle(selector = "#navbar li a[data-value=Tab4]")
    }
    
    welcome
  })
  
  observeEvent(input$navbar,{

  output$hist <- DT::renderDataTable({
    G_id <- as.numeric(userDetails()$id)
    
    name <- as.character(userDetails()$displayName)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    
 
    
    query <- sprintf(
      "select Date,t.name,Screen_name from login_activity l, t_userdetails t where l.ID= t.ID and G_ID=%s",G_id
    )
    
    df <- dbGetQuery(mydb, query)
    
    dbDisconnect(mydb)
    
    df
    
  })
  })
 

  output$feed <- renderUI({
   
    HTML(paste('<img src="',input$Feedback,'.png",  style="position:absolute;top:20px;left:20px;">',sep = ""))
  })
  
  output$feed1 <- renderUI({
    
    HTML(paste('<img src=".',input$Feedback,'.png",  style="position:absolute;top:20px;left:20px;">',sep = ""))
  })
  
  output$thanks <- renderUI({
    
    HTML(paste('<img src="thank.png" style="position:absolute;top:20px;left:20px;">',sep = ""))
  })
  
  output$thanku <- renderUI({
    
    HTML(paste('<img src="thanku.png" style="position:absolute;top:20px;left:20px;">',sep = ""))
  })
  
  
  output$about <- renderUI({
    
    str0 <- paste("    <t/>")
    str1 <- paste(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    Curious about your personality ? or what people think about you ? 
                  <p/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                   We here at persona understand the importance of Personality in human life which will stick with you even when you're old, but it can be improved over the time to excel in carrer and personal life.
                    Nature and personality can only be improved when one knows who he really is and what others think about him. And we are here to help you with that. 
                      We believe that Social Media is the best way to find your personality then current methods of
                  going through long, boring quenstionaries. Persona is web-based application which analyzes twitter data using advanced text mining techniques and generates personality report for user. ")
  
    HTML(paste(str1 ,sep ="   "))
    
  })
  
  
  
  observeEvent(input$Feedback, {
    G_id <- as.numeric(userDetails()$id)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")

    query1 <- sprintf(
      "Select id from login_activity where G_ID = %s",G_id
    )
    
    id <-  dbGetQuery(mydb, query1)
    
    query <- sprintf(
     "update feedback set rating =  %s where id = %s",input$Feedback,id
        )
    
    dbGetQuery(mydb, query)
    
    dbDisconnect(mydb)
    
    
  })
  
  observeEvent(input$c_submit, {
    G_id <- as.numeric(userDetails()$id)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    
    query1 <- sprintf(
      "Select id from login_activity where G_ID = %s",G_id
    )
    
    id <-  dbGetQuery(mydb, query1)
    
    query <- sprintf(
      "update feedback set Comment =  '%s' where id = %s",input$comment,id
    )
    
    toggle('hid3')
    toggle('hid4')
    
    dbGetQuery(mydb, query)
    
    dbDisconnect(mydb)
  
  })
  
  observeEvent(input$page1, {
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = 'Thank you for clicking')
   
    withProgress(message = 'Generating Report', value = 0, {
      
    incProgress(1/8, detail = paste("Validating Twitter ID"))
 
    t_id <- as.character(input$twitter_id)
 
   
    
    G_id <- as.numeric(userDetails()$id)
    
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    
    
    error <- get_timeline(t_id,1)
    
    if(length(error)==0)
    {
      output$msg <- renderText({
        message <- "Invalid Twitter ID" 
      })
    }
    else{
      
    
    # 
    # 
   
    query1 <- sprintf(
      "Select id from login_activity where G_ID = %s",G_id
    )
    
    query2 <- sprintf(
      "Select * from t_userdetails ORDER BY U_ID DESC LIMIT 1"
    )
   
    
    
    
  
    id <-  dbGetQuery(mydb, query1)
    
  
    trigger <- 0
    
   
    incProgress(1/6, detail = paste("Downloading Twitter data"))
    
    Final <- mainfun(t_id,id,trigger)
    
   
    incProgress(1/4, detail = paste("Performing Analysis"))
    
    user_d <- dbGetQuery(mydb, query2)
    
    u_id <- user_d$U_ID
    
    dbDisconnect(mydb)
    
    incProgress(0, detail = paste("Done"))
   
    toggle(selector = "#navbar li a[data-value=Tab2]")
 
    
    
    
    
    toggle(selector = "#navbar li:nth-child(2)")
    

    
  
   
    updateTabsetPanel(session, "navbar", selected = "Tab2")
    
    
    
    output$user_detail1 <- renderUI({
      str1 <- paste("<br/><b>Name:</b>", user_d$Name)
      str2 <- paste("<b>Screen name:</b>", user_d$Screen_name)
     
      str5 <- paste("<b>Instagram URL:</b>", user_d$Instagram_url)
      str6 <- paste("<b>Followers:</b>", user_d$Followers)
      str7 <- paste("<b>Following:</b>", user_d$Following)
      str8 <- paste("<b>Tweets count:</b>", user_d$Tweet_count)
      str9 <- paste("<b>Account created on:</b>", user_d$Account_created)
      str10 <- paste("<b>Verified:</b>", user_d$Verified)
      
      if(nchar(user_d$Location) == 0)
      {
        str3 <- "<b>Location:</b> NOT AVAILABLE"
      }else
      {
        str3 <- paste("<b>Location:</b>", user_d$Location)
      }
      
      if(nchar(user_d$Description) == 0)
      {
        str4 <- "<b>Description:</b> NOT AVAILABLE"
      }else
      {
        str4 <- paste("<b>Description:</b>", user_d$Description)
      }
      
      HTML(paste(str1,str2,str3,str4,str5,str6,str7,str8,str9,str10, sep = '<br/><br/>'))
    }
    )
    
    
    output$img1 <- renderUI({
      HTML(paste('<img src="',user_d$Profile_image,'" style="position:absolute;top:160px;right:0px; width: 130px;">'))
    })
    
    p <- Final$Sentiment[Final$Traits=="Positive_tweet"]
    n <- Final$Sentiment[Final$Traits=="Negative_tweet"]
    ne <- Final$Sentiment[Final$Traits=="Neutral_tweet"]
    Count <- c(p,n,ne)
    Names <- c("Postive","Negative","Neutral")
    
    T_senti <- data.frame(Names,Count)
    
    
    pl1 <- Final[Final$Traits != 'Positive_tweet' & Final$Traits != 'Negative_tweet' &  Final$Traits != 'Neutral_tweet' &  Final$Traits != 'Low_ext' ,]
    
    pl1$Traits <- factor(pl1$Traits)
    
    output$plot1 <- renderPlotly({
      p1 <- plot_ly(pl1,x=~Traits,y=~Sentiment,type='bar')
      
    })
    
    result1 <- get_personality(Final)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    query4 <- sprintf(
      "INSERT INTO t_result (U_ID, ID, Anger , Trust , Disgust , Fear , Anticipation , Joy , Negative, Positive, Sadness, Surprise, 
      T_Positive, T_Negative, T_Neutral, High_Extraversion, Low_Extraversion, High_Neuroticism, Low_Neuroticism, High_Conscientiousness, Low_Conscientiousness, High_Openness, Low_Openness, High_Agreeableness, Low_Agreeableness )
      VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)", 
      u_id, id, Final$Sentiment[Final$Traits=='Anger'], Final$Sentiment[Final$Traits=='Trust'],Final$Sentiment[Final$Traits=='Disgust'],Final$Sentiment[Final$Traits=='Fear'],Final$Sentiment[Final$Traits=='Anticipation'],
      Final$Sentiment[Final$Traits=='Joy'],Final$Sentiment[Final$Traits=='Negative'],Final$Sentiment[Final$Traits=='Positive'],Final$Sentiment[Final$Traits=='Sadness'],Final$Sentiment[Final$Traits=='Surprise'], 
      Final$Sentiment[Final$Traits=="Positive_tweet"], Final$Sentiment[Final$Traits=="Negative_tweet"], Final$Sentiment[Final$Traits=="Neutral_tweet"],
      result1$High[result1$Ocean=='Extraversion'],result1$Low[result1$Ocean=='Extraversion'], 
      result1$High[result1$Ocean=='Neuroticism'],result1$Low[result1$Ocean=='Neuroticism'],
      result1$High[result1$Ocean=='Conscientiousness'],result1$Low[result1$Ocean=='Conscientiousness'],
      result1$High[result1$Ocean=='Openness'],result1$Low[result1$Ocean=='Openness'],
      result1$High[result1$Ocean=='Agreeableness'],result1$Low[result1$Ocean=='Agreeableness']
      
    )
    
    dbGetQuery(mydb, query4)
    
    dbDisconnect(mydb)
    
    
    output$plot2 <- renderPlotly({
      p2 <- plot_ly(result1, x = ~Ocean, y = ~High, type = 'bar', name = 'Likely', marker = list(color = 'rgb(49,130,189)')) %>%
        add_trace(y = ~Low, name = 'Unlikely', marker = list(color = 'rgb(204,204,204)')) %>%
        layout(xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = ""),
               margin = list(b = 100),
               barmode = 'group')
      
    })
    
    output$plot3 <- renderPlotly({
      p3  <- plot_ly(T_senti, labels = ~Names, values = ~Count, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(Count),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
        layout(
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    
    
    
    output$df <- renderTable({
      Final
    })
    
    
    
    }
    })
  })
  
  observeEvent(input$page2, {
    
    withProgress(message = 'Generating Report', value = 0, {
      
    incProgress(1/8, detail = paste("Validating Twitter ID"))
    
    t_id <- as.character(input$twitter_id)
    
    G_id <- as.numeric(userDetails()$id)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    
    
    query1 <- sprintf(
      "Select id from login_activity where G_ID = %s",G_id
    )
    
    query2 <- sprintf(
      "Select * from t_userdetails ORDER BY U_ID DESC LIMIT 1"
    )
    
  
    id <-  dbGetQuery(mydb, query1)
    trigger <- 1
    
    incProgress(1/6, detail = paste("Downloading Twitter data"))
    

  
 
    
    Final <- mainfun(t_id,id, trigger)
    
   
    incProgress(1/4, detail = paste("Performing Analysis"))
    
    user_d <- dbGetQuery(mydb, query2)
    
    u_id <- user_d$U_ID
    
    dbDisconnect(mydb)
    
    toggle(selector = "#navbar li a[data-value=Tab3]")
    
    toggle(selector = "#navbar li:nth-child(3)")
    
    updateTabsetPanel(session, "navbar", selected = "Tab3")
    
    toggle('hid5')
    
    
    output$user_detail2 <- renderUI({
      str1 <- paste("<br/><b>Name:</b>", user_d$Name)
      str2 <- paste("<b>Screen name:</b>", user_d$Screen_name)
      
     
      str5 <- paste("<b>Instagram URL:</b>", user_d$Instagram_url)
      str6 <- paste("<b>Followers:</b>", user_d$Followers)
      str7 <- paste("<b>Following:</b>", user_d$Following)
      str8 <- paste("<b>Tweets count:</b>", user_d$Tweet_count)
      str9 <- paste("<b>Account created on:</b>", user_d$Account_created)
      str10 <- paste("<b>Verified:</b>", user_d$Verified)
      
      if(nchar(user_d$Location) == 0)
      {
        str3 <- "<b>Location:</b> NOT AVAILABLE"
      }else
      {
        str3 <- paste("<b>Location:</b>", user_d$Location)
      }
      
      if(nchar(user_d$Description) == 0)
      {
        str4 <- "<b>Description:</b> NOT AVAILABLE"
      }else
      {
        str4 <- paste("<b>Description:</b>", user_d$Description)
      }
      
      HTML(paste(str1,str2,str3,str4,str5,str6,str7,str8,str9,str10, sep = '<br/><br/>'))
    }
    )
    
    
    output$img2 <- renderUI({
      HTML(paste('<img src="',user_d$Profile_image,'" style="position:absolute;top:160px;right:0px; width: 130px;">'))
    })
    
    p <- Final$Sentiment[Final$Traits=="Positive_tweet"]
    n <- Final$Sentiment[Final$Traits=="Negative_tweet"]
    ne <- Final$Sentiment[Final$Traits=="Neutral_tweet"]
    Count <- c(p,n,ne)
    Names <- c("Postive","Negative","Neutral")
    
    T_senti <- data.frame(Names,Count)
    
    
    pl1 <- Final[Final$Traits != 'Positive_tweet' & Final$Traits != 'Negative_tweet' &  Final$Traits != 'Neutral_tweet' & Final$Traits != 'Low_ext',]
    
    pl1$Traits <- factor(pl1$Traits)
    
    output$plot1.1 <- renderPlotly({
      p1 <- plot_ly(pl1,x=~Traits,y=~Sentiment,type='bar')
      
    })
    
    result1 <- get_personality(Final)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    query4 <- sprintf(
      "INSERT INTO t_result_tag (U_ID, ID, Anger , Trust , Disgust , Fear , Anticipation , Joy , Negative, Positive, Sadness, Surprise, 
      T_Positive, T_Negative, T_Neutral, High_Extraversion, Low_Extraversion, High_Neuroticism, Low_Neuroticism, High_Conscientiousness, Low_Conscientiousness, High_Openness, Low_Openness, High_Agreeableness, Low_Agreeableness )
      VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)", 
      u_id, id, Final$Sentiment[Final$Traits=='Anger'], Final$Sentiment[Final$Traits=='Trust'],Final$Sentiment[Final$Traits=='Disgust'],Final$Sentiment[Final$Traits=='Fear'],Final$Sentiment[Final$Traits=='Anticipation'],
      Final$Sentiment[Final$Traits=='Joy'],Final$Sentiment[Final$Traits=='Negative'],Final$Sentiment[Final$Traits=='Positive'],Final$Sentiment[Final$Traits=='Sadness'],Final$Sentiment[Final$Traits=='Surprise'], 
      Final$Sentiment[Final$Traits=="Positive_tweet"], Final$Sentiment[Final$Traits=="Negative_tweet"], Final$Sentiment[Final$Traits=="Neutral_tweet"],
      result1$High[result1$Ocean=='Extraversion'],result1$Low[result1$Ocean=='Extraversion'], 
      result1$High[result1$Ocean=='Neuroticism'],result1$Low[result1$Ocean=='Neuroticism'],
      result1$High[result1$Ocean=='Conscientiousness'],result1$Low[result1$Ocean=='Conscientiousness'],
      result1$High[result1$Ocean=='Openness'],result1$Low[result1$Ocean=='Openness'],
      result1$High[result1$Ocean=='Agreeableness'],result1$Low[result1$Ocean=='Agreeableness']
      
  )
    
    dbGetQuery(mydb, query4)
    
    dbDisconnect(mydb)
    
    output$plot2.1 <- renderPlotly({
      p2 <- plot_ly(result1, x = ~Ocean, y = ~High, type = 'bar', name = 'Likely', marker = list(color = 'rgb(49,130,189)')) %>%
        add_trace(y = ~Low, name = 'Unlikely', marker = list(color = 'rgb(204,204,204)')) %>%
        layout(xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = ""),
               margin = list(b = 100),
               barmode = 'group')
      
    })
    
    output$plot3.1 <- renderPlotly({
      p3  <- plot_ly(T_senti, labels = ~Names, values = ~Count, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(Count),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
        layout(
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    
    
    
    output$df_1 <- renderTable({
      Final
    })
    
    })
  })
  
  ## Workaround to avoid shinyaps.io URL problems
  observe({
    if (rv$login) {
      
      shinyjs::onclick("gauth_login-googleAuthUi",
                       shinyjs::runjs("window.location.href = 'http://127.0.0.1:6584';"))
      
    }
  
   
  })
  
  output$downloadReportHTML <- downloadHandler(
    filename = function() {
      paste('Personality_Report(Twitter_Profile).html'
      #       switch(
      #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      # )
      )
    },
    
    content = function(file) {
      
      library(rmarkdown)
      out <- render('report.rmd')
      #   switch(
      #   input$format,
      #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
      # )
     
      file.rename(out, file)
    }
  )
 
  output$download_ReportHTML <- downloadHandler(
    filename = function() {
      paste('Personality_Report(Public_Opinion).html'
            #       switch(
            #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            # )
      )
    },
    
    content = function(file) {
      
      library(rmarkdown)
      out <- render('report2.rmd')
      #   switch(
      #   input$format,
      #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
      # )
      
      file.rename(out, file)
    }
  )


output$download_ReportHTML_f <- downloadHandler(
  filename = function() {
    paste('Personality_report.html'
          #       switch(
          #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
          # )
    )
  },
  
  content = function(file) {
    
    library(rmarkdown)
    out <- render('report3.rmd')
    #   switch(
    #   input$format,
    #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
    # )
    
    file.rename(out, file)
  }
)
}

get_personality <- function(Final){
  
  h_e =( Final$Percentage[Final$Traits=='Positive']+ Final$Percentage[Final$Traits=='Joy'])/2
  l_e = Final$Percentage[Final$Traits=='Low_ext']
  h_n=(Final$Percentage[Final$Traits=='Disgust']+ Final$Percentage[Final$Traits=='Negative']+ Final$Percentage[Final$Traits=='Fear']+ Final$Percentage[Final$Traits=='Sadness']+ Final$Percentage[Final$Traits=='Anger'] )/5
  l_n=(Final$Percentage[Final$Traits=='Joy']+ Final$Percentage[Final$Traits=='Trust'])/2
  h_c=(Final$Percentage[Final$Traits=='Positive']+ Final$Percentage[Final$Traits=='Anticipation'])/2
  l_c=(Final$Percentage[Final$Traits=='Negative']+ Final$Percentage[Final$Traits=='Anger'])/2
  h_o=(Final$Percentage[Final$Traits=='Positive']+ Final$Percentage[Final$Traits=='Anticipation']+ Final$Percentage[Final$Traits=='Surprise'])/3
  l_o=(Final$Percentage[Final$Traits=='Disgust']+ Final$Percentage[Final$Traits=='Fear']+ Final$Percentage[Final$Traits=='Negative'])/3
  h_a=(Final$Percentage[Final$Traits=='Joy']+ Final$Percentage[Final$Traits=='Trust'])/2
  l_a=(Final$Percentage[Final$Traits=='Negative']+ Final$Percentage[Final$Traits=='Anger']+ Final$Percentage[Final$Traits=='Disgust'])/3
  
  High <- c(h_e,h_n,h_c,h_o,h_a)
  Low <- c(l_e,l_n,l_c,l_o,l_a)
  Ocean = c("Extraversion","Neuroticism","Conscientiousness","Openness","Agreeableness")
  
  Result <- data.frame(Ocean,High,Low)
  
  
  
  return(Result)
}

mainfun <- function(username,id,trigger){
  
  
  
  #-----------------
  #username <- "@iamsrk"
  if(trigger==0)
  {
    tweets <- get_timeline(username, 100)
    
    
    tag <- ""
    user_data <- users_data(tweets)
    
    phrase <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", user_data$description)
    user_data$description <- gsub("U00..", "", phrase)
    
    user_data$account_created_at <- substr(user_data$account_created_at, 1,19)
    date <- substr(Sys.time(), 1,19)
    
    mydb = dbConnect(MySQL(), user='root', password='Kishan@19972', dbname='persona', host='127.0.0.1', port=3306, DBMSencoding="UTF-8")
    query <- sprintf(
      "INSERT INTO t_userdetails (ID, Date, Name, Screen_name, Location, Description, Instagram_url, Followers, Following, Tweet_count, Account_created, Verified, Profile_image)
      VALUES (%s,'%s','%s','%s','%s','%s','%s',%s,%s,%s,'%s','%s','%s')", id, date,user_data$name, user_data$screen_name, user_data$location, user_data$description, user_data$url,user_data$followers_count, user_data$friends_count, user_data$statuses_count, user_data$account_created_at, user_data$verified, user_data$profile_image_url
      
      
    )
    
    dbGetQuery(mydb, query)
    
    dbDisconnect(mydb)
    
  }
  else
  {
    tweets <- search_tweets(username, n = 100, type = "recent", include_rts = TRUE, retryonratelimit = TRUE)
    tag <- "tag"
  }
  
  
  #tweetsDF <- as.data.frame(tweets)
  
  # path <- "D:/Study/sem 8/Results/"
  # dir.create(paste(path,username,sep =""))
  # file=paste(path,username,"/",tag,username,sep = "")
  # save_as_csv(tweets, file_name = file, prepend_ids = TRUE, na = "",fileEncoding = "UTF-8")
  # 
  
  word_count=0
  
  
  lib <- get_sentiments("nrc")
  i <- 1
  
  
  text <- c(tweets$text)
  
  rm_words <- function(string, words) {
    stopifnot(is.character(string), is.character(words))
    spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
    vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
  }
  
  
  for(i in 1:length(text))
  {
    
    
    phrase_clean <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", text[i])
    text[i] <- gsub("U00..", "", phrase_clean)
    text[i] <- rm_words(text[i], tm::stopwords("en"))
    
  }
  data <- data_frame(line= 1:length(text), text = text)
  data <- data %>% unnest_tokens(word, text)
  
  
  Anger <- Trust <- Disgust <- Fear <- Anticipation <- Joy <- Negative <- Positive <- Sadness <- Surprise <- T_senti <-  c(0)
  i <- 1
  for(i in 1:length(text))
  {
    t <- data[data$line==i,]
    
    anger <- trust <- disgust <- fear <- anticipation <- joy <- negative <- positive <- sadness <- surprise <- p <- n <-  0
    # remove Stop Words
    # data(stop_words)
    # t <- t %>% anti_join(stop_words)
    
    
    
    
    if(nrow(t)!=0)
    {
      w <- 1
      for(w in 1:nrow(t))
      {
        
        wm=tolower(t$word[w])
        
        
        senti <- lib$sentiment[lib$word==wm | lib$word==substr(wm,1,nchar(wm)-1) | lib$word==substr(wm,1,nchar(wm)-2)]
        
        
        if(length(senti)!=0)
        {
          word_count=word_count+1 
          for(k in 1:length(senti))
          {
            
            if(senti[k]=="trust")
            {
              trust= trust+1
            } 
            else if(senti[k]=="anger")
            {
              anger=anger+1
            }
            else if(senti[k]=="disgust")
            {
              disgust=disgust+1
            }
            else if(senti[k]=="fear")
            {
              fear=fear+1
            }
            else if(senti[k]=="joy")
            {
              joy=joy+1
            }
            else if(senti[k]=="anticipation")
            {
              anticipation=anticipation+1 
            }
            else if(senti[k]=="negative")
            {
              negative=negative+1
            }
            else if(senti[k]=="positive")
            {
              positive=positive+1
            }
            else if(senti[k]=="sadness")
            {
              sadness=sadness+1
            }
            else if(senti[k]=="surprise")
            {
              surprise=surprise+1
            }
            else
            {
              
            }
          }
        }   
        
      }
    }
    
    Trust[i] <- trust
    Anger[i] <- anger
    Disgust[i] <- disgust 
    Fear[i] <- fear
    Anticipation[i] <- anticipation 
    Joy[i] <- joy
    Negative[i] <- negative
    Positive[i] <- positive
    Sadness[i] <- sadness
    Surprise[i] <- surprise
    
    p <- (trust+anticipation+positive+surprise)
    n <- (anger+fear+negative+sadness+disgust)
    
    if(p>n)
    {
      T_senti[i] <- "Positive"
    }
    else if(n>p)
    {
      T_senti[i] <- "Negative"
    }
    else
    {
      T_senti[i] <- "Neutral"
    }
    
  }
  
  Trust[i+1] <- sum(Trust)
  Anger[i+1] <- sum(Anger)
  Disgust[i+1] <- sum(Disgust) 
  Fear[i+1] <- sum(Fear)
  Anticipation[i+1] <- sum(Anticipation) 
  Joy[i+1] <- sum(Joy)
  Negative[i+1] <- sum(Negative)
  Positive[i+1] <- sum(Positive)
  Sadness[i+1] <- sum(Sadness)
  Surprise[i+1] <- sum(Surprise)
  text[i+1] <- "Total"
  T_senti[i+1] <- "0"
  
  T_positive <- length(T_senti[T_senti=='Positive'])
  T_negative <- length(T_senti[T_senti=='Negative'])
  T_neutral <- length(T_senti[T_senti=='Neutral'])
  word_count <- (word_count*75)/100
  
  allsenti <- data.frame(text ,Anger , Trust , Disgust , Fear , Anticipation , Joy , Negative, Positive, Sadness, Surprise, T_senti )
  
  # write.csv(allsenti, file=paste(path,username,"/",tag,username,"_Result.csv",sep=""))
  
  Sentiment <- c( Anger[i+1],Trust[i+1],Disgust[i+1],Fear[i+1],Anticipation[i+1], Joy[i+1], Negative[i+1],Positive[i+1],Sadness[i+1], Surprise[i+1], T_positive, T_negative, T_neutral, length(text))
  
  Traits <- c("Anger" , "Trust" , "Disgust" , "Fear" , "Anticipation" , "Joy" , "Negative", "Positive", "Sadness", "Surprise", "Positive_tweet", "Negative_tweet", "Neutral_tweet", "Low_ext")
  
  Percentage <- c( (Anger[i+1]*100)/word_count,(Trust[i+1]*100)/word_count,(Disgust[i+1]*100)/word_count,(Fear[i+1]*100)/word_count,(Anticipation[i+1]*100)/word_count, (Joy[i+1]*100)/word_count, (Negative[i+1]*100)/word_count,(Positive[i+1]*100)/word_count,(Sadness[i+1]*100)/word_count, (Surprise[i+1]*100)/word_count, (T_positive*100)/3200,(T_negative*100)/3200,(T_neutral*100)/3200, 100-((length(text)*100)/3200))
  
  Final <- data.frame(Traits, Sentiment, Percentage)
  
  return(Final)
  # barplot(t(as.matrix(senti)), names.arg = name, xlab="categories", ylab="Count", color= "blue", beside = TRUE)
  
  
}

shinyApp(ui = ui, server = server)