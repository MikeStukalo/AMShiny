library(shinydashboard)
library(DT)
library(shiny)
library(shinyWidgets)


shinyUI(dashboardPage(skin = "black" , 
  dashboardHeader(title = "Portfolio Allocation Demo"),
  
  dashboardSidebar(
    sidebarUserPanel("Mike Stukalo", image = "avatar.png"),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("book")),
      menuItem("Theory", tabName = "theory", icon = icon("graduation-cap"),
               menuSubItem("Risk/Return Ratio", tabName = "theory_1"),
               menuSubItem("Optimal Portfolio", tabName = "theory_2"),
               menuSubItem("Performance Measures", tabName = "theory_3")
               ),
      menuItem("Backtest", tabName = "backtest", icon = icon("line-chart"),
               menuSubItem("Your Allocation", tabName = "user_port"),
               menuSubItem("Allocation Comparison", tabName = "opt_port")
               
               ),
      menuItem("The Author", tabName = "author", icon = icon("user")),
      menuItem("Disclaimers", tabName = "discl", icon = icon("exclamation-triangle")))
      
  ),
  
  dashboardBody(
    tabItems(

      
            ####ABOUT PAGE
            tabItem(tabName = "about", fluidRow(column(6, htmlOutput("abt")))),
      
            ##### My CV Page
            tabItem(tabName = "author", 
                    fluidRow(column(3,h3("About Me"))),
                    fluidRow(column(1, div(img(src="avatar.png", height=120))),
                             column(1,div(
                                    fluidRow(actionButton(inputId="li", label = "", icon = icon("linkedin"),
                                                          onclick ="location.href='https://www.linkedin.com/in/mstukalo/';")),
                                    fluidRow(actionButton(inputId="gh", label = "", icon = icon("github"),
                                                          onclick ="location.href='https://github.com/mikestukalo';")), br(),
                                    fluidRow(actionButton(inputId="cv", label = "Resume", icon = icon("file"),
                                                          onclick ="location.href='https://www.dropbox.com/s/gbfdkcp9fe0wx5z/Stukalo_resume.pdf?dl=0';"))
                                        )
                                    )
                            ),
                    fluidRow(column(6, 
                                    div(br(),br(),
                                      p("Dr. Mikhail Stukalo has over 15 years of experience in financial markets."),
                                      p("Prior to obtaining his Doctoral degree Mikhail Stukalo was a Director and a Partner at 
                                        Svarog Capital, a Russian private equity and venture capital fund with over $250 million AuM. 
                                        As a part of his involvement with Svarog Capital, Dr. Stukalo supervised the fund’s portfolio 
                                        investments by sitting on the Board of Directors of various companies, ranging from start-ups 
                                        to multibillion-dollar enterprises. Also, he made a number of successful private investments as 
                                        an early-stage investor in start-ups. His prior career included a number of managerial positions 
                                        in investment banking and M&A advisory companies."),
                                      p("Currently, Mikhail is a Data Science Fellow with NYC Data Science Academy"),
                                      p("Dr. Stukalo earned his MBA degree from London Business School and a Doctor of Philosophy in 
                                        Business degree from Georgia State University. He is a CFA and a CAIA charter holder. 
                                        He also holds a Certificate in Quantitative Finance (CQF) from Fitch Learning, London.")
                                      
                                      
                                    )))
                    ),
            
            
            ##### Legal Disclaimer Page 
            tabItem(tabName = "discl", div(htmlOutput("disclaimer"))),
            
             
            ####Risk/Return Page
            tabItem(tabName = "theory_1", 
                     fluidPage(h1("Risk/Return Ratio"),
                               p("In 1952 Harry Markowitz suggested that assets should be evaluated based on their risk/return ratio.
                                  For the purposes of this App I look at the asset returns measured by corresponding indices in 1Q2000 
                                 - 3Q2018. "),
                               p("The assets are:"),
                               p(em("Equities:")),
                               tags$div(tags$ul(
                                 tags$li("Russell 2000"),
                                 tags$li("MSCI Europian Stock Index"),
                                 tags$li("MSCI Emerging Market Stock Index"))
                                 ),
                               p(em("Bonds:")),
                               tags$div(tags$ul(
                                 tags$li("Barclays US Treasury Total Return Index"),
                                 tags$li("Barclays US Corporate Bonds Total Return Index")
                                 )
                               ),
                               p(em("Real Estate:")),
                               tags$div(tags$ul(
                                 tags$li("Dow Jones Real Estate Index"))
                               ),
                               tabsetPanel(
                                 tabPanel("Whole Period", br(), plotlyOutput("graph1")),
                                 tabPanel("By Years",  plotlyOutput("graph2")),
                                 tabPanel("Compound Return",  plotlyOutput("graph3"))
                               )
                               )
                     ),
             
            #####Optimal potrfolio page
             
            tabItem(tabName = "theory_2", 
                     fluidPage(fluidRow(
                       column(6,h1("Optimal portfolio"),
                               p("Asset returns are not perferctly correlated. Therefor we can combine assets into portfolios, and harverst 
                                 the results of diversification."),
                               p("However, diversification is not limitless. For each expected risk there will be a portfolio with 
                                 a maximum achievable risk.The graph below shows risk/return profiles of simulated portfolios (gray) and 
                                 a line (blue) depicting portfolios offering highest return for a given risk."),
                               p("In Harry Markowitz (1952) framework, such line is called the Efficient Frontier. However, Markowitz' theory 
                                 assumes that investors hold long-short portfolio. In our analysis we limit ourselves to long-only portfolios, 
                                 as it is the type retail investors usually hold. Therefore, we will refer to portfolios on this line as
                                 'Optimal Portfolios', and the line itself as the 'Optimal Line'."),
                               br(),
                               plotlyOutput("graph4")
                               )))
                     ),
            
            tabItem(tabName = "theory_3", 
                    fluidRow(column(8,div(htmlOutput("measures"))))
            ),
            
            
            
            #####  HERE IS WHERE FUN BEGINS
            #####
            
            #### Your allocation Page
            tabItem(tabName = "user_port", 
                     fluidRow(div(column(6, h4("Select Portfolio Allocation:", align = "center")),
                              column(3, h4("Select Rebalance Schedule:", align = "left")),
                              column(3, h4("Allocation", align = "center")))
                              ),
                     fluidRow(column(3,
                                      uiOutput("p1ui"),
                                      uiOutput("p2ui"),
                                      uiOutput("p3ui")),
                              column(3,
                                     uiOutput("p4ui"),
                                     uiOutput("p5ui"),
                                     uiOutput("p6ui")),
                              column(3,
                                     fluidRow(
                                     radioButtons(inputId="rebalance",
                                                  label=NULL, 
                                                  choices=c("Monthly","Quarterly", "Annually", "Never"),
                                                  selected = "Never")),
                                     fluidRow(br(),br(),br(),
                                       div(actionBttn("go", label = "Backtest", color = "primary"), 
                                                         align = "left"))
                                     ),
                              column(3,
                                     div(plotlyOutput("graph5"), align = "center", style = "height:250px"))),
                     fluidRow(column(12,
                                     div(sliderTextInput(
                                       inputId = "date_range", label = h4("Time interval:"), width = "80%",
                                       choices = date_choices, selected = range(date_choices),
                                       grid = TRUE, dragRange = FALSE
                                     ), align = "center"))
                              ),
                     fluidRow(column(6, h4("Compound Return", align="center")),
                              column(6, h4("Performance Measures", align="center"))),
                     fluidRow(column(6, div(plotlyOutput("graph6"), align="center")),
                              column(6, div(tableOutput("bt_table1"), align="center"))
                              )
             ),
            
            ####Allocation Comparison Page
            tabItem(tabName = "opt_port", 
                    fluidRow(column(4, h4("Your Allocation", align="center")),
                             column(4, h4("Same Return", align="center")),
                             column(4, h4("Same Risk", align="center"))
                             ),
                    fluidRow(column(4, div(plotlyOutput("graph7"), align="center")),
                             column(4, div(plotlyOutput("graph8"), align="center")),
                             column(4, div(plotlyOutput("graph9"), align="center"))
                             ),
                    fluidRow(column(6, h4("Compound Return", align = "center")),
                             column(6, h4("Performance Measures", align="center"))
                            ),
                    fluidRow(column(6, div(plotlyOutput("graph10"), allign = "center")),
                             column(6, div(br(),tableOutput("bt_table2"), align="center"))
                             )
                    )
            
            
    )
    
)
))
