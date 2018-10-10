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
    tabItems(tabItem(tabName = "about", h2("Some description of the App")),
             
             tabItem(tabName = "theory_1", 
                     fluidPage(h1("Risk/Return Ratio"),
                               p("In 1952 Harry Markowitz suggested that assets should be evaluated based on their risk/return ratio.
                                  For the purposes of this App I look at the asset returns measured by corresponding indices in 1Q2000 
                                 - 3Q2018. <br> The assets are:"),
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
             
             tabItem(tabName = "theory_2", 
                     fluidPage(h1("Optimal portfolio"),
                               p("Asset returns are not perferctly correlated. Therefor we can combine assets into portfolios, and harverst 
                                 the results of diversification."),
                               p("However, diversification is not limitless. For each expected risk there will be a portfolio with 
                                 a maximum achievable risk. The graph below shows risk/return profiles of simulated portfolios (gray) and 
                                 a line (blue) depicting portfolios offering highest return for a given risk."),
                               p("In Harry Markowitz (1952) framework, such line is called the Efficient Frontier. However, Markowitz' theory 
                                 assumes that investors hold long-short portfolio. In our analysis we limit ourselves to long-only portfolios, 
                                 as it is the type retail investors usually hold. Therefore, we will refer to portfolios on this line as
                                 'Optimal Portfolios', and the line itself as the 'Optimal Line'."),
                               br(),
                               plotlyOutput("graph4")
                               )
                     ),
             tabItem(tabName = "author", h2("My CV")),
             tabItem(tabName = "discl", h2("Legal Disclaimer")),
             tabItem(tabName = "user_port", 
                     fluidRow(column(6, h4("Select Portfolio Allocation:")),
                              column(3, h4("Select Rebalance Schedule:")),
                              column(3, h4("Allocation"))
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
                                     h1("Rebalance")),
                              column(3,
                                     plotlyOutput("graph5"))),
                     fluidRow(column(2, h1()),
                              column(10,
                                     sliderTextInput(
                                       inputId = "date_range", label = h4("Time interval:"), width = "80%",
                                       choices = date_choices, selected = range(date_choices),
                                       grid = TRUE, dragRange = FALSE
                                     # ),
                                     # verbatimTextOutput(outputId = "res")
                                     )
                              )
                     )
             )
    )
    
)
))
