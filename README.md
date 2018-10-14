# AMShiny
Shiny Project for Illustrating Asset Management Principles
The App is running from 
https://mikestukalo.shinyapps.io/AMShinyApp/


## File structure
/ - in root directory there are standard Shiny App files (ui.R, server.R, global.R)

/html - static html pages

/func - helper functions

  /func/am_helper.R - a collection of functions that calculate various portfolio outputs
  
  /func/shiny_helper - a collection of functions that calculate certain outputs for server.R
  
/data - input data in .csv format


## Usage
The user receive interactive information through the BackTest section. She can select a range of dates for which the 
backtesting is performed, portfolio asset weights, and rebalancing schedule. The App shows the key performace measures of 
the selected portfolio and comparison to pure equity and a 60/40 portfolio.

On 'Allocation Comparison' tab the user can compare the portfolio to optimal portfolios with the same risk or same return,
and see the structure of the optimal portfolios. 

Behind the scenes there is a penalizing optimization algorithm that takes the return and risk of the user portfolio and finds 
optimal portfolios with corresponding risk or return. 


