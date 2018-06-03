## ui.R ##
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
        dashboardHeader(title = "Bitcoin" ),
        
        ## Sidebar content
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Price dashboard", tabName = "bitcoin_dashboard", icon = icon("dashboard")),
                        menuItem("Mining dashboard", tabName = "bitcoin_pools_dashboard", icon = icon("dashboard")),
                        menuItem("Blocks dashboard", tabName = "bitcoin_blocks_dashboard", icon = icon("dashboard")),
                        menuItem("Mempool dashboard", tabName = "bitcoin_mempool_dashboard", icon = icon("dashboard")),
                        menuItem("Wallet transactions", tabName = "wallet", icon = icon("dashboard")),
                        menuItem("Credits", tabName = "credits", icon = icon("th"))
                )
        ),
        dashboardBody(
                tabItems(
                        # dashboard content
                        tabItem(tabName = "bitcoin_dashboard",
                                fluidRow(
                                        infoBoxOutput("dateBox"),
                                        infoBoxOutput("speedBox"),
                                        infoBoxOutput("pulseBox")
                                ),
                                fluidRow(
                                        box(title = "Monthly activity", status = "primary", solidHeader = TRUE,
                                            plotOutput("mp", height = 300) ),
                                        box(title = "Monthly histogram", status = "primary", solidHeader = TRUE,
                                            plotOutput("mh", height = 300) ) 
                                ),
                                fluidRow(
                                   box(title = "Weekly activity", status = "primary", solidHeader = TRUE,
                                       plotOutput("wp", height = 300) ),
                                   box(title = "Weekly histogram", status = "primary", solidHeader = TRUE,
                                       plotOutput("wh", height = 300) )
                                ),
                                fluidRow(
                                        box(title = "Daily activity", status = "primary", solidHeader = TRUE,
                                            plotOutput("dp", height = 300) ),
                                        box(title = "Daily histogram", status = "primary", solidHeader = TRUE,
                                            plotOutput("dh", height = 300) )
                                )
                                
                        ),
                        # dashboard content
                        tabItem(tabName = "bitcoin_pools_dashboard",
                                fluidRow(
                                   box(title = "Monthly activity", status = "primary", solidHeader = TRUE,
                                       plotOutput("b30", height = 300) ),
                                   box(title = "Monthly histogram", status = "primary", solidHeader = TRUE,
                                       plotOutput("bh30", height = 300) )
                                ),
                                fluidRow(
                                   box(title = "Weekly activity", status = "primary", solidHeader = TRUE,
                                       plotOutput("b7", height = 300) ),
                                   box(title = "Weekly histogram", status = "primary", solidHeader = TRUE,
                                       plotOutput("bh7", height = 300) )
                                ),
                                fluidRow(
                                   box(title = "Daily activity", status = "primary", solidHeader = TRUE,
                                       plotOutput("b1", height = 300) ),
                                   box(title = "Daily histogram", status = "primary", solidHeader = TRUE,
                                       plotOutput("bh1", height = 300) )
                                )
                                
                        ),
                        # dashboard content
                        tabItem(tabName = "bitcoin_blocks_dashboard",
                                fluidRow(
                                   box(title = "Monthly free space", status = "primary", solidHeader = TRUE,
                                       plotOutput("f30", height = 300) ),
                                   box(title = "Monthly speed", status = "primary", solidHeader = TRUE,
                                       plotOutput("s30", height = 300) )
                                ),
                                fluidRow(
                                   box(title = "Weekly free space", status = "primary", solidHeader = TRUE,
                                       plotOutput("f7", height = 300) ),
                                   box(title = "Weekly speed", status = "primary", solidHeader = TRUE,
                                       plotOutput("s7", height = 300) )
                                ),
                                fluidRow(
                                   box(title = "Daily free space", status = "primary", solidHeader = TRUE,
                                       plotOutput("f1", height = 300) ),
                                   box(title = "Daily speed", status = "primary", solidHeader = TRUE,
                                       plotOutput("s1", height = 300) )
                                )
                                
                        ),
                        # dashboard content
                        tabItem(tabName = "bitcoin_mempool_dashboard",
                                fluidRow(
                                   box(title = "Fees distribution in mempool", status = "primary", solidHeader = TRUE,
                                       plotOutput("fee", height = 300) ),
                                   box(title = "Segregated Witness impact", status = "primary", solidHeader = TRUE,
                                       plotOutput("feesw", height = 300) )
                                ),
                                fluidRow(
                                   box(title = "Ages distribution in mempool (last hour)", status = "primary", solidHeader = TRUE,
                                       plotOutput("adis", height = 300) ),
                                   box(title = "Size distribution in mempool (last hour)", status = "primary", solidHeader = TRUE,
                                       plotOutput("sdis", height = 300) )
                                )
                                
                        ),
                        # wallet content
                        tabItem(tabName = "wallet",
                                fluidRow(
                                   box(title = "Wallet", status = "primary", solidHeader = TRUE, width = 12,
                                       textInput("wallet", "Wallet", "1HB5XMLmzFVj8ALj6mfBsbifRoD4miY36v"),
                                       dataTableOutput('wallet'))
                                ),
                                fluidRow(
                                   box(title = "Transactions", status = "primary", solidHeader = TRUE, width = 12,
                                       plotOutput("pl", height = 300),
                                       dataTableOutput('transactions'))
 
                                )
                                
                        ),
                        # credits content
                        tabItem(tabName = "credits",
                                tags$a(
                                   href="https://github.com/valerio-vaccaro/Bitcoin-dashboard/",
                                   tags$img(
                                      style="position: absolute; top: 10; right: 0; border: 0;",
                                      src="https://s3.amazonaws.com/github/ribbons/forkme_right_red_aa0000.png",
                                      alt="Fork me on GitHub"
                                   )
                                ),
                                h2("Bitcoin activity"),
                                "Bitcoin activity is a dashboard developed in R and shiny and able to show:", br(),
                                "- the value of Bitcoin in EUR and USD in the last day, last week and in last month", br(),
                                "- the mining of Bitcoin in the last 24 hours and in last 10 days", br(),
                                "- the performances of Bitcoin in terms of blocks free space and delay in the last 24 hours and in last 10 days", br(),
                                "- the performance of mempool in last hours and the prices of transactions still present in mempool", br(),
                                "- the transactions connected with one bitcoin wallet address.", br(), 
                                br(),"The dataset is generate using free API from btc.com, blockchair.com and coinmarketcap.com", br(),
                                br(),"All the code is available at", a("https://github.com/valerio-vaccaro/Bitcoin-dashboard"), br(),
                                br(),"Copyright (c) 2017-2018 Valerio Vaccaro", a("http://www.valeriovaccaro.it"), br(),
                                br(),"Permission is hereby granted, free of charge, to any person obtaining a copy", br(),
                                "of this software and associated documentation files (the \"Software\"), to deal", br(),
                                "in the Software without restriction, including without limitation the rights", br(),
                                "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell", br(),
                                "copies of the Software, and to permit persons to whom the Software is", br(),
                                "furnished to do so, subject to the following conditions:", br(),
                                br(),"The above copyright notice and this permission notice shall be included in all", br(),
                                "copies or substantial portions of the Software.", br(),
                                br(),"THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR", br(),
                                "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,", br(),
                                "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE", br(),
                                "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER", br(),
                                "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,", br(),
                                "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE", br(),
                                "SOFTWARE."
                        )
                )
        )
)

