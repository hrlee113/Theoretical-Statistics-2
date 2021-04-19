##################################
####### Project1_Insurance #######
##################################

# Library load
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)

# Customizing Theme
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Helvetica"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(95, 168, 237)"
  ,headerButtonBackColor = "rgb(100, 179, 244)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(95, 168, 237)"
    ,colorMiddle = "rgb(163,214,245)"
    ,colorEnd = "rgb(194,229,156)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "4px 4px 4px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(248,248,248)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "rgb(248,248,248)"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "5px 7px 7px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(230,230,230)"
  ,sidebarSearchIconColor = "rgb(75,75,75)"
  ,sidebarSearchBorderColor = "rgb(163,214,245)"
  
  ,sidebarTabTextColor = "rgb(75,75,75)"
  ,sidebarTabTextSize = 18
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(163,214,245)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(95, 168, 237)"
    ,colorMiddle = "rgb(147, 208, 245)"
    ,colorEnd = "rgb(163,214,245)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(156, 229, 185)"
    ,colorMiddle = "rgb(175, 229, 156)"
    ,colorEnd = "rgb(194,229,156)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(250,250,250)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 20
  ,boxDefaultColor = "rgb(105, 165, 131)"
  ,boxPrimaryColor = "rgb(41,64,82)"
  ,boxInfoColor = "rgb(255,255,255)"
  ,boxSuccessColor = "rgb(21,25,80)"
  ,boxWarningColor = "rgb(21,25,80)"
  ,boxDangerColor = "rgb(21,25,80)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 12
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

# Set UI

dashboardPage(
  dashboardHeader(title = 'Car Insurance', titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarSearchForm(textId = 'infotext', buttonId = 'infobutton', label='Search'),
    sidebarMenu(
      menuItem('Calculator', tabName = 'tab1', icon=icon('fas fa-car')),
      menuItem('Information', tabName = 'tab2', icon=icon('fas fa-chart-line'))
    )
  ),
  
  dashboardBody(
    customTheme, ## customized theme
    
    tags$head(tags$style(HTML(
      '.main-header label{font-family: BentonSans Book;}',
      '.main-sidebar {font-size : 18px;}',
      '.box {font-size : 15px;}',
      '.small-box {width: 150px}'
    ))),
    
    tabItems(
      tabItem(tabName='tab1',
              fluidRow(        
                column(width=7,
                       box(width=NULL, height=550, title="Driver's Information",
                           fluidRow(
                             column(width=4,
                                    radioButtons('kilometres', 'Mileage',
                                                 choices=c('1. less than 1000km' = 1, '2. 1000km-15000km' = 2, '3. 15000km-20000km' = 3, '4. 20000km-25000km' = 4, '5. more than 25000km' = 5)),
                                    radioButtons('zone', 'Driving Area', 
                                                 choices=c('1. Seoul & Gyeonggi-do' = 1, '2. Gyeongsangnam-do' = 2, '3. Gyeongsangbuk-do' = 3, '4. Jeolla-do' = 4, 
                                                           '5. Chungcheong-do' = 5, '6. Gangwon-do' = 6, '7. Jeju-island' = 7))
                             ),
                             column(width=3,
                                    radioButtons('make', 'Car',
                                                 choices = c('1. Volvo' = 1, '2. VW' = 2, '3. Kia' = 3, '4. Mercedes' = 4, '5. BMW' = 5,'6. Toyota' = 6, '7. Audi' = 7, '8. Skoda' = 8, '9. Renault' = 9))
                             ),
                             column(width=5,
                                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: rgb(178, 229, 156)}")),
                                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: rgb(178, 229, 156)}")),
                                    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: rgb(178, 229, 156)}")),
                                    sliderInput('bonus', 'Bonus (= No accident years + 1 year)', 1, 7, 1, step=1),
                                    sliderInput('deductable', 'Deductable', 0, 10000, 0, step=10),
                                    sliderInput('limit', 'Limit', 0, 15000, 10000, step=10)    
                             )
                           ),
                           fluidRow(
                             
                           )
                       ),
                ),
                column(width=5,
                       box(title='Premium', width=16, height=550,
                           fluidRow(
                             column(width=12, valueBoxOutput('kilometres1'), valueBoxOutput('make1')),
                             column(width=12, valueBoxOutput('zone1'), valueBoxOutput('bonus1'))
                           ),
                           fluidRow(
                             column(width=12, h4(textOutput("premium1")))
                           )
                       )
                )
              ),
              fluidRow(
                column(width=12,
                       box(width=4, height=150, title='How much insurance do you want?',
                           column(width=6, textInput('limit1', 'Lower Bound')), 
                           column(width=6, textInput('limit2', 'Upper Bound'))
                           ),
                       column(width=8, DT::dataTableOutput('table2')
                       ),
                )
              )
      ),
      tabItem(tabName = 'tab2',
              fluidRow(
                column(6,
                       tabsetPanel(
                         tabPanel(title='Mean Insured',
                                  plotOutput('plot1')
                                  ),
                         tabPanel(title='Mean Payment',
                                  plotOutput('plot2')
                                  ),
                         tabPanel(title='Mean Claim',
                                  plotOutput('plot3')
                                  )
                       )
                ),
                column(6,
                       box(title='Table Information by Area', width=12, tableOutput('table1'))
                )
             )
      )
    )
  )
)
