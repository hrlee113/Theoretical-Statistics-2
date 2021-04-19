####### Project2_Optimal Portfolio #######

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(DT)
library(stringi)

customTheme <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Helvetica"
    ,appFontColor = "rgb(0,0,0)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = 'rgb(250,250,250)'
    
    ### header
    ,logoBackColor = "rgba(43,63,107,1)"
    
    
    ,headerButtonBackColor = "rgba(43,63,107,1)"
    ,headerButtonIconColor = "rgba(248,155,108,1)"
    ,headerButtonBackColorHover = "rgb(210,210,210)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgba(43,63,107,1)"
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "2px 2px 2px"
    
    ### sidebar
    ,sidebarBackColor = "#D7E2EA"
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "#D7E2EA"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = "3px 3px 5px"
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
        ,colorStart = "rgb(194,229,156)"
        ,colorMiddle = "rgb(61, 153, 190)"
        ,colorEnd = "rgb(58, 181, 176)"
        ,colorStartPos = 0
        ,colorMiddlePos = 70
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
    ,boxBackColor = "rgb(143,188,219)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 20
    ,boxDefaultColor = "rgb(143,188,219)"
    ,boxPrimaryColor = "rgb(245,245,245)"
    ,boxInfoColor = "rgb(143,188,219)"
    ,boxSuccessColor = "rgb(143,188,219)"
    ,boxWarningColor = "rgb(143,188,219)"
    ,boxDangerColor = "rgb(143,188,219)"
    
    ,tabBoxTabColor = "rgba(248,155,108,1)"
    ,tabBoxTabTextSize = 18
    ,tabBoxTabTextColor = 'FFC8A2'
    ,tabBoxTabTextColorSelected = 'FFC8A2'
    ,tabBoxBackColor = "rgb(238,238,238)"
    ,tabBoxHighlightColor = "rgba(248,155,108,1)"
    ,tabBoxBorderRadius = 3
    
    ### inputs
    ,buttonBackColor = "rgba(248,155,108,1)"
    ,buttonTextColor = "rgb(255,255,255)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(245,245,245)"
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


dashboardPage(
    dashboardHeader(title = 'Optimal Portfolio', titleWidth = 300),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: rgba(248,155,108,1)}")),
            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: rgba(248,155,108,1)}")),
            tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: rgba(248,155,108,1)}")),
            textInput("year","Start year", "2013"),
            textInput("V0","Early Stage Investment (thousand)", "10"), #V0
            sliderInput("theta0", "Ratio of Fixed Deposit (%)", 0, 100,value=20, step=1),
            sliderInput("istar", "Target Return (%)", 0, 100,value=40, step=1),
            textInput("M","Investment Period (per year)", "5"),
            textInput("N","Period of Validation data (per year)", "5"),
            actionButton("go", label = "Go!")
        )
    ),
    
    dashboardBody(
        
        customTheme,
        
        tags$head(tags$style(HTML(
            '.main-header label{font-family: BentonSans Book;}',
            '.main-sidebar {font-size : 18px;}',
            '.box {font-size : 15px;}',
            '.small-box {width: 150px}'
        ))),
        fluidRow(
            tabBox(
                title=NULL, width=12, id='tabset1', height="800px",
                tabPanel(id='result1', 
                         'Stock Information',
                         column(12,
                                fluidRow(
                                         column(6,
                                                gradientBox(title = div("Companies", style="font-size:18px"), width=12, gradientColor = "rgb(143,188,219)", 
                                                            icon= 'fas fa-building', height=NULL,
                                                            footer= checkboxGroupInput('company1', NULL, c('KAKAO' = 'KAKAO', 'KOGAS' = 'KOGAS', 'KT&G' = 'KT&G', 'NAVER' = 'NAVER',
                                                                                                           'SAMSUNG ELECTRONICS' = 'SAMSUNG ELECTRONICS')), 'Choose companies that you want'),
                                                gradientBox(title = div("Distribution", style="font-size:18px"), width=12, gradientColor = "rgb(143,188,219)", 
                                                            icon= 'fas fa-chart-area', height=NULL,
                                                            footer= tableOutput('table1'), 'See distributions that you want')            
                                         ),
                                         column(6,
                                                gradientBox(title = div("Stock Price", style="font-size:18px"), width=12, gradientColor = "rgb(143,188,219)", 
                                                            icon= 'fas fa-chart-line', height=NULL,
                                                            footer= plotOutput('plot1'), 'Time series plot')
                                         )
                                         
                                )
                        )
                ),
                tabPanel(id='result2',
                         'Optimal Portfolio',
                         column(12,
                                fluidRow(
                                    column(5, 
                                           box(title = div('Fixed Ratio', style="font-size:18px"), width=NULL, 
                                                       height='480px',
                                                       column(6,
                                                              textInput('f_kakao', 'KAKAO', 0),
                                                              textInput('f_kogas', 'KOGAS', 0),
                                                              textInput('f_kt', 'KT&G', 0),
                                                              textInput('f_naver', 'NAVER', 0),
                                                              textInput('f_sam', 'SAMSUNG', 0)
                                                       )
                                           )
                                    ),
                                    column(7,
                                           gradientBox(title = div("Optimal Portfolio", style="font-size:18px"), width=12, gradientColor = "rgb(143,188,219)", 
                                                       icon= 'fas fa-chart-area', height='450px',
                                                       footer= column(12,
                                                                      tableOutput('thetas'),
                                                                      plotOutput('plot2')
                                                       )
                                           )
                                )
                                
                        )
                    
                )
            )
        )
            
            
    )
))

