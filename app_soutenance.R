#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#-------ASSESSING THE EFFECTS OF UNCONVENTIONAL MONETARY POLICY IN THE EUROSYSTEM------------
#--------------------------------DURING THE COVID19 CRISIS-----------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------Carles CERDA VILA, M1 EKAP 2021-2022------------------------------
#---------------------------------------Soutenance-------------------------------------------
#--------------------------------------------------------------------------------------------

#libraries
library(readxl)
library(readr)
library(shiny)
library(shinydashboard)
library(fontawesome)
library(mathjaxr)
library(TSstudio)
library(plotly)

#data for graphs

#APP
app<-read_xlsx("C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/APP_hist_graph.xlsx", col_names = TRUE)
app<-app[, -c(2:6)]

#SVAR variables

#Inflation rate
hicpQ<-read_csv("C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/hicp_Q.csv", col_names = FALSE)
names(hicpQ)<-c("Date","HICP","Normal Value","NA","HICP Q")
hicpQ<-hicpQ[, -c(1:4)]
str(hicpQ)
hicpQ<-na.omit(hicpQ)
hicpQ<-hicpQ[-1,]
hicpQ[]<-rev(hicpQ$`HICP Q`)
piQ<-ts(data = hicpQ, start = c(2000,01), frequency = 4)

#GDP growth rate
gdp<-read_csv("C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/gdp.csv", col_names = FALSE)
names(gdp)<-c("Date","GDP","Normal Value")
gdp<-gdp[, -c(1,3)]
str(gdp)
gdp[]<-rev(gdp$GDP)
yy<-ts(data = gdp, start = c(2000,01), frequency = 4)


#LT interest rate
eltrQ<-read_csv("C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/eLTR_Q.csv", col_names = FALSE)
names(eltrQ)<-c("Date","LT interest rate","Normal Value","LT interest rate Q")
eltrQ<-eltrQ[, -c(1:3)]
str(eltrQ)
eltrQ<-na.omit(eltrQ)
eltrQ<-eltrQ[-1,]
eltrQ[]<-rev(eltrQ$`LT interest rate Q`)
ltQ<-ts(data = eltrQ, start = c(2000,01), frequency = 4)

#Database from EViews
# db<-read_csv("C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/inf_gdp_db.csv", col_names = FALSE)

#--------------------------------------------------------------------------------------------

header<-dashboardHeader(title = "Assessing the efects of unconventional monetary policy in the Eurosystem during the covid19 crisis",
                        titleWidth = 1050)

sidebar<-dashboardSidebar(
    width = 300,
    sidebarMenu(
        id = "menu",
        menuItem("Unconventional monetary policy", tabName = "part1", icon = icon("university")),
        menuItem("SVAR model", tabName = "part2", icon = icon("chart-bar")),
        menuItem("Findings", tabName = "part3", icon = icon("database"))
    )
)

body <- dashboardBody(tabItems(
    
#First part
    
    tabItem(tabName = "part1",
            fluidPage(h1(strong("Introduction to unconventional monetary policy")),
            p("When policy rates are already equal to 0, the economy is said to be in the Zero 
              Lower Bound. The problem with the ZLB is that the economy is said to be in a liquidity 
              trap: increasing the monetary base will not have effects on investment and spending decisions. 
              Conventional monetary policy becomes inefficient at this point. CBs will affect long term interest 
              rates in order to still have an influence on the real economy. Thus, the difference between conventional
              and unconventional monetary policy is the term of the interest rates that are affected. 
              Conventional monetary policy affects ST interest rates."),
            p("The way CBs affect LT rates is by entailing large purchases of assets programmes,
              although here we would also include announcements of how monetary policy is going to be
              and policies concerning credit easing. The complexity of these policies is greater and 
              so there are different channels of transmission to the economy. This is an important remark 
              since  the  correct  assessment  of  such  policies strongly  depends on  thechosenchannel. 
              Thus, the  modality  of  asset  purchases is  crucial to betterunderstand  the transmission 
              channelthrough which economic policies affect the economy."),
            h2(em("Transmission channels")),
            p(strong("- Portfolio rebalancing channel"), "As  a  conventional OMO, the increase on the demand of 
              assets will entail an increase in asset prices, resulting in lower interest rates(local supply effect).
              Once  the  prices  of  these  assets  are  higher,  the  relative  price  of  riskier  assets  becomes 
              relatively cheaper.Investors observe this and are willing to buy riskier assets with their bank deposits. 
              The previous effect is repeated: asset prices increase as a result of a rising demand and their yield falls. 
              This process is repeated until all assets in the markets have been affected by the initial purchase of the CB."),
            p(strong("- Signaling channel"), "The CB, by carrying out large purchases of assets, is signaling that 
              policy rates will remain low for a longer time (loose monetary policy). It demonstrates commitmentand 
              confidence,  which  in  return  increases  the credibility of the public to its announcements."),
            p(strong("- Liquidity channel"), "During a recession, it can happen that some assets become illiquid  
              due to the lack of buyers. Therefore, when the CB purchases large amounts of assets, the liquidity 
              problem is reversed, and the risk of not finding a potential buyer disappears(the risk premium would decrease). 
              However, in the financial markets in which the CB operates, there is usually not a liquidity problem (so the effect 
              on prices is not extremely relevant)."),
            p(strong("- Duration channel"), "When the CB buys long maturity assets, the interest rate risk that investors 
              have is transferred to the CB. The interest rate risk normally corresponds to the uncertainty in future changes 
              on the interest rates. Hence, reducing risks results in lower risk premiums, and lower long term interest rates."),
            p(strong("- Insurance channel"), "It depicts the announces of the CB showing that it is willing to ease monetary 
              policy as it needs to in order to recover the desired output levels."),
            br(),
            h1(strong("Monetary policy in the Eurosystem")),
            p(strong("The main objective of the ECB is price stability (keeping inflation constant at a 2% rate).")),
            p("Regarding unconventional monetary policies, we  can distinguish three groups: forward  guidance, liquidity provision 
              policies and credit easing. When  it  comes to the subject of liquidity provision, the ECB intervenes through
              regular OMOs in one-week liquidity-providing operations (MROs) and three-month liquidity-providing 
              operations (LTROs). Nevertheless, there exist non-regular OMOs which intend to provide long term 
              funding for financial institutions. By doing so, they facilitate credit conditions and lending. The most 
              famous example of these operations are TLTROs, which provide liquidity to credit institutions for periods 
              of a maximum of 4 years. The interest rates applied in financing operations are related to the 
              loans that banks are willing to offer. Hence, the more optimistic banks are for lending, the more 
              attractive interest rates will be. We can also include the APPs. These consist in large purchases 
              of securities to ensure reaching the target inflation rate. The difference between theses programs 
              is the nature of the securities being bought. In a way, we can say that it is the QE measure of the eurosystem."),
            p("Forward  Guidance is formed by the announcements of the strategy that the CB is going to follow.
              By doing this, investors will understand how the  CB is going to react and they will be able to 
              adapt their expectations to the CB's reaction function. This will reduce uncertainty and shape
              interest rate expectations."),
            fluidRow(column( width = 6,h4(em("APP breakdown history"), align = 'center'), imageOutput("app_graph") ),
                     column( width = 6,h4(em("Decomposition of the APP in millions of euros"), align = 'center', DT::dataTableOutput("app_tab"))
                     )
            ),

            fluidPage(h2(em("Policies deployed during the pandemic")),
            p(strong("PEPP")),
            p(strong("PELTROs")))

    )),
    
#Second part

    tabItem(tabName = "part2",
            fluidPage(h1(strong("Methodology")),
                      h2(em("Structural Vector Autoregressive model")),
           uiOutput('ex1'),
           uiOutput('ex2'),
           uiOutput('ex3'),
           uiOutput('ex4'),
                    h2(em("Restrictions")),
                    p("We need to add another restriction so that the model can be estimated. The restriction imposed
                      is based on economic theory."),
                    p("In the precedent equations we exposed a relationship of variables on the period t and t-1.
                      Nevertheless, one may argue that it is possible that a variable is not affected immediately 
                      by the other. The implications of this are that one coefficient in the matrix B could be 0."),
           uiOutput('ex5'),
                    h2(em("Impulse Response Functions")),
                    p("These functions are calculated using the MA part of the SVAR. They illustrate how shocks of 
                      monetary policy variables affect macroeconomic variables. It is useful to assess the transmission 
                      mechanisms of such policies."),
                    h2(em("Variance decomposition")),
                    p("The forecast error variance decomposition shows the composition of the variation of each variable 
                      to the different shocks. This means that we can observe how much of the changes in the variables 
                      correspond to each exogenous shock.")
            )
            ),

#Third part

    tabItem(tabName = "part3",
            fluidPage(h1(strong("Effects of ECB responses to the covid crisis using the SVAR model")),
                      h2(em("Dataset and descriptive statistics"))),
            fluidRow(column(width = 4, h4(em("Inflation rate"), align = 'center'), plotlyOutput("inf_graph")),
                     column(width = 4, h4(em("GDP growth rate"), align = 'center'), plotlyOutput("gdp_graph")),
                     column(width = 4, h4(em("LT interest rates"), align = 'center'), plotlyOutput("ltr_graph"))
                     ),
            fluidPage(br(),
                      br(),
                      br()),
            fluidRow(column(width = 9,
                box(
                title = "Descriptive statistics", background = "light-blue", collapsible = TRUE,
                solidHeader = TRUE, imageOutput("stats", 
                                                height = 350)
            ))),
            fluidPage(h2(em("Stationarity")),
                        p("There are two levels of stationary series: stationary with respect to its variance 
                          and its trend. If it is non-stationary with respect to its variance, the variations all 
                          along the series will not be constant. On the other hand, if the series are non-stationary 
                          with respect to the trend, the values will not converge into the mean. It is an important 
                          part before going any further because we need that all the observations depend on the same 
                          distribution. In order to test the stationarity, we use the graphs containing the evolution 
                          of the times series, their respective correlogram and the Augmented Dickey-Fuller test.")),
            fluidRow(column(width = 4, h4(em("Inflation rate")), imageOutput("inf_stat")),
                     column(width = 4, h4(em("GDP growth rate")), imageOutput("gdp_stat")),
                     column(width = 4, h4(em("LT interest rates")), imageOutput("ltr_stat"))
            ),
            fluidPage(h2(em("Findings of the regression")),
                      h4(em("Impulse Response Functions")), imageOutput("irf"),
                      br(),
                      br(),
                      br(),
                      h4(em("Variance decomposition")), imageOutput("var"),
                      p("The final results did not allow us to give a clear conclusion about the effects of unconventional 
                        monetary policy as a response to the covid19 crisis in the eurosystem. The IRFs of inflation and 
                        output to shocks in LT interest rates did not show any movements. Maybe, the nature of the recession 
                        and the fast recovery could differ from other economic slumps. Even though, monetary policy shocks could explain 
                        a part of the variance of the macro variables.")
                      ),

)
    
)
)

ui <- dashboardPage(header, sidebar, body)


#--------------------------------------------------------------------------------------------

server <- function(input, output) {

#Unconventional monetary policy in the Eurosystem
    
#Graph of APP breakdown history
    output$app_graph <- renderImage({
        pfad<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/graphapp.png"
        list(src = pfad,
             contentType = 'image/png',
             height = 300,
             width = 600,
             alt = "Alt text")
    }, deleteFile = F)
    
#Table for APP
    output$app_tab <- DT::renderDataTable({
        DT::datatable(app, options = list(pageLength = 4))
    })
    
#Methodology

#Equations
    output$ex1 <- renderUI({
        withMathJax(helpText('Initial equations for a bivariate model:  $$y_t + b_{12}x_t = b_{10} + \\gamma_{11}y_{t-1} + \\gamma_{12}x_{t-1} + \\varepsilon_{yt} $$'),
                    helpText('$$b_{21}y_t + x_t = b_{20} + \\gamma_{21}y_{t-1} + \\gamma_{22}x_{t-1} + \\varepsilon_{xt} $$')
        )
    })
    output$ex2 <- renderUI({
        withMathJax(
            helpText('we can express both equations in a matrix form: $$BX_t = \\Gamma_0 + \\Gamma_1X_{t-1} + \\varepsilon_t$$'),
            helpText('where: $$B=\\begin{bmatrix}1 & b_{12}\\\\b_{21} & 1\\end{bmatrix}, X_t = \\begin{bmatrix}y_t\\\\x_t\\end{bmatrix}, \\Gamma_0 = \\begin{bmatrix}b_{10}\\\\b_{20}\\end{bmatrix}, \\Gamma_1 = \\begin{bmatrix}\\gamma_{11} & \\gamma_{12}\\\\\\gamma_{21} & \\gamma_{22}\\end{bmatrix}, \\varepsilon_t = \\begin{bmatrix}\\varepsilon_{yt}\\\\\\varepsilon_{xt}\\end{bmatrix}$$')
        )
    })
    output$ex3 <- renderUI({
        withMathJax(
            helpText('by multiplying the equation by \\(B^{-1}\\) we obtain the reduce form:
               $$X_t = A_0 + A_1X_{t-1} + u_t$$'))
    })
    
    output$ex4 <- renderUI({
        withMathJax(
            helpText('remember that $$\\Sigma_e = \\begin{bmatrix}\\sigma^2_y & \\sigma_{yx}\\\\\\sigma_{xy} & \\sigma^2_x\\end{bmatrix}$$'))
    })
    
    output$ex5 <- renderUI({
        withMathJax(
            helpText('$$B = \\begin{bmatrix}1 & b_{12}\\\\0 & 1\\end{bmatrix}$$'))
    })
    
#Effects of ECB responses to the covid crisis using the SVAR model
    
#Time series graphs for quarterly data
    output$inf_graph <- renderPlotly({
        ts_plot(piQ, title = "Inflation rate in the eurosystem", Ytitle = "Inflation rate (%)", Ygrid = TRUE, color = "darkorange")
    })
    
    output$gdp_graph <- renderPlotly({
        ts_plot(yy, title = "GDP growth rate in the eurosystem", Ytitle = "Growth rate (%)", Ygrid = TRUE, color = "mediumseagreen")
    })
    
    output$ltr_graph <- renderPlotly({
        ts_plot(ltQ, title = "Long term interest rate - 10 years maturity", Ytitle = "Interest rate (%)", Ygrid = TRUE, color = "cornflowerblue")
    })
    
#Descriptive statistics
    output$stats <- renderImage({
        pfed<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/desstats.png"
        list(src = pfed,
             contentType = 'image/png',
             width = 350,
             height = 350,
             alt = "Alt text")
    }, deleteFile = F)
    
#Correlograms
    output$inf_stat <- renderImage({
        fir<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/corinf.png"
        list(src = fir,
             contentType = 'image/png',
#             height = 300,
#             width = 600,
             alt = "Alt text")
    }, deleteFile = F)
    
    output$gdp_stat <- renderImage({
        sec<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/corgdp.png"
        list(src = sec,
             contentType = 'image/png',
#             height = 300,
#             width = 600,
             alt = "Alt text")
    }, deleteFile = F)
    
    output$ltr_stat <- renderImage({
        thi<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/corltr.png"
        list(src = thi,
             contentType = 'image/png',
#             height = 300,
#             width = 600,
             alt = "Alt text")
    }, deleteFile = F)
    
#IRFs
    output$irf <- renderImage({
        fin1<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/irf.png"
        list(src = fin1,
             contentType = 'image/png',
             height = 450,
             width = 650,
             alt = "Alt text")
    }, deleteFile = F)
    
#Variance decomposition
    output$var <- renderImage({
        fin2<- "C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Soutenance/vardec.png"
        list(src = fin2,
             contentType = 'image/png',
             height = 300,
             width = 800,
             alt = "Alt text")
    }, deleteFile = F)

    output$menu <- renderMenu({
        
        sidebarMenu(
            menuItem(
                "Menu Item", icon = icon("university")
            ),
            menuItem(
                "Menu Item", icon = icon("chart-bar")
            ),
            menuItem(
                "Menu Item", icon = icon("database")
            )
        )
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
