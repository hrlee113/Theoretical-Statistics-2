##################################
####### Project1_Insurance #######
##################################


function(input, output){
    
    # Library load
    library(ggplot2)
    library(dplyr)
    library(DT)
    library(maps)
    options(warn=-1)
    
    # Data load
    dat=read.csv('data.csv')
    colnames(dat)[1] <- 'Kilometres'
    zone_list <- c('Seoul & Gyeonggi-do', 'Gyeongsangnam-do', 'Gyeongsangbuk-do', 'Jeolla-do', 'Chungcheong-do', 'Gangwon-do', 'Jeju-island')
    kilo_list <- c('less than\n1000', '1000-15000', '15000-20000', '20000-25000', 'more than\n25000')
    
    # Data pre-processing
    zone_mean <- tapply(dat$Payment, dat$Zone, mean)
    zone_claim_sum <- tapply(dat$Claims, dat$Zone, sum)
    kilo_claim_sum <- tapply(dat$Claims, dat$Kilometres, sum)
    zone_payment_sum <- tapply(dat$Payment, dat$Zone, sum)
    kilo_payment_sum <- tapply(dat$Payment, dat$Kilometres, sum)
    zone_payment_mean <- zone_payment_sum / zone_claim_sum
    kilo_payment_mean <- kilo_payment_sum / kilo_claim_sum
    
    dat$Kilometres <- ordered(dat$Kilometres)
    dat$Zone <- as.factor(dat$Zone)
    dat$Bonus <- ordered(dat$Bonus)
    dat$Make <- as.factor(dat$Make)
    dat$lninsured <- log(dat$Insured)
    dat$lninsured[is.infinite(dat$lninsured)==T] = 0
    
    # GLM for calculating insurance fee (= Customized Premium)
    glm_n <- glm(Claims/Insured ~ offset(lninsured) + Kilometres + Zone + Bonus + Make, family='poisson', data=dat, na.action='na.omit')
    glm_y<- glm(Payment/Claims ~ Kilometres + Zone + Bonus + Make + Zone*Bonus, family = Gamma(link="log"), data = dat, weight=Claims)
    
    p_ftn <- function(dataa){
        en <- predict(glm_n, dataa, type='response')
        ey <- predict(glm_y, dataa, type='response')
        final_ey <- min((ey-input$deductable),input$limit)
        p <- en * final_ey
        return(p)
    }
    
    ## Calculate Customized Premium
    private_predicted_premium <- reactive({
        newdata <- data.frame('Kilometres'=input$kilometres, 'Zone'=input$zone, 'Make'=input$make, 'Bonus'=as.factor(input$bonus), 'lninsured' = mean(dat$lninsured))
        p_ftn(newdata)
    })
    
    # Full results
    origindata <- dat[,c(1:4,8)]
    origindata$lninsured <- round(origindata$lninsured,1)
    group_list <- paste0(dat[,1], dat[,2], dat[,3], dat[,4])
    full_predicted_premium <- reactive({
        p_ftn(origindata)
    })
    
    ###### Output ######
    
    # ValueBox
    output$kilometres1 <- renderValueBox({
        valueBox(
            value = tags$p("Mileage", style = "font-size: 70%;"), paste0(input$kilometres, "km"), icon = icon("ruler"), color = "teal"
        )
    })
    output$zone1 <- renderValueBox({
        valueBox(
            value = tags$p("Area", style = "font-size: 70%;"), input$zone, icon = icon("globe-asia"), color = "light-blue"
        )
    })
    output$make1 <- renderValueBox({
        valueBox(
            value = tags$p("Car", style = "font-size: 70%;"), input$make, icon = icon("car-side"), color = "aqua"
        )
    })
    output$bonus1 <- renderValueBox({
        valueBox(
            value = tags$p("Bonus", style = "font-size: 70%;"), paste0(input$bonus, ' year'), icon = icon("car-crash"), color = "blue"
        )
    })
    
    # Text 
    output$premium1 <- renderText({
        paste0('Your Insurance is ',round(private_predicted_premium(),1),' won')
    })
    
    # RenderDataTable
    output$table2 <- DT::renderDT({
        datatable(data.frame('Group'=group_list, 'Mileage'=origindata[,1], 'Area'=origindata[,2], 'Car'=origindata[,3], 'Bonus'=origindata[,4], 'Premium' = round(full_predicted_premium(),1)) 
                  %>% filter(round(full_predicted_premium(),1) >= as.numeric(input$limit1) & round(full_predicted_premium(),1) <= as.numeric(input$limit2))) 
    })
    
    # Data pre-processing for making plots
    mean_claims = data.frame(dat %>% group_by(Zone) %>% summarise(mean(Claims)))
    mean_payment = data.frame(dat %>% group_by(Zone) %>% summarise(mean(Payment)))
    mean_insured = data.frame(dat %>% group_by(Zone) %>% summarise(mean(Insured)))
    mean_grouped_zone_pre = merge(mean_claims, mean_payment, by='Zone')
    mean_grouped_zone = merge(mean_insured, mean_grouped_zone_pre, by='Zone')
    colnames(mean_grouped_zone) <- c('Area', 'Mean_Insured', 'Mean_Claims', 'Mean_Payment')
    mean_grouped_zone$Area <- zone_list
    
    # Regional Information
    output$table1 <- renderTable({
        mean_grouped_zone
    })
    output$plot1 <- renderPlot({
        ggplot(mean_grouped_zone, aes(Area, Mean_Insured)) + geom_bar(stat='identity', fill='#CCE5A3')
    })
    output$plot2 <- renderPlot({
        ggplot(mean_grouped_zone, aes(Area, Mean_Payment)) + geom_bar(stat='identity', fill='#A4D967')
    })
    output$plot3 <- renderPlot({
        ggplot(mean_grouped_zone, aes(Area, Mean_Claims)) + geom_bar(stat='identity', fill='#4FA601')
    })
}
