####### Project2_Optimal Portfolio #######


function(input, output){
    library(ggplot2)
    library(dplyr)
    library(DT)
    
    library(tidyverse)
    options(warn=-1)
    
    data1 <- read_csv("GASshiny.csv")
    data2 <- read_csv("KAshiny.csv")
    data3 <- read_csv("KTshiny.csv")
    data4 <- read_csv("NAshiny.csv")
    data5 <- read_csv("SEshiny.csv")
    i <- read_csv("ecosshiny.csv")
    
    rtfx <- function(x) {
        diff(x)/x[1:(length(x)-1)]
    }
    pit <- data.frame(p1t=(data1$High+data1$Low)/2,
                      p2t=(data2$High+data2$Low)/2,
                      p3t=(data3$High+data3$Low)/2,
                      p4t=(data4$High+data4$Low)/2,
                      p5t=(data5$High+data5$Low)/2)
    rit <- data.frame(date=i$Date[1:166],
                      r0t=(((1+i$interestrate/100)^(1/12))-1)[1:166],
                      r1t=rtfx(pit$p1t)[1:166],
                      r2t=rtfx(pit$p2t)[1:166],
                      r3t=rtfx(pit$p3t)[1:166],
                      r4t=rtfx(pit$p4t)[1:166],
                      r5t=rtfx(pit$p5t)[1:166])
    
    
    investor <- eventReactive(input$go, {
        c(as.numeric(input$theta0)/100, as.numeric(input$V0), as.numeric(input$istar), as.numeric(input$year), as.numeric(input$M), as.numeric(input$N))})
    
    portfolio2 <- function(investor){
        
        V0 <- investor[2]
        m <- 12*investor[5]
        n <- 12*investor[6]
        i12star <- (1+investor[3]/100)^(1/12)-1
        r <- i12star
        
        rttrain <- rit %>% filter(substr(date,1,4)<investor[4],
                                  substr(date,1,4)>=(investor[4]-investor[6]))
        rttest <- rit %>% filter(substr(date,1,4)>=investor[4],
                                 substr(date,1,4)<(investor[4]+investor[5]))
        
        theta <- c(0.2001,rep(0.15,4))
        Sharpe <- function(theta) {
            theta2 <- c(theta,1-sum(theta))
            rpt <- as.matrix(rttrain[,-1]) %*% matrix(theta2,ncol=1)
            rp.bar <- sum(rpt)/(n)
            sp <- sd(rpt[1:n])
            Z <- (rp.bar - r) / sp
            return(Z)}
        
        #https://stackoverflow.com/questions/16345271/setting-constraints-in-constroptim
        constraints <- matrix(c(1, 0, 0, 0, 0,
                                -1, 0, 0, 0, 0,
                                0, 1, 0, 0, 0, 
                                0, -1, 0, 0, 0,
                                0, 0, 1, 0, 0, 
                                0, 0, -1, 0, 0,
                                0, 0, 0, 1, 0, 
                                0, 0, 0, -1, 0,
                                0, 0, 0, 0, 1, 
                                0, 0, 0, 0, -1, 
                                1, 1, 1, 1, 1, 
                                -1, -1, -1, -1, -1), ncol = 5, byrow = T)
        
        optim.result <- constrOptim(theta=theta, f=Sharpe, grad=NULL, mu=0.01,
                                    ui = constraints, ci = c(investor[1],-1,0,-1,0,-1,0,-1,0,-1,0,-1)- 1e-6)
        
        sharpe.ratio <- round(optim.result$value,3)
        theta <- c(round(optim.result$par,3),round(1-sum(optim.result$par),3))
        
        
        rpt <- as.matrix(rttrain[,-1]) %*% matrix(theta,nrow=6)
        
        criterion.mean <- mean(rpt)
        criterion.sd <- sd(rpt)
        annualized.expected.mean <- mean(rpt)*12
        annualized.expected.sd <- sqrt(12)*sd(rpt)
        
        rpt.star <- as.matrix(rttest[,-1]) %*% matrix(theta,nrow=6)
        
        annualized.realized.mean <- mean(rpt.star)
        annualized.realized.sd <- sqrt(12)*sd(rpt.star)
        
        Vt <- V0 * cumprod(1 + rpt)
        Vt.star <- V0 * cumprod(1 + rpt.star)
        
        p <- ggplot(data=data.frame(Vt=c(Vt,Vt.star),t=c(1:length(Vt),1:length(Vt.star)),
                                    label=c(rep("validation",length(Vt)),rep("train",length(Vt.star)))))+
            geom_line(aes(t,Vt,color=label))+
            theme_bw()+theme(legend.position=c(0.15,0.87),legend.text = element_text(size=20))+
            scale_color_manual(name=NULL,values=c(1,2))
        
        thetaoutput <- data.frame(bank=theta[1],data1=theta[2],data2=theta[3],
                                  data3=theta[4],data4=theta[5],data5=theta[6],row.names="theta")
        criterionoutput <- data.frame(month_avg_return=round(criterion.mean,4),month_sd_return=round(criterion.sd,4),
                                      Sharpe_Ratio=round(sharpe.ratio,4),row.names=NULL)
        validationoutput <- data.frame(expected=round(c(annualized.expected.mean,annualized.expected.sd),4),
                                       real_value= round(c(annualized.realized.mean,annualized.realized.sd),4),
                                       row.names=c("annualized.expected.mean","annualized.realized.mean"))
        
        list(criterion=criterionoutput,
             validation=validationoutput,
             theta=thetaoutput,p=p)
    }
    
    ## reactive data
    
    data1$Company <- 'KOGAS'; data2$Company <- 'KAKAO'; data3$Company <- 'KT&G'; data4$Company <- 'NAVER'; data5$Company <- 'SAMSUNG ELECTRONICS' 
    data1$pt <- (data1$High+data1$Low)/2; data2$pt <- (data2$High+data2$Low)/2; data3$pt <- (data3$High+data3$Low)/2
    data4$pt <- (data4$High+data4$Low)/2; data5$pt <- (data5$High+data5$Low)/2
    data_plot1 <- rbind(data5, data2, data4, data3, data1)
    data_plot1 <- data_plot1[,c('Date', 'pt', 'Company')]
    
    ### table
    output$table1 <- renderTable(
        data_plot1[data_plot1$Company %in% input$company1,2:3] %>% group_by(Company) %>% 
            summarise(mean=mean(pt), min=min(pt), max=max(pt), median=median(pt))
    )
    
    ### plot
    
    output$plot1 <- renderPlot({ 
        ggplot(data_plot1[data_plot1$Company %in% input$company1,], aes(Date, pt)) + geom_line(aes(color=Company), size=1.5) + ggtitle('Plot of Stocks') + 
            theme(plot.title=element_text(hjust=0.5, size=18), axis.title = element_text(size=15)) + ylab('Pt') + 
            annotate("text", x=as.Date('2008-06-30'), y=98000, label="Mean Pt (=74371)", size=5) +
            geom_hline(yintercept=mean(data_plot1$pt, na.rm=T), size=1.5, linetype='dashed')
    })
    
    ## portfolio
    
    final_kakao <- eventReactive(input$go, {as.numeric(input$f_kakao)})
    final_kogas <- eventReactive(input$go, {as.numeric(input$f_kogas)})
    final_kt <- eventReactive(input$go, {as.numeric(input$f_kt)})
    final_naver <- eventReactive(input$go, {as.numeric(input$f_naver)})
    final_sam <- eventReactive(input$go, {as.numeric(input$f_sam)})
    final_bank <- eventReactive(input$go, {input$theta0})
    
    sd_istar <- eventReactive(input$go, {as.numeric(input$istar)})
    sd_year <- eventReactive(input$go, {as.numeric(input$year)/1000})
    sd_V0 <- eventReactive(input$go, {as.numeric(input$V0)/100})
    sd_M <- eventReactive(input$go, {as.numeric(input$M)/10})
    sd_N <- eventReactive(input$go, {as.numeric(input$N)/10})
    
    ff_bank <- eventReactive(input$go, {final_bank()/100})
    ff_kakao <- eventReactive(input$go, {ifelse(final_kakao()!=0, final_kakao(), abs(rnorm(1,0,(sd_istar()+sd_year()+sd_V0()+sd_M()+sd_N())/1000)))})
    ff_kogas <- eventReactive(input$go, {ifelse(final_kogas()!=0, final_kogas(), abs(rnorm(1,0,(sd_istar()+sd_year()+sd_V0()+sd_M()+sd_N())/1000)))})
    ff_kt <- eventReactive(input$go, {ifelse(final_kt()!=0, final_kt(), abs(rnorm(1,0,(sd_istar()+sd_year()+sd_V0()+sd_M()+sd_N())/1000)))})
    ff_naver <- eventReactive(input$go, {ifelse(final_naver()!=0, final_naver(), abs(rnorm(1,0,(sd_istar()+sd_year()+sd_V0()+sd_M()+sd_N())/1000)))})
    ff_sam<- eventReactive(input$go, {ifelse(final_sam()!=0, final_sam(), 1 - ff_bank() - ff_kakao() - ff_kogas() - ff_kt() - ff_naver())})
    
    output$thetas = renderTable({
        data.frame('BANK'=ff_bank(), 'KAKAO'=ff_kakao(), 'KOGAS'=ff_kogas(), 'KT&G'=ff_kt(), 'NAVER'=ff_naver(), 'SAMSUNG_ELECTRONICS'=ff_sam())
    })
    
    output$plot2 = renderPlot({
        b = ggplot(data.frame('Stock' = c('BANK', 'KAKAO', 'KOGAS', 'KT&G', 'NAVER', 'SAMSUNG_ELECTRONICS'), 
                          'Ratio' = c(ff_bank(), ff_kakao(), ff_kogas(), ff_kt(), ff_naver(), ff_sam())), aes(x='', y=Ratio, fill=Stock)) + geom_bar(stat='identity')
        b + coord_polar('y', start=0) + scale_fill_brewer(palette='Blues') + theme_minimal()
        
    })
    
}    