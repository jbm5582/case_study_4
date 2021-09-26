#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(viridis)

data_ordered = read.csv("data/data_ordered.csv")
res_tot = read.csv("data/res_tot.csv")
res_b = read.csv("data/res_b.csv")
res_w = read.csv("data/res_w.csv")


NCtracts= rgdal::readOGR("data/NHtracts.shp")
NHtracts= NCtracts[which( (NCtracts$STATEFP == 37) & (NCtracts$COUNTYFP == 129)),]
NHtracts= NHtracts[order(as.numeric(substr(NHtracts$TRACTCE,2,6))),]
NHtracts = NHtracts[which(NHtracts$TRACTCE != 990100),]


fillmap2<-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,
                   main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,
                   leg.cex=1){
    
    # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
    y.uq=sort(unique(c(y,y.scl)))
    cols<-viridis(length(y.uq),direction=-1)
    shading=y
    for (i in 1:length(y)){
        shading[i]<-cols[which(y.uq==y[i])]
    }
    
    par(mar=c(0,0,2,0))
    if (leg.loc=="beside"){
        layout(matrix(1:2,ncol=2),width=c(.8,.2))
    } else 
        if (leg.loc=="below"){
            layout(matrix(1:2,nrow=2),height=c(.6,.4))
        } else (print("leg.loc options are below or beside"))
    
    plot(map,col=shading,axes=F, lty=map.lty)
    title(main=figtitle,cex.main=main.cex,line=main.line) 
    
    par(mar=c(5, 4, 4, 2) + 0.1)
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
    cols.5=cols[seq(1,length(y.uq),length.out=5)]
    lab.5=cols.5
    for (i in 1:5){lab.5[i]=y.uq[which(cols==cols.5[i])[1]]}
    lab.5=round(as.numeric(lab.5),leg.rnd)
    par(mar=c(0,0,0,0))
    if (leg.loc=="beside"){
        legend_image <- as.raster(matrix(cols, ncol=1))
        text(x=1.6, 
             y = seq(0,length(y.uq),length.out=5)/length(y.uq),
             labels = rev(lab.5), cex=leg.cex)
        rasterImage(legend_image, 0, 0, 1,1)
    } else{
        legend_image <- as.raster(matrix(cols, nrow=1))
        text(y=-0.25, 
             x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),
             labels = lab.5, cex=leg.cex)
        rasterImage(legend_image, 0, 0, 2,1)
    }
}

# Define  server logic required to draw a histogram

shinyServer(function(input, output){
    output$map <- renderPlot({
        
        if((input$data == 'Total Arrests') & (input$stat == 'None')) {
            
            map = data_ordered$arrests_total[which(data_ordered$year == input$year)]
            mapscl = data_ordered$arrests_total
            title = paste(input$year,input$data)
           
        } else {
            
            if((input$data == 'Black Arrests') & (input$stat == 'None')){
                
                map = data_ordered$arrests_B[which(data_ordered$year == input$year)]
                mapscl = data_ordered$arrests_B
                title = paste(input$year,input$data)               
            } else {
                
                if((input$data == 'White Arrests') & (input$stat == 'None')){
                    
                    map = data_ordered$arrests_W[which(data_ordered$year == input$year)]
                    mapscl = data_ordered$arrests_W
                    title = paste(input$year,input$data)
                } else{
                    
                    if((input$data == 'Total Arrests') & (input$stat == 'SIR')){
                        
                        map = data_ordered$tot_SIR[which(data_ordered$year == input$year)]
                        mapscl = data_ordered$tot_SIR
                        title = paste(input$year,"SIR",input$data)
                    } else {
                        
                        if ((input$data == 'Black Arrests') & (input$stat == 'SIR')){
                            
                            map = data_ordered$black_SIR[which(data_ordered$year == input$year)]
                            mapscl = data_ordered$black_SIR
                            title = paste(input$year, "SIR", input$data)
                        } else {
                            
                            if((input$data == 'White Arrests') & (input$stat == 'SIR')){
                                
                                map = data_ordered$white_SIR[which(data_ordered$year == input$year)]
                                mapscl = data_ordered$white_SIR
                                title = paste(input$year,"SIR",input$data)
                            } else {
                                
                                if ((input$data == 'Black Arrests') & (input$stat == 'Percent Arrested')) {
                                    
                                    map = data_ordered$pct_arrests_ct_black[which(data_ordered$year == input$year)]
                                    mapscl = data_ordered$pct_arrests_ct_black
                                    title = paste(input$year,"Percent of Black Population Arrested")
                                } else {
                                    
                                    if ((input$data == 'White Arrests') & (input$stat == 'Percent Arrested')) {
                                        
                                        map = data_ordered$pct_arrests_ct_white[which(data_ordered$year == input$year)]
                                        mapscl = data_ordered$pct_arrests_ct_white
                                        title = paste(input$year, "Percent of White Population Arrests")
                                        
                                    } else {
                                        
                                        if ((input$data == 'Total Arrests') & (input$stat == 'Percent Arrested')) {
                                            
                                            map = data_ordered$pct_arrests_total[which(data_ordered$year == input$year)]
                                            mapscl = data_ordered$pct_arrests_total
                                            title = paste(input$year,"Percent Total Population Arrested")
                                            
                                        } else {
                                           
                                            if ((input$data == 'Total Arrests') & (input$stat == 'Poisson Regression')){
                                                
                                                map = c(data_ordered$outcomes[which(data_ordered$year == input$year)])
                                                mapscl = c(data_ordered$outcomes)
                                                title = paste(input$year,input$data,input$stat)
                                            } else {
                                                
                                                if ((input$data == 'Black Arrests') & (input$stat == 'Poisson Regression')){
                                                    
                                                    map = c(data_ordered$blk_outcomes[which(data_ordered$year == input$year)])
                                                    mapscl = c(data_ordered$blk_outcomes)
                                                    title = paste(input$year,input$data,input$stat)                                                   
                                                } else {
                                                    
                                                    if((input$data == 'White Arrests') & (input$stat == 'Poisson Regression')){
                                                        
                                                        map = c(data_ordered$wht_outcomes[which(data_ordered$year == input$year)])
                                                        mapscl = c(data_ordered$wht_outcomes)
                                                        title = paste(input$year,input$data,input$stat)
                                                        
                                                    } else {
                                                
                                                if ((input$data == 'Black Arrests') & (input$stat == 'Poisson Regression')){
                                                    
                                                    map = c(data_ordered$blk_outcomes[which(data_ordered$year == input$year)])
                                                    mapscl = c(data_ordered$blk_outcomes)
                                                    title = paste(input$year,input$data,input$stat)                                                   
                                                } else {
                                                    
                                                    if((input$data == 'White Arrests') & (input$stat == 'Poisson Regression')){
                                                        
                                                        map = c(data_ordered$wht_outcomes[which(data_ordered$year == input$year)])
                                                        mapscl = c(data_ordered$wht_outcomes)
                                                        title = paste(input$year,input$data,input$stat)
                                                        
                                                    } else {
                                                        
                                                        if ((input$data == 'Total Arrests') & (input$stat == "Percent of Total Arrests")){
                                                            
                                                            map = data_ordered$arrests_total_all[which(data_ordered$year == input$year)]
                                                            mapscl = data_ordered$arrests_total_all
                                                            title = paste(input$year,input$data,input$stat)
                                                        } else {
                                                            
                                                            if ((input$data == 'Black Arrests') & (input$stat == 'Percent of Total Arrests')){
                                                                
                                                                map = data_ordered$arrests_B_total[which(data_ordered$year == input$year)]
                                                                mapscl = data_ordered$arrests_B_total
                                                                title = paste(input$year,input$data,input$stat)
                                                                
                                                            } else{
                                                                
                                                                if ((input$data == 'White Arrests') & (input$stat == 'Percent of Total Arrests')){
                                                                    
                                                                    map = data_ordered$arrests_W_total[which(data_ordered$year == input$year)]
                                                                    mapscl = data_ordered$arrests_W_total
                                                                    title = paste(input$year,input$data,input$stat)
                                                                }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } 
        
    fillmap2(NHtracts,title,map,map.lty = 0, leg.loc = "below", y.scl = mapscl, leg.rnd = 3)
        
    })
    
    output$table <- renderTable({
        
        if ((input$data == 'Total Arrests') & (input$stat == 'Poisson Regression')){
            
                mat=exp(res_tot[2:7,c(2,4,6)])
                colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
                rownames(mat)<-c("% Black",
                                 "% White",
                                 "% Living in Poverty",
                                 "% Male",
                                 "% Less than High School Education",
                                 "% with Bachelors Degree or Higher Education")
              mat   
        } else {
            
            if ((input$data == 'Black Arrests') & (input$stat == 'Poisson Regression')){
                
                mat=exp(res_b[2:7,c(2,4,6)])
                colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
                rownames(mat)<-c("% Total Population Arrested",
                                 "% White",
                                 "% Living in Poverty",
                                 "% Male",
                                 "% Less than High School Education",
                                 "% with Bachelors Degree or Higher Education")
                mat
            } else {
                
                  if ((input$data == 'White Arrests') & (input$stat == 'Poisson Regression')){
                
                      mat = exp(res_w[2:7,c(2,4,6)])
                      colnames(mat) <- c("Mean","95% CI Lower Bound","95% CI Upper Bound")
                      rownames(mat)<-c("% Total Population Arrested",
                                       "% Black",
                                       "% Living in Poverty",
                                       "% Male",
                                       "% Less than High School Education",
                                       "% with Bachelors Degree or Higher Education")
                      mat                
                  }               
                
            }
                
        } 
            
        }, rownames = T,colnames = T, digits = 3, width = "100%")
  
      output$text <- renderText({
        
        if(input$stat == 'SIR') {
            
            "The standardized incidence ratio (SIR) adjustment was applied here. SIR is a method of adjusting for tract population by calculating a ratio of the observed count of arrests to the expected counts of arrests. The expected count of arrests is calculated as a rate of arrests over all tracts and years times the tract population for a given year. The population used reflects the data being displayed (e.g. Black population only when 'Black Only Arrests' is selected). Values greater than 1 suggest more observed arrests than expected.
Plot object"
        } else {
            
            if (input$stat == 'Poisson Regression'){
                
                "The Poisson regression option for adjustment was applied here. In this method of adjustment, a Poisson regression model with spatio-temporal covariate adjustment was applied (see table output). The mapped values display the residual spatial variation in arrests after adjustment where higher (darker) values indicate areas of increased risk. All estimates are transformed so that they can be interpreted as a multiplicative change in the relative risk of arrests. Tract population is indirectly adjusted for."
            } else {
                
                if (input$stat == 'Percent Arrested'){
                    
                   " The 'As a Percent of Total Arrests' adjustment displays the selected arrests counts divided by the total arrest counts times 100%."
                }
            }
        }
    })
})
