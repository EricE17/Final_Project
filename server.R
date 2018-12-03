
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(input,output){

   
#Plot the data

     output$futureplot <- renderPlot({plotdata <- subset(merged,site_no==input$site &
                                                            Date >= input$futuredates[1] &
                                                            Date <= input$futuredates[2])
     #Checked box logic
     if (input$checkbox==TRUE){
       
       ggplot()+
         geom_line(data=merged, aes(x=merged$Date, y=merged$Discharge), color='blue') +
         geom_line(data=merged,aes(x=merged$Date, y=merged$SWE), color='red')
       
     }else{
       
       ggplot()+
         geom_line(data=merged,aes(x=merged$Date, y=merged$Discharge), color='blue') 
     }
   })

     
#box plots
    #box plot1 and box plot2
    output$boxplot1 <- renderPlot({
       
       l <- ggplot(merged, aes(month, SWE)) + 
            geom_boxplot(fill = "Blue") 
       l
    })      
       
       output$boxplot2 <- renderPlot({
         
       p <- ggplot(merged, aes(month, Discharge)) + 
            geom_boxplot(fill ="Red")  
       p
    })
       
# comparison plot between SWE and discharge
       
       output$SWEdischarge <- renderPlot({
         
         mp <- ggplot(filteredmerged,aes(x=mergeddischarge,y=mergedSWEdrop),
                      size=20) +
                      geom_jitter(colour=alpha("Blue",.15), size=3.25) +
                      labs(x="Discharge, cfs",y="Drop in SWE Between Days, inches",
                      title="Alignment of SWE Drop to Discharge",
                      subtitle = "Location: Big Cottonwood Stream") +
                      stat_smooth(method = "lm", col = "red")
                   ####  R-squared value needs to be added here  ####
         mp
         })
     
#Leaflet plot of Snotel and USGS gage location
     output$mymap <- renderLeaflet({
       m <- leaflet()
       m <- addTiles (m)
       m <- addMarkers(m, lng=-111.75138889, lat=40.62388889, label ="USGS: Stairs Gage Station 10168300" ,
                       labelOptions = labelOptions(noHide = T))
       m <- addMarkers(m, lng=-111.583333, lat=40.60, label = "Snotel: Brighton Site 366",
                       labelOptions = labelOptions(noHide = T)) 
       m
     })
 })

