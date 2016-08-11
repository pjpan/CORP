library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$uideid <- DT::renderDataTable(DT::datatable({
    
    
    uid_info
    # uid_eid_mapping
    print(input$uid)
    # debug
   # uid_canditie<- c('_NOCARD13736699099')
    uid_canditie <- input$uid
    uid_eid_mapping <- cbind(uid_transform_total%>%
                               filter(num_uid %in% uid_canditie), eid_info%>%
                               select(Eid, age, worktime, zodiac_uid, Gender))
    
    # join上uid和eid之间的服务关系；
    uid_eid_mapping <- left_join(uid_eid_mapping, uid_op_services, by = c("num_uid"="uid", "Eid"="eid"), copy = F)
    
    # NA赋值成0
    uid_eid_mapping[is.na(uid_eid_mapping$score), "score"] <- 3
    uid_eid_mapping[is.na(uid_eid_mapping$times), "times"] <- 0
    
    #  Feature变化
    uid_eid_mapping <- uid_eid_mapping%>%
      mutate(isgendermatch = num_gender==Gender) # eid和uid的性别是否相同；
    uid_eid_mapping <- uid_eid_mapping%>%
      mutate(positivelevel= ifelse(score>3,1,ifelse(score<3,-1,0))) # 如果评分<3分，则为负推荐；
   
    uid_eid_mapping <- uid_eid_mapping%>%
     mutate(worktime2= worktime/20) # 如果评分<3分，则为负推荐；
    
   
   #    针对不同的客户群体，需要进行微调
   #   客户年龄段    员工年龄段 
   #    ≤1970         1980-1985
   #    1971-1980     1986-1990
   #    1981-1990     1991-1995
   #    ≥1991         1996-2000
    # 生成agegap;
    uid_eid_mapping <- uid_eid_mapping%>%
      mutate(agegap = (as.integer(num_age)-age)/100)
    
   uid_eid_mapping <- uid_eid_mapping%>%
     mutate(agegap = (as.integer(num_age)-age)/100)
   

   
   
   
   
   
   
        # 计算出最后的得分
  if(any(uid_eid_mapping$num_complaintimes>0 | uid_eid_mapping$grade==30 | uid_eid_mapping$num_isboss==1)){
    data <- uid_eid_mapping%>%
      mutate(totalscore = isgendermatch*weight_isgendermatch
             +score*weight_AvgUidScore
             +times*weight_Servertimes
             +worktime2*weight_workexperience
             +weight_zodiacuid*zodiac_uid
             +positivelevel*weight_positivelevel
             +agegap*weight_AgeGap)%>%
      arrange(desc(totalscore))%>%select(totalscore, num_uid, num_complaintimes, num_grade, num_age, num_gender, Eid, age, worktime, zodiac_uid, Gender) 
   } else {
      data <- uid_eid_mapping%>%
        mutate(totalscore = isgendermatch*weight_isgendermatch_low
               +score*weight_AvgUidScore_low
               +times*weight_Servertimes_low
               +worktime2*weight_workexperience_low
               +weight_zodiacuid_low*zodiac_uid
               +positivelevel*weight_positivelevel_low
               +agegap*weight_AgeGap_low)%>%
        arrange(desc(totalscore))%>%select(totalscore, num_uid, num_complaintimes, num_grade, num_age, num_gender, num_isboss, Eid, age, worktime, zodiac_uid, Gender)
    }
  
   names(data) <- c("totalscore", "UID", "UID_complaintimes", "UID_grade", "UID_age", "UID_gender", "UID_isboss","EID", "EID_age", "EID_workexperience", "EID_zodiacuids"," EID_Gender")   
   print(names(data)) 
   data
  
  }))
  
})

# shinyServer(function(input, output, session) {
#   # Logic to subset the data based on the placeIds selection
#   plotData <- reactive({
#     out <- dataSubset
#
#     # Subset the original data if the place selection is not empty
#     if(input$placeIdSel != ""){
#       out <- out[place_id == input$placeIdSel]
#     }
#     
#     # Add the time wrap factor
#     out[, timeHalf := factor(ifelse(time<input$wrapCutoffTime,
#                                     "First time part", "Last time part"))]
#     
#     out
#   })
#   
#   # Previous place logic
#   observeEvent(input$prevPlace,{
#     currentId <- which(input$placeIdSel == placeIds)
#     newId <- max(c(1, currentId - 1))
#     
#     # Update place
#     updateSelectInput(session, "placeIdSel", selected = placeIds[newId])
#   })
#   
#   # Next place logic
#   observeEvent(input$nextPlace,{
#     currentId <- which(input$placeIdSel == placeIds)
#     newId <- min(c(length(placeIds), currentId + 1))
#     
#     # Update place
#     updateSelectInput(session, "placeIdSel", selected = placeIds[newId])
#   })
#   
#   # Previous analysis variable logic
#   observeEvent(input$prevAnalysisVar,{
#     currentId <- which(input$analysisVar == analysisVars)
#     newId <- max(c(1, currentId - 1))
#     
#     # Update analysis variable
#     updateSelectInput(session, "analysisVar", selected = analysisVars[newId])
#   })
#   
#   # Next analysis variable logic
#   observeEvent(input$nextAnalysisVar,{
#     currentId <- which(input$analysisVar == analysisVars)
#     newId <- min(c(length(analysisVars), currentId + 1))
#     
#     # Update analysis variable
#     updateSelectInput(session, "analysisVar", selected = analysisVars[newId])
#   })
#   
#   # Generate the x-y plot for the plot data
#   output$xyPlotly <- renderPlotly({
#     if(input$xyType == "Observations" && input$placeIdSel == "") return()
#     plotData <- plotData()
#     
#     # Generate the ggplot based on the plot type selection
#     if(input$xyType == "Density"){
#       p <- ggplot(plotData, aes_string(x="x", y="y", z=input$analysisVar)) +
#         stat_summary_2d(fun = mean, bins = input$nbXYBins)
#       
#       if(!is.factor(plotData[[input$analysisVar]])){
#         p <- p + scale_color_viridis()
#       }
#     } else{
#       p <- ggplot(plotData, aes_string(x="x", y="y",
#                                        colour=input$analysisVar))
#       
#       if(is.factor(plotData[[input$analysisVar]])){
#         p <- p +
#           geom_point(aes_string(shape=input$analysisVar))
#       } else{
#         p <- p +
#           geom_point() +
#           scale_color_viridis()
#       }
#     }
#     
#     # Optionally wrap vs time
#     if(input$wrapTime){
#       p <- p +
#         facet_wrap(~timeHalf,nrow=2)
#     }
#     
#     # Convert to plotly format
#     ggplotly(p)
#   })
#   
#   # Reactive calculation of the maximum number of target bins
#   maxTargetBins <- reactive({
#     if(input$analysisVar %in% c("hour", "day")){
#       out <- 1 + max(plotData()[[input$analysisVar]]) -
#         min(plotData()[[input$analysisVar]])
#     } else{
#       out <- Inf
#     }
#     
#     out
#   })
#   
#   # Generate the density plot for the plot data
#   output$densityPlotly <- renderPlotly({
#     plotData <- plotData()
#     
#     # Generate the ggplot based on the plot type selection
#     if(input$densityType == "Histogram"){
#       p <- ggplot(plotData, aes_string(input$analysisVar)) +
#         geom_histogram(bins = min(c(maxTargetBins(),
#                                     input$nbDensityBins)))
#     } else{
#       p <- ggplot(plotData, aes_string(input$analysisVar)) +
#         geom_density()
#     }
#     
#     # Restrict x axis for accuracy density plot
#     if(input$analysisVar == "accuracy"){
#       p <- p +
#         xlim(c(0,input$maxAcDensPlot))
#     }
#     
#     # Optionally wrap vs time
#     if(input$wrapTime){
#       p <- p +
#         facet_wrap(~timeHalf,nrow=2)
#     }
#     
#     # Convert to plotly format
#     ggplotly(p)
#   })
#   
#   # Generate the density plot for the plot data
#   output$accuracyDensityPlotly <- renderPlotly({
#     plotData <- plotData()
#     
#     if(input$accuracyDensityRemoveOutliers){
#       # Calculate outliers 
#       values <- plotData[[input$analysisVar]]
#       outliers <- values %in% boxplot.stats(values, coef=2)$out
#       plotData <- plotData[!outliers,]
#     }
#     
#     # Generate the ggplot based on the plot type selection
#     if(input$accuracyDensityType == "Histogram"){
#       intercepts <- plotData[,median(as.numeric(get(input$analysisVar))),
#                              by=accuracyGroup]
#       p <- ggplot(plotData, aes_string(input$analysisVar)) +
#         geom_histogram(bins = min(c(maxTargetBins(),
#                                     input$nbDensityBins))) +
#         geom_vline(data=intercepts, aes(xintercept = V1), colour="green")
#     } else{
#       p <- ggplot(plotData, aes_string(input$analysisVar)) +
#         geom_density()
#     }
#     
#     # Wrap vs accuracy group
#     p <- p +
#       facet_wrap(~accuracyGroup, ncol=1)
#     
#     # Convert to plotly format
#     ggplotly(p)
#   })
#   
#   # Generate the custom comparison plot
#   output$comparisonPlotly <- renderPlotly({
#     plotData <- plotData()
#     
#     if(input$compPlotType == "Scatter"){
#       p <- ggplot(plotData, aes_string(x=input$analysisVar,
#                                        y=input$comparisonVar,
#                                        col=input$colComparisonVar)) +
#         geom_point() +
#         scale_color_viridis()
#     } else{
#       plotData[,CompVar := factor(plotData[[input$analysisVar]])]
#       p <- ggplot(plotData, aes_string(x="CompVar",
#                                        y=input$comparisonVar,
#                                        fill="CompVar")) +
#         geom_violin() +
#         xlab(input$analysisVar) +
#         theme(legend.position="none")
#     }
#     
#     # Optionally wrap vs time
#     if(input$wrapTime){
#       p <- p +
#         facet_wrap(~timeHalf,nrow=2)
#     }
#     
#     # Convert to plotly format
#     ggplotly(p)
#   })
#   
#   # Generate a data table of the plot data
#   output$plotDataTable <- renderDataTable({
#     out <- plotData()
#     
#     out
#   }, options = list(pageLength = 10,
#                     lengthMenu = list(c(5, 10, 20, -1),
#                                       c('5', '10', '20', 'All'))),
#   filter = "top"
#   )
# })