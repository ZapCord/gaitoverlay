options(shiny.maxRequestSize=3000*1024^2)

## libraries
library(dplyr)
library(av)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(DT)
## functions
## vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

## server
server<-function(input,output,session){
  
  observeEvent(input$refresh,{shinyjs::js$refresh()})
  hash = new.env(hash = TRUE, parent = emptyenv())
  output$fileinputpanel<-renderPrint({ req(input$existing_data_input)})

  ## creating a hash table structure where 
  ## the key is the time for a force matrix value
  keys<-observe({
    req(input$existing_video_input)
    #req(input$existing_data_input)
    
    ## find current working directory and see if temp video storage exists
    ## if it does not exist, create it. If it does exist, delete it and recreate.
    workingdir<-getwd()
    print(workingdir)
    if(dir.exists(file.path(paste(workingdir,"/temp_video",sep="")))){
      unlink("temp_video", recursive = TRUE)
      dir.create("temp_video")
    } else{
      dir.create("temp_video")
    }
    av_video_images(toString(input$existing_video_input[4]), destdir = paste(workingdir,"/temp_video",sep=""), format = "jpg")
    images<-list.files(path="temp_video")
    updateSliderInput(session,"slider_time",max=length(images))
    # exist_data <- reactiveFileReader(1000,session,
    #                                  filePath=toString(input$existing_data_input[4]),
    #                                  readFunc=read.csv)
    # d1_matrix<-exist_data()
    # d1_matrix<-d1_matrix[!names(d1_matrix) %in% c("X")]
    # 
    # updateSliderInput(session,"slider_time",max=nrow(d1_matrix))
    # updateSliderInput(session,"slider_range",max=nrow(d1_matrix),value=c(1,nrow(d1_matrix)))
    # data<-matrix(nrow=10,ncol=16)
    # for(j in 1:nrow(d1_matrix)){
    #   for(i in 1: ncol(d1_matrix)){
    #     if(i==1){
    #       time<-d1_matrix[j,i]
    #       row_num<-1
    #       col_num<-1
    #       next
    #     }else{
    #       data[row_num,col_num]<-d1_matrix[j,i]
    #       if(row_num < 10){
    #         row_num=row_num+1
    #       }else{
    #         row_num=1
    #         col_num=col_num+1
    #       }
    #       
    #     }
    #     
    #   }
    #   hash[[paste(time)]]<-data
    # }
    # as.character(c(sort(as.numeric(ls(hash)))))
    
  })
  
  output$image<-renderImage({
    req(input$existing_video_input)
    images<-list.files(path="temp_video")
    # render the image based on where the slider time is.
    pull_frame<-images[input$slider_time]
    filename <- normalizePath(file.path(paste(getwd(),"/temp_video",sep=""),
                                        pull_frame))
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", pull_frame))

  }, deleteFile = FALSE)
  
  ## calculating the heatmap and 
  ## long format data table for a chosen time index
  # observe({
  #     data_m<-unlist(get_hash(keys()[input$slider_time],hash))
  #     data_df<-matrix(nrow=10,ncol=16)
  #     row_num<-1
  #     col_num<-1
  #     for(i in 1:length(data_m)){
  #       data_df[row_num,col_num]<-data_m[i]
  #       if(row_num < 10){
  #         row_num=row_num+1
  #       }else{
  #         row_num=1
  #         col_num=col_num+1
  #       }
  #     }
  #     
  #     colnames(data_df)<-c("C1","C2","C3","C4","C5","C6","C7","C8",
  #                          "C9","C10","C11","C12","C13","C14","C15","C16")
  #     rownames(data_df)<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10")
  #     longData<-melt(data_df)
  #     names(longData)<-c("Rows","Columns","Force")
  #     output$table<-DT::renderDT(longData)
  #     
  #     g<-reactive({
  #       if(length(input$checkGroup)==1){
  #         ggplot(longData, aes(x = Columns, y = Rows)) + 
  #           geom_raster(aes(fill=Force),interpolate = TRUE) +
  #           scale_fill_gradient(low="grey90", high="red") +
  #           labs(x="Columns", y="Rows", title=
  #                  paste("Scooter Force:",input$test,",",
  #                        input$subject,",","Time:",keys()[input$slider_time],"s",sep=" ")) +
  #           theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
  #                              axis.text.y=element_text(size=9),
  #                              plot.title=element_text(size=11))
  #       }else{
  #         ggplot(longData, aes(x = Columns, y = Rows)) + 
  #           geom_raster(aes(fill=Force)) +
  #           scale_fill_gradient(low="grey90", high="red") +
  #           labs(x="Columns", y="Rows", title=
  #                  paste("Scooter Force:",input$test,",",
  #                        input$subject,",","Time:",keys()[input$slider_time],"s",sep=" ")) +
  #           theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
  #                              axis.text.y=element_text(size=9),
  #                              plot.title=element_text(size=11))
  #       }
  #     })
  #     
  #     output$plot<-renderPlot({g()})
  #     
  #   
  # })
  
  
}