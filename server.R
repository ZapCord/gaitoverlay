## Tony Johnson
## Last Modified: 6/11/2021
## UCHealth Motion Capture Lab

## Max upload size for files is 3GB
options(shiny.maxRequestSize=3000*1024^2)

## libraries
library(dplyr)
library(av)
library(tidyr)
library(stringr)
library(reshape2)
library(data.table)
library(ggplot2)
library(jpeg)

## functions
## vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

## server
server<-function(input,output,session){

  observeEvent(input$refresh,{shinyjs::js$refresh()})
  hash = new.env(hash = TRUE, parent = emptyenv())
  output$fileinputpanel<-renderPrint({ df<-rbind(input$existing_video_input,
                                                 input$existing_data_input)
                                      df
                        })

  keys<-observe({
    req(input$existing_video_input)

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

    ## update the slider for time based on the number of frames in the video
    updateSliderInput(session,"slider_time",max=length(images))

  })

  ## Show the image from the video reactive based on slider time.
  output$image<-renderImage({
      req(input$existing_video_input,input$dimension)
      images<-list.files(path="temp_video")
      ## render the image based on where the slider time is.
      pull_frame<-images[input$slider_time]
      filename <- normalizePath(file.path(paste(getwd(),"/temp_video",sep=""),
                                          pull_frame))
      image_dim <- dim(readJPEG(filename))
      image_ratio <- image_dim[2]/image_dim[1]
      ## Return a list containing the file name and alt text
      list(src = filename,
           width = round(input$dimension[1]*0.4),
           height = round(input$dimension[1]*0.4/image_ratio),
           alt = paste("Image number", pull_frame))

  }, deleteFile = FALSE)

  ## grab the file path for the uploaded csv
  filename<-reactive({
    req(input$existing_data_input)
    input$existing_data_input[4]
    })

  ## read the uploaded csv at the file path.
  exist_data<-reactive({
    req(filename())
    fread(toString(filename()))
  })

  change_names<-observe({
    req(is.null(exist_data())==FALSE)
    ## Find the available graphs from the csv
    column_names<-colnames(exist_data())
    select_names<-str_split(column_names,"[LR]")
    change_names<-c()
    for(i in 1:length(select_names)){
      change_names<-c(change_names,select_names[[i]][2])
    }

    ## update the drop down to reflect the available graphs
    change_names<-unique(change_names)
    updateSelectInput(session,"graph_name", choices = change_names[seq(1,length(change_names)-3)])
  })

  ## create a data table where a time vector in frames,
  ## along with the left and right kinematic/kinetic data is stored
  ## for graphing
  df<-reactive({
    req(input$graph_name!="plac")
    leftname<-paste0("Norm_L",input$graph_name, collapse="")
    rightname<-paste0("Norm_R",input$graph_name, collapse="")
    data_cols<-colnames(exist_data())
    data <- exist_data()
    LGC <- c(data$LGCStart[1], data$LGCEnd[1])
    RGC <- c(data$RGCStart[1], data$RGCEnd[1])
    ## find difference in frame times to get total gait cycle times in frames
    LGCdiff<-(LGC[2]-LGC[1]+1)
    RGCdiff<-(RGC[2]-RGC[1]+1)
    timevec<-seq_len(nrow(data %>% select(leftname)))
    data<-cbind(timevec, data %>% select(leftname, rightname))
    colnames(data) <- c("Time", leftname, rightname)

    ## interpolate back to the original frame length
    ## so that video can be compared to kinematics directly
    ## output in long format for easy graphing of left vs right
    if(LGCdiff < RGCdiff){
      d<-cbind(seq_len(RGCdiff),y=data %>% select(leftname))
      colnames(d)<-c("time",leftname)

      left_orig<-approx(x=d %>% select(leftname),n=LGCdiff)
      for(i in 1:abs(RGCdiff-LGCdiff)){
        left_orig$y<-c(left_orig$y,NA)
      }
      orig_data<-cbind(timevec,left_orig$y,data %>% select(rightname))
      colnames(orig_data)<-c("Time",leftname,rightname)

    } else if(RGCdiff < LGCdiff){
      d<-cbind(seq_len(LGCdiff),y=data %>% select(rightname))
      colnames(d)<-c("time",rightname)

      right_orig<-approx(x=d %>% select(rightname),n=RGCdiff)
      for(i in 1:abs(RGCdiff-LGCdiff)){
        right_orig$y<-c(right_orig$y,NA)
      }
      orig_data<-cbind(timevec,data %>% select(leftname),right_orig$y)
      colnames(orig_data)<-c("Time",leftname,rightname)

    }

    ## extend the frames back to frame dependent
    ## so that it can be true to the video
    ## for the chosen valid steps
    new_L<-orig_data %>% select(leftname) %>% drop_na()
    new_R<-orig_data %>% select(rightname) %>% drop_na()
    if(LGC[1]<RGC[1]){
      for(i in 1:abs(RGC[1]-LGC[1])){
        new_R<-rbind(NA,new_R,use.names=FALSE)
      }
    }else if(LGC[1]>RGC[1]){
      for(i in 1:abs(LGC[1]-RGC[1])){
        new_L<-rbind(NA,new_L,use.names=FALSE)
      }
    }
    if(LGC[2]<RGC[2]){
      for(i in 1:abs(RGC[2]-LGC[2])){
        new_L<-rbind(new_L,NA,use.names=FALSE)
      }
    }else if(LGC[2]>RGC[2]){
      for(i in 1:abs(LGC[2]-RGC[2])){
        new_R<-rbind(new_R,NA,use.names=FALSE)
      }
    }

    orig_data<-cbind(seq_len(nrow(new_L)),new_L,new_R)
    colnames(orig_data)<-c("Time",leftname,rightname)
    longData<-as.data.table(melt(orig_data, id=c("Time"),na.rm=FALSE),na.rm=FALSE)
    setnames(longData,names(longData)[2:3],c("Variable","Value"))

  })

  ## render the data table created above
  output$kinematics_csv<-renderDataTable({
    df()
  })

  output$plot<-renderPlot({
      #req(input$existing_video_input)
      longData<-df()
      if(input$graph_name=="PelvisX"){
        ylabel = expression("Post"~'('*degree*')'~"Ant")
        title = "Pelvic Tilt"
      } else if(input$graph_name=="PelvisY"){
        ylabel = expression("Down"~'('*degree*')'~"Up")
        title = "Pelvic Obliquity"
      } else if(input$graph_name=="PelvisZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Pelvic Rotation"
      } else if(input$graph_name=="KneeX"){
        ylabel = expression("Ext"~'('*degree*')'~"Flex")
        title = "Knee Flexion/Extension"
      } else if(input$graph_name=="KneeY"){
        ylabel = expression("Val"~'('*degree*')'~"Var")
        title = "Knee Varus/Valgus"
      } else if(input$graph_name=="KneeZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Knee/Tibia Rotation"
      } else if(input$graph_name=="HipX"){
        ylabel = expression("Ext"~'('*degree*')'~"Flex")
        title = "Hip Flexion/Extension"
      } else if(input$graph_name=="HipY"){
        ylabel = expression("Abd"~'('*degree*')'~"Add")
        title = "Hip Abduction/Adduction"
      } else if(input$graph_name=="HipZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Hip/Femoral Rotation"
      }else if(input$graph_name=="AnkleX"){
        ylabel = expression("Plan"~'('*degree*')'~"Dors")
        title = "Ankle Plantarflexion/Dorsiflexion"
      } else if(input$graph_name=="AnkleY"){
        ylabel = expression("Abd"~'('*degree*')'~"Add")
        title = "Ankle Abduction/Adduction"
      } else if(input$graph_name=="AnkleZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Ankle Rotation"
      } else if(input$graph_name=="FootProZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Foot Progression"
      } else if(input$graph_name=="HipMXD"){
        ylabel = expression("Ext (Nm/kg) Flex")
        title = "Hip Flexion/Extension Moment"
      } else if(input$graph_name=="HipMYD"){
        ylabel = expression("Abd (Nm/kg) Add")
        title = "Hip Abduction/Adduction Moment"
      } else if(input$graph_name=="HipMZD"){
        ylabel = expression("Ext (Nm/kg) Int")
        title = "Hip Rotation Moment"
      } else if(input$graph_name=="KneeMXD"){
        ylabel = expression("Ext (Nm/kg) Flex")
        title = "Knee Flexion/Extension Moment"
      } else if(input$graph_name=="KneeMYD"){
        ylabel = expression("Valg (Nm/kg) Var")
        title = "Knee Varus/Valgus Moment"
      } else if(input$graph_name=="KneeMZD"){
        ylabel = expression("Ext (Nm/kg) Int")
        title = "Knee Rotation Moment"
      } else if(input$graph_name=="AnkMXD"){
        ylabel = expression("Plan (Nm/kg) Dors")
        title = "Ankle Dorsiflexion/Plantarflexion Moment"
      } else if(input$graph_name=="AnkMYD"){
        ylabel = expression("Abd (Nm/kg) Add")
        title = "Ankle Abduction/Adduction Moment"
      } else if(input$graph_name=="AnkMZD"){
        ylabel = expression("Ext (Nm/kg) Int")
        title = "Foot Rotation Moment"
      } else if(input$graph_name=="AnkPZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Total Ankle Power"
      } else if(input$graph_name=="KneePZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Total Knee Power"
      } else if(input$graph_name=="HipPZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Total Hip Power"
      } else if(input$graph_name=="AnkPZZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Rotational Ankle Power"
      } else if(input$graph_name=="KneePZZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Rotational Knee Power"
      } else if(input$graph_name=="HipPZZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Rotational Hip Power"
      } else if(input$graph_name=="AnkPX"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Dorsiflexion/Plantarflexion Ankle Power"
      } else if(input$graph_name=="KneePX"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Flexion/Extension Knee Power"
      } else if(input$graph_name=="HipPX"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Flexion/Extension Hip Power"
      } else if(input$graph_name=="AnkPY"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Abduction/Adduction Ankle Power"
      } else if(input$graph_name=="KneePY"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Varus/Valgus Knee Power"
      } else if(input$graph_name=="HipPY"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Abduction/Adduction Hip Power"
      } else{
        ylabel = "Variable"
        title = "Variable vs Time"
      }


      g<-ggplot(data=longData, aes(x=Time, y=Value, group=Variable, color=Variable)) +
        geom_line()+
        scale_color_manual(values=c('#DC143C','#14C108'))+
        geom_hline(yintercept=0, color="black")+
        labs(x = "Time (Frames)", y = ylabel, title = title)+ theme_bw()

      frame<-input$slider_time*input$ratio
      data <- exist_data()
      LGC <- c(data$LGCStart[1], data$LGCEnd[1])
      RGC <- c(data$RGCStart[1], data$RGCEnd[1])
      if(LGC[1]<RGC[1]){
        start<-LGC[1]
      }else{
        start<-RGC[1]
      }
      if(LGC[2]<RGC[2]){
        end<-RGC[2]
      }else{
        end<-LGC[2]
      }
      if(frame <= start){
        g<-g+geom_vline(xintercept=0, color="blue")
      } else if(frame >= end){
        g<-g+geom_vline(xintercept=end-start, color="blue")
      } else{
        g<-g+geom_vline(xintercept=frame-start, color="blue")
      }
      g
  })
  
  output$info <- renderPrint({
    req(input$graph_name!="plac")
    if(!is.null(input$plot_brush)){
      newdf<-brushedPoints(df()
               , input$plot_brush, xvar = "Time", yvar = "Value")
      newdf<-cbind(c("Max","Min"),rbind(newdf[which.max(newdf$Value)],newdf[which.min(newdf$Value)]))
      names(newdf)<-c("Type","Time","Variable","Value")
      newdf
    } else{
      nearPoints(df()
               , input$plot_click, xvar = "Time", yvar = "Value", threshold = 10, maxpoints = 1)
    }
    
  })



}
