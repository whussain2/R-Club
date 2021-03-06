#==========================R CLUB scripts: Basic Data Visualization==================#
# Name: Waseem hussain
# Dated: 12-04-2018
#====================================================================================
# Load the required packages if not installed
      packages = c("ggplot2", "dplyr", "grid", "ggthemes","plotly", "forcats", "car")
# Create a function which will install the package if it is not installed
      package.check <- lapply(packages, FUN = function(x) { # apply lapply to list of packages
        if (!require(x, character.only = TRUE)) {
          install.packages(x, dependencies = TRUE) # install dependencies if required
          library(x, character.only = TRUE) # Load the package
        }
      })

  rm(list=ls()) # remove any object
# now read the data, will be working with water use data collected at 20 points for 356 rice genotypes
      data1<-read.csv(file="~/Box/Postdoc/R_club/data_files/data1.csv", header = TRUE)
      str(data1) # displays structure of data
# convert timepoint to factor
      data1$timepoint<-as.factor(data1$timepoint)
      str(data1)
      head(data1[1:6,])  # Visualize the data with first 6 rows and all columns
# Let us draw first the boxplot for water use data
        #png(file="~/Box/Postdoc/R_club/plots/boxplot.png", width=10, height =6, units = 'in',res=300) 
        p<-ggplot(data1, aes(x=timepoint, y=wu))+ 
          geom_boxplot(aes(fill=timepoint))+ # fill by timepoint to give different color
          #scale_fill_manual(values = c("", ""))+
          #scale_color_manual(values = c("", ""))+
          theme_classic()+ #choose the theme for background
          labs(title="Water use Data Trend Across the Time points",x="Time Point", y = "Trait Value")+ # Add the labels to the plots
          theme (plot.title = element_text(color="black", size=14, face="bold",hjust=0.5), # add and modify the title to plot
                 axis.title.x = element_text(color="black", size=12, face="bold"), # add and modify title to x axis
                 axis.title.y = element_text(color="black", size=12, face="bold")) + # add and modify title to y axis
          theme(axis.text= element_text(face = "bold", color = "black", size = 10))+ # modify the axis text
          theme(legend.position="none") # remove the theme from the plot
          #aes(x = fct_inorder(timepoint))+ # order the levels
          #dev.off()
          ggplotly(p)
# Violin plot
        p1<-ggplot(data1, aes(x=timepoint, y=wu)) +
          geom_violin(aes(fill = timepoint), trim = FALSE)+ 
          geom_boxplot(aes(fill = timepoint), width = 0.3)+
          theme_classic()+
          labs(title="Water use Data Trend Across the Time points",x="Time Point", y = "Trait Value")+
          theme (plot.title = element_text(color="black", size=14, face="bold",hjust=0.5),
                 axis.title.x = element_text(color="blue", size=12, face="bold"),
                 axis.title.y = element_text(color="blue", size=12, face="bold")) +
          theme(axis.text= element_text(face = "bold", color = "black", size = 10))+
          theme(legend.position="none")
          #aes(x = fct_inorder(timepoint))+ # order the levels
          ggplotly(p1)  
# Density plot
         p2<- ggplot(data=data1, aes(wu, fill = timepoint)) +
            geom_density(alpha = 0.4)+
            #geom_density(position = "stack")+
            theme_classic()+
            labs(title="Density Plot Across All the Timepoints",x="Time Point", y = "Trait Value")+
            theme (plot.title = element_text(color="black", size=14, face="bold",hjust=0),
                   axis.title.x = element_text(color="black", size=12, face="bold"),
                   axis.title.y = element_text(color="black", size=12, face="bold")) +
            theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
            theme(legend.title = element_text(colour="black", size=16, face="bold"), legend.position = "right",
                  legend.text = element_text(colour="black", size=14, face="bold"))+ # add and modify the legends
                  guides(fill=guide_legend(title="Timepoints")) # add the name to legend title
          ggplotly(p2)%>%layout( legend=list(x=1,y=0)) # adjust legend position in interactive plot
          
# Histograms
# Filter the data for timepoint 1 using library dplyr
          tp_1<-data1 %>% filter(timepoint == 1) 
# Now draw the Histogram
          ggplot(data=tp_1, aes(wu)) +
          geom_histogram(breaks=seq(0.78, 1, by =.025), color="darkblue", fill="lightblue")+ # adjust x values and breaks
          geom_vline(aes(xintercept=mean(wu)), # adding the line to represent mean
                       color="darkred", linetype="dashed", size=1)+
          labs(title="",x="Value", y = "Count")+
          theme_classic()+
          theme (plot.title = element_text(color="black", size=14, face="bold", hjust=0),
                   axis.title.x = element_text(color="black", size=10, face="bold"),
                   axis.title.y = element_text(color="black", size=10, face="bold")) +
          theme(axis.text= element_text(face = "bold", color = "black", size = 8))
# Facet_wrap/grid plots by one varaible
          p3<-ggplot(data=data1, aes(wu, color=timepoint, fill=timepoint))+
          #geom_density(alpha = 0.5)+
          geom_histogram(fill="pink", color="black")+
          theme_few()+ #use white theme
          labs(title="",x="Value", y = "Count")+
          theme_classic()+
          theme (plot.title = element_text(color="black", size=14, face="bold", hjust=0.5),
                   axis.title.x = element_text(color="black", size=10, face="bold"),
                   axis.title.y = element_text(color="black", size=10, face="bold")) +
          theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
          theme(legend.position="none")+
          facet_wrap(~ timepoint, ncol = 4,nrow=5,scales = "free")+
          #facet_grid(~ timepoint,ncol=4,scales = "free")+
          theme(strip.text.x = element_text(size = 10,face="bold",colour = "black"))+ #adding theme and background to headings
          theme(strip.background = element_rect(fill = "lightblue", color = "black", size=1.5))
          #ggplotly(p3) 
# Faceit wrap by multiple variables
          data2<- cbind(data.frame(species    = c(rep("Wheat",nrow(data1)/2), rep("Rice",nrow(data1)/2))), data1)
# recode the timepoints same for wheat and rice
          data2$timepoint<- recode(data2$timepoint,"'11'='1'; '12'='2'; '13'='3';'14'='4'; '15'='5'; '16'='6';
                                     '17'='7'; '18'='8'; '19'='9'; '20'='10'")
          str(data2)
          head(data2[1:6,])
          #data1$timepoint <- factor(data1$timepoint, levels = c("1", "2", "3", "4", "5", "6","7", "8",
                                                               # "9", "10"))
# plot 
          
          p4<-ggplot(data=data2, aes(wu, color=timepoint, fill=timepoint))+
            geom_density(alpha = 0.5)+
           # geom_histogram(fill="pink", color="black")+
            theme_few()+ #use white theme
            labs(title="",x="Value", y = "Count")+
            theme_classic()+
            theme (plot.title = element_text(color="black", size=14, face="bold", hjust=0.5),
                   axis.title.x = element_text(color="black", size=10, face="bold"),
                   axis.title.y = element_text(color="black", size=10, face="bold")) +
            theme(axis.text= element_text(face = "bold", color = "black", size = 8))+
            theme(legend.position="none")+
            facet_grid(timepoint~species, scales = "free", space = "free")+ #
            theme(strip.text.x = element_text(size = 10,face="bold",colour = "black"))+ #adding theme and background to headings
            theme(strip.background = element_rect(fill = "lightblue", color = "black", size=1.5))
         # ggplotly(p4) 
          
# Bar grid plot, will be using rain data 
            rain<-read.csv(file="~/Box/Postdoc/R_club/data_files/rainfall.csv", header = TRUE, stringsAsFactors = FALSE)
            str(rain)  
# covert location into factor
            rain$Location <- as.factor(rain$Location)
# maintain the order of the locations
            rain$Location <- factor(rain$Location, levels=unique(rain$Location))
# Barplot
            p5<- ggplot(rain, aes(x = Location, y = Precp, fill=Location))+
              scale_fill_manual(values=c("#CC79A7", "#009E73", "#e79f00", "#9ad0f3", 
                                         "#0072B2", "#D55E00"))+
              geom_bar(stat = "identity")+
              facet_wrap(~Year)+
              theme_few()+
              labs(title = "", x = "Location", y = "Precipitation (cm)")+
              theme (axis.title.x = element_text(color="black", size=20, face="bold"),
                     axis.title.y = element_text(color="black", size=20, face="bold")) +
              theme(axis.text= element_text(face = "bold", color = "black", size =12))+
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())+
              theme(legend.title = element_text(colour="black", size=24, face="bold"),
                    legend.position = c(0.84, 0.28), legend.text = element_text(colour="black", 
                                                                                size=18, face="bold"))+
              theme(strip.text.x = element_text(size = 16.5,face="bold",colour = "black"))+
              theme(strip.background = element_rect(fill = "white", color = "black", size=1.5))
            p5
            
#============================================END=========================================================##

        
        
        
        
        
