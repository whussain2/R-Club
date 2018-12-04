#==========================R CLUB scripts: Correlation Heatmaps==================#
# Name: Waseem hussain
# Dated: 12-04-2018
#====================================================================================
# Load the required packages if not installed
      packages = c("ggplot2", "dplyr", "grid", "ggthemes","plotly", "forcats",
                   "reshape2", "corrplot","ggcorrplot","Hmisc")
# Create a function which will install the package if it is not installed
      package.check <- lapply(packages, FUN = function(x) { # apply lapply to list of packages
        if (!require(x, character.only = TRUE)) {
          install.packages(x, dependencies = TRUE) # install dependencies if required
          library(x, character.only = TRUE) # Load the package
        }
      })
      
      rm(list=ls()) # remove any object
      
# Now read the data, will be using wheat data for 13 traits with 204 observations
      corr<-read.csv(file="~/Box/Postdoc/R_club/data_files/corr.csv", header=TRUE)
      str(corr)
      table(is.na(corr))
      head(corr[1:5, ]) ## Extract first five and and all the columns

#Use library Hmisc to get correlation cofficients and p-matrix
#Correlation methods: ("pearson", "kendall", "spearman")***
      
# Let is create function to perform correlations
      mycorr<-function (data, type){
        result<-rcorr(as.matrix(data), type=type)
        result$r
      }
# Get the correlation cofficents
      # use function to get cor.cofficients, rounding results by two digits after point and
      # droping column first from corr data file as it contains names of the lines
      corr_mat_pe<-round(mycorr(corr[,-1], type="pearson"),2) 
      corr_mat_sp<-round(mycorr(corr[,-1], type="spearman"),2)
      head (corr_mat_pe[, 1:4])
      head (corr_mat_sp[, 1:4])
#now get matrix of p-values using cor_pmat function from package corrplot package
      pmat_pe<-  cor_pmat(corr_mat_pe)
      head(pmat_pe[, 1:4])
      pmat_sp<-  cor_pmat(corr_mat_sp)
      head(pmat_pe[, 1:4]) 
#Visualize the correlation matrix through heatmaps in ggcorrplot package
      myggcorr<-function(cor_cof, p.mat){
      ggcorrplot(cor_cof, method="circle",hc.order = FALSE,outline.col = "blue", 
                   type="lower", lab=TRUE,  p.mat =p.mat, insig = "blank",pch = 4, 
                   pch.col="black", pch.cex = 5,
                   show.diag = FALSE, lab_col = "black", lab_size = 2, sig.level =c(0.1,0.05,0.01),
                   tl.cex=10, tl.col="black", tl.srt=45, digits=2)
      }
# Now plot correlation heatmap method pearson using function myggcorr
      myggcorr(cor_cof=corr_mat_pe, p.mat=pmat_pe)
# Now plot correlation heat map method spearman
      myggcorr(cor_cof=corr_mat_sp, p.mat=pmat_sp)
      
# Now Visualize the correlation matrix through heatmaps in corrplot package
      
      mycorrplot<-function(corr_cof, p.mat){
        corrplot(corr_cof, p.mat = p.mat, insig = "label_sig",method="circle", type="upper",
                 sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white", 
                 tl.col = "black",tl.srt=45)
      }
# Now plot correlation heat map method pearson
      mycorrplot(corr_cof=corr_mat_pe, p.mat=pmat_pe)
# Now plot correlation heat map method spearman
      mycorrplot(corr_cof=corr_mat_sp, p.mat=pmat_sp)
      
      
#Correlation plot mixed
      mycorrmix<-function(corr_mix){
        corrplot.mixed(corr_mix, lower="number", upper="circle",lower.col = "black",bg="white",
                       number.cex = .7)
      }
# Now plot mixed correlation heat map method pearson
      mycorrmix(corr_mix=corr_mat_pe)
# Now plot mixed correlation heat map method spearman
      mycorrmix(corr_mix=corr_mat_sp)
      
#======================================END=================================================
        
      
    