##################################################################################
#                  Installing and loading Packages                               #
##################################################################################
myPackages <- c("readr" #Read CSV File
                #,"dplyr" #função if_else
                ,"caret" #Confusion Matrix
                ,"pROC" #ROC Curve Visualization
                ,"rpart","rpart.plot" #Estimate and Visualize Decision Tree
                ,"unbalanced" #Balancear Dataset
                ,"kableExtra" #Table visualization
                ,"knitr" #Table visualization
)

if(sum(as.numeric(!myPackages %in% installed.packages())) != 0){
  pack_install <- myPackages[!myPackages %in% installed.packages()]
  for(i in 1:length(pack_install)) {
    install.packages(pack_install, dependencies = T)
    break()}
  sapply(myPackages, require, character = T) 
} else {
  sapply(myPackages, require, character = T) 
}

##################################################################################