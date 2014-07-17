
## download training and test datasets
downloadfile<-function(){
        if (!file.exists("./PML")) dir.create ("./PML")
        myTrainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(myTrainUrl,"./PML/pml-training.csv")
        myTestUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(myTestUrl,"./PML/pml-testing.csv")       
}

## Build a model with "classe" as an outcome and other variables as predictors
pml<-function(){
        library(caret)
        
        ##read in training csv files by read.csv.
        data<-read.csv("./PML/pml-training.csv")
        
        ##subset recorded numeric sensor data and "classe" column
        sensordata<-data[,c(8:11,37:49,60:68,84:86,102,113:124,140,151:160)]
        
        ##Split sensor data sets into training set and cross validation set.
        ##Training data is used to training models and crossValidation is used 
        ##to cross validate to assess the out of sample error.
        inTrain<-createDataPartition(sensordata$classe,p=0.8,list=FALSE)
        training<-sensordata[inTrain,]
        crossValidation<-sensordata[-inTrain,]
        
        ## Fit sensordata with random forest "rf" method. The sensordata are preprocessed
        ## with knnImpute method to replace NA with near neigbor average.
        modelFit<-train(classe~.,data=training,preProcess="knnImpute",
                        method="rf",trControl=trainControl(method="oob"))
        ##Predict the classe outcome with the model built with "rf" method using
        ##the cross validation data set. The out of sample error is assessed.
        predictions<-predict(modelFit,crossValidation)
        confusionMatrix(predictions,crossValidation$classe)
        
        ##read in tesing csv files by read.csv.
        dataTest<-read.csv("./PML/pml-testing.csv")
        
        ##subset numeric sensor data and "classe" column.
        sensordataTest<-dataTest[,c(8:11,37:49,60:68,84:86,102,113:124,140,151:160)]
        ##Use the built model to predict the classe outcome of the test data set.
        predictTest<-predict(modelFit,sensordataTest)
        predictTest
}

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("./PML/problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}