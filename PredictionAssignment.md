Prediction Assignment Writeup (Binit Malla)
========================================================
**Background:**
-------------------------------------------------------
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

**Goal:**
-------------------------------------------------------
The goal of this project is to create a model to predict the manner, "classe" variable in the training set, in which participants performed barbell lifts using the data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.


**Data Step:**
-------------------------------------------------------

Training and test data sets obtained from the following sources:

  - Training dataset:   https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

  - Testing dataset:   https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

Data originated from :	http://groupware.les.inf.puc-rio.br/har



```r
###1--Create data folder if it does not exist

if (file.exists('data')){
} else {
  dir.create(file.path(getwd(),'data'))
}
```

```
## NULL
```

```r
###2--Download data files

if (!file.exists("data/pml-training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "data/pml-training.csv")
}
if (!file.exists("data/pml-testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "data/pml-testing.csv")
}

training<-read.csv("data/pml-training.csv",header=TRUE,sep=",",na.strings=c("NA", "#DIV/0!"))
testing<-read.csv("data/pml-testing.csv",header=TRUE,sep=",",na.strings=c("NA", "#DIV/0!"))
```


**Cleaning Fields:**
-------------------------------------------------------
The following steps were taken to clean the fields:

- Columns with more than 80% NAs removed from the data. There are about 100 columns that dropped out in the training dataset.
- The first seven columns did not include any quantitative information that could be used to predict the "classe" variable and, hence, were also removed.
- The remaining columns, except for "Classe", were converted to numeric type.



```r
###3--Function that performs the cleaning

Clean_data<-function(data){

  ###Remove columns with more than 80% of rows with NAs
  
  col_remove=c()
  for (i in 1:ncol(data)){
  
    if (sum(is.na(data[,i]))/nrow(data)>0.80){
        col_remove <-c(col_remove,i)
    }
  }

  data<-data[,-col_remove]


  ###Remove the first 7 columns that are not quantitative values that can be used in the model
  
  data<-data[,-c(1:7)]
  

  ###Convert all columns to numeric type except last column
  
  for (i in 1:(ncol(data)-1)){
    data[,i]<-as.numeric(data[,i])
  }

  invisible(data)                 
}
```

**Data Splitting:**
-------------------------------------------------------
  
Once cleaned, the training dataset is split into train_set and test_set. Train_set is used to build the model and test_set is used to evaluate the strength of the model.  The data is split into 75/25.



```r
#Load Caret Library
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.1
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
set.seed(10000)


#Clean training dataset and split into training and test sets
training<-Clean_data(training)
inTrain<-createDataPartition(y=training$classe,p=0.75,list=F)

#Prepare training and test set
train_set<-training[inTrain,]
test_set<-training[-inTrain,]
```


**PreProcessing:**
-------------------------------------------------------
If there are still NA values in the data set, the values were imputed using the "KnnImpute" method. The features were also standardized and reduced using PCA.



```r
###Impute if there are still any missing values using knnImpute method; standarize features and reduce features using PCA

pre_Object<-preProcess(train_set[,-ncol(train_set)],method=c("center","scale","knnImpute","pca"))
pre_results<-predict(pre_Object,train_set[,-ncol(train_set)])
```



**Prediction Model:**
-------------------------------------------------------

Once the data is cleaned and preprocessed as described above, it is used to build a prediction model using the "Knn" method and the model is evaluated on the testing data.



```r
#Model Training
model<-train(train_set$classe ~.,data=pre_results,method="knn")

#Testing the model on test data
test_results<-predict(pre_Object,test_set[,-ncol(test_set)])

#Output Results
confusionMatrix(test_set$classe,predict(model,test_results))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1376   11    3    5    0
##          B   23  895   29    1    1
##          C    6   17  817   13    2
##          D    4    1   41  756    2
##          E    0    8   16    9  868
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9608         
##                  95% CI : (0.955, 0.9661)
##     No Information Rate : 0.2873         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9505         
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9766   0.9603   0.9018   0.9643   0.9943
## Specificity            0.9946   0.9864   0.9905   0.9883   0.9918
## Pos Pred Value         0.9864   0.9431   0.9556   0.9403   0.9634
## Neg Pred Value         0.9906   0.9906   0.9780   0.9932   0.9988
## Prevalence             0.2873   0.1900   0.1847   0.1599   0.1780
## Detection Rate         0.2806   0.1825   0.1666   0.1542   0.1770
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9856   0.9734   0.9461   0.9763   0.9930
```

The model has an accuracy of 96.1%, hence, out of sample error of about 3.9%.


**Test Set Classification**
-------------------------------------------------------
Once the model was prepared, the test set was cleaned and classified using the prediction model. The resulting classification are shown below. The results were saved into the text files for submission assignment.


```r
#5--Running the model on testing dataset

testing<-Clean_data(testing)
test<-predict(pre_Object,testing[,-ncol(testing)])
predict_output<-predict(model,test)
print(predict_output)
```

```
##  [1] B A C A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
#Saving the results into text files.

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predict_output)
```
