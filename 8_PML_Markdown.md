PML Markdown
================
Faraz Syed
August 1, 2018

Practical Machine Learning: Prediction Assignment
-------------------------------------------------

This assignment will use data from an experiment which used motion trackers to track how participants performed Unilateral Biceps Curls 5 different ways (A,B,C,D,E) with 'A' being the correct specified way.

This assignement will use the data produced by the sensors to train a random forest model for the 'classe' dependent variable. The model will then use to test data to predict which set of movements was performed correctly. We will partition the data to check for the model's initial accurcy and selection of independent variables.

Our task is train the model using the training data set and predict which of the 5 different ways the exercise was performed.

Getting Data
------------

The data was sourced from the Groupware website (link below in R code)

``` r
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "./data/8_training.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "./data/8_test.csv")
```

``` r
training = read.table("C:/Users/farsyed/Documents/R/Working Dir/data/8_training.csv", header=TRUE, sep=",", na.strings=c("NA", "#DIV/0!"))
test = read.table("C:/Users/farsyed/Documents/R/Working Dir/data/8_test.csv", header=TRUE, sep=",", na.strings=c("NA", "#DIV/0!"))
```

Exploring this data we notice that there are a lot of NA's and columns that will not be used. We want to find the the variables that we will use to predict. The assignment refers to "accelerometers on the belt, forearm, arm, and dumbell". We will now clean the data by removing columns with NA and keeping columns referencing variables with belt, forearm, arm and dumbell.

``` r
population = grep("_belt|_arm|_dumbbell|_forearm", names(training))
training2 = training[,c(population,160)]

na.data = is.na(training2)
NAcols=which(colSums(na.data) > 19000)
training2 = training2[,-NAcols]

barplot(table(training2$classe), ylab="Frequency", xlab=deparse(training2$classe), 
        main="Frequency of Classe - Training")
```

![](8_PML_Markdown_files/figure-markdown_github/unnamed-chunk-3-1.png) A distribution of the classe variable in the training set - majority of movements seem to be performed correctly :)

We are left with a data set which now has 53 indpendent variables which can be used to determine the exercise class. We now test the variables for the variances amongst them.

Building the Model with Random Forest using cross validation with the testing data set
--------------------------------------------------------------------------------------

we will partition the data 75%/25% for the cross validation and check the results for the out of sample accuracy.

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
set.seed(2018)

sample.train = createDataPartition(y=training2$classe, p=0.75, list=FALSE)
training3 = training2[sample.train,]
testing = training2[-sample.train,]
```

Training the Model
------------------

``` r
time1=proc.time()
randForest = randomForest(classe~., data=training3, ntree=500)
```

Applying the Model to the partitioned Testing data and checking for out of sample accuracy
------------------------------------------------------------------------------------------

``` r
prediction.test = predict(randForest, newdata=testing)

confusionMatrix(prediction.test, testing$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1393    2    0    0    0
    ##          B    2  946    6    0    0
    ##          C    0    1  849   11    1
    ##          D    0    0    0  793    0
    ##          E    0    0    0    0  900
    ## 
    ## Overall Statistics
    ##                                         
    ##                Accuracy : 0.9953        
    ##                  95% CI : (0.993, 0.997)
    ##     No Information Rate : 0.2845        
    ##     P-Value [Acc > NIR] : < 2.2e-16     
    ##                                         
    ##                   Kappa : 0.9941        
    ##  Mcnemar's Test P-Value : NA            
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9986   0.9968   0.9930   0.9863   0.9989
    ## Specificity            0.9994   0.9980   0.9968   1.0000   1.0000
    ## Pos Pred Value         0.9986   0.9916   0.9849   1.0000   1.0000
    ## Neg Pred Value         0.9994   0.9992   0.9985   0.9973   0.9998
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2841   0.1929   0.1731   0.1617   0.1835
    ## Detection Prevalence   0.2845   0.1945   0.1758   0.1617   0.1835
    ## Balanced Accuracy      0.9990   0.9974   0.9949   0.9932   0.9994

The diagonal of the matrix shows the correct predictions in the training, and the off-diagonals represent the incorrect preditions. This model has a high Out of Sample accuracy rate of 99.53%. This should be a good predictor to use on the test data.

Applying the Model to the original 20 case test data
----------------------------------------------------

First we get clean the test data to only keep the relevant columns.

``` r
population = grep("_belt|_arm|_dumbbell|_forearm", names(test))
length(population)
```

    ## [1] 152

``` r
test2 = test[,c(population,160)]

na.data = is.na(test2)
NAcols=which(colSums(na.data) > 19)
test3 = test2[,-NAcols]
```

Now we apply the model to the testing data set for our prediction. Results were correctly identified in the quiz 20/20.

``` r
prediction.final = predict(randForest, newdata=test3)
```
