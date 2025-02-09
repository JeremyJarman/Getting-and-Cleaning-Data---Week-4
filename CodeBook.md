---
title: "CodeBook.md"
author: "Jeremy Jarman"
date: "22 April 2017"
output: html_document
---

# Code book

This CodeBook describes the variables and transformations carried out to achieve the "summary.txt" data set, which was generated by running the script "run_analysis.R"

The data was sourced from : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
and originally fully described here : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Summary data 

The data produced by the run_analysis script is a set of 180 observations of 68 variables, each row contains gyroscopic and accelerometer data for one patient performing one activity. 

# Variables

1. Subject - This is a unique identifier for each of the 30 volunteer subjects
2. Activity - This is a descriptive variable of the type of activity carried out

Variable names for the raw data were sourced from the "features.txt" file. Unique variable names were forced using make.names with unique=TRUE

```{r}
valid_column_names <- make.names(names=names(mergedData), unique=TRUE, allow_ = TRUE)
names(mergedData) <- valid_column_names
```

Variables 3-68 of the summary data are a subset of the variables present in the original data, specifically the columns showing Mean and Standard deviation for the various measured parameters. The full list of variables in "summary.txt" is given below:

[1] "Subject"                                    
 [2] "Activity"                                   
 [3] "TimeBodyAccelerometerMeanX"                 
 [4] "TimeBodyAccelerometerMeanY"                 
 [5] "TimeBodyAccelerometerMeanZ"                 
 [6] "TimeGravityAccelerometerMeanX"              
 [7] "TimeGravityAccelerometerMeanY"              
 [8] "TimeGravityAccelerometerMeanZ"              
 [9] "TimeBodyAccelerometerJerkMeanX"             
[10] "TimeBodyAccelerometerJerkMeanY"             
[11] "TimeBodyAccelerometerJerkMeanZ"             
[12] "TimeBodyGyroscopeMeanX"                     
[13] "TimeBodyGyroscopeMeanY"                     
[14] "TimeBodyGyroscopeMeanZ"                     
[15] "TimeBodyGyroscopeJerkMeanX"                 
[16] "TimeBodyGyroscopeJerkMeanY"                 
[17] "TimeBodyGyroscopeJerkMeanZ"                 
[18] "TimeBodyAccelerometerMagnitudeMean"         
[19] "TimeGravityAccelerometerMagnitudeMean"      
[20] "TimeBodyAccelerometerJerkMagnitudeMean"     
[21] "TimeBodyGyroscopeMagnitudeMean"             
[22] "TimeBodyGyroscopeJerkMagnitudeMean"         
[23] "FrequencyBodyAccelerometerMeanX"            
[24] "FrequencyBodyAccelerometerMeanY"            
[25] "FrequencyBodyAccelerometerMeanZ"            
[26] "FrequencyBodyAccelerometerJerkMeanX"        
[27] "FrequencyBodyAccelerometerJerkMeanY"        
[28] "FrequencyBodyAccelerometerJerkMeanZ"        
[29] "FrequencyBodyGyroscopeMeanX"                
[30] "FrequencyBodyGyroscopeMeanY"                
[31] "FrequencyBodyGyroscopeMeanZ"                
[32] "FrequencyBodyAccelerometerMagnitudeMean"    
[33] "FrequencyBodyAccelerometerJerkMagnitudeMean"
[34] "FrequencyBodyGyroscopeMagnitudeMean"        
[35] "FrequencyBodyGyroscopeJerkMagnitudeMean"    
[36] "TimeBodyAccelerometerStdX"                  
[37] "TimeBodyAccelerometerStdY"                  
[38] "TimeBodyAccelerometerStdZ"                  
[39] "TimeGravityAccelerometerStdX"               
[40] "TimeGravityAccelerometerStdY"               
[41] "TimeGravityAccelerometerStdZ"               
[42] "TimeBodyAccelerometerJerkStdX"              
[43] "TimeBodyAccelerometerJerkStdY"              
[44] "TimeBodyAccelerometerJerkStdZ"              
[45] "TimeBodyGyroscopeStdX"                      
[46] "TimeBodyGyroscopeStdY"                      
[47] "TimeBodyGyroscopeStdZ"                      
[48] "TimeBodyGyroscopeJerkStdX"                  
[49] "TimeBodyGyroscopeJerkStdY"                  
[50] "TimeBodyGyroscopeJerkStdZ"                  
[51] "TimeBodyAccelerometerMagnitudeStd"          
[52] "TimeGravityAccelerometerMagnitudeStd"       
[53] "TimeBodyAccelerometerJerkMagnitudeStd"      
[54] "TimeBodyGyroscopeMagnitudeStd"              
[55] "TimeBodyGyroscopeJerkMagnitudeStd"          
[56] "FrequencyBodyAccelerometerStdX"             
[57] "FrequencyBodyAccelerometerStdY"             
[58] "FrequencyBodyAccelerometerStdZ"             
[59] "FrequencyBodyAccelerometerJerkStdX"         
[60] "FrequencyBodyAccelerometerJerkStdY"         
[61] "FrequencyBodyAccelerometerJerkStdZ"         
[62] "FrequencyBodyGyroscopeStdX"                 
[63] "FrequencyBodyGyroscopeStdY"                 
[64] "FrequencyBodyGyroscopeStdZ"                 
[65] "FrequencyBodyAccelerometerMagnitudeStd"     
[66] "FrequencyBodyAccelerometerJerkMagnitudeStd" 
[67] "FrequencyBodyGyroscopeMagnitudeStd"         
[68] "FrequencyBodyGyroscopeJerkMagnitudeStd"

