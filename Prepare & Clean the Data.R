# Package Loading 
  install.packages("foreign") #To read the data sets
  install.packages("dplyr")
  install.packages("esquisse")
  install.packages("xgboost")  
  library(xgboost)             
  library(foreign)
  library(dplyr)
  library(esquisse)
#Load Data 
  y1maindata <- read.arff("C:\\Users\\malek\\Desktop\\Masters\\Semester I\\Machine Learning\\Projects\\Companies Bankruptcy Predictor. (DO NOT CHANGE LOCATION)\\polish+companies+bankruptcy+data\\1year.arff")  
  y2maindata <- read.arff("C:\\Users\\malek\\Desktop\\Masters\\Semester I\\Machine Learning\\Projects\\Companies Bankruptcy Predictor. (DO NOT CHANGE LOCATION)\\polish+companies+bankruptcy+data\\2year.arff")
  y3maindata <- read.arff("C:\\Users\\malek\\Desktop\\Masters\\Semester I\\Machine Learning\\Projects\\Companies Bankruptcy Predictor. (DO NOT CHANGE LOCATION)\\polish+companies+bankruptcy+data\\3year.arFf")
  y4maindata <- read.arff("C:\\Users\\malek\\Desktop\\Masters\\Semester I\\Machine Learning\\Projects\\Companies Bankruptcy Predictor. (DO NOT CHANGE LOCATION)\\polish+companies+bankruptcy+data\\4year.arFf")
  y5maindata <- read.arff("C:\\Users\\malek\\Desktop\\Masters\\Semester I\\Machine Learning\\Projects\\Companies Bankruptcy Predictor. (DO NOT CHANGE LOCATION)\\polish+companies+bankruptcy+data\\5year.arFf")
#Prepare the data
  #Y1
   #Discover the data
        View(y1maindata)
        y1 <- y5maindata
        summary(y1)  
        sum(is.na(y1))
        View(y1)  
        table(y1$class)
      #Split the Data based on class in order to average out the missing values 
        y1class0 <- y1 %>% filter(class==0)
        y1class1 <- y1 %>% filter(class==1)
        table(y1class1$class) 
        #Now replace missing values with col mean and check
        new_y1class1 <- y1class1 %>%
          mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
        new_y1class0 <- y1class0 %>%
          mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
        sum(is.na(new_y1class0))
        #join the Data again
        y1_preclean <- rbind(new_y1class0, new_y1class1)
        View(y1_preclean)  
        #Randomize the data so that you can split it later on for training the model
       y1_clean <- y1_preclean[sample(nrow(y1_preclean)), ]
      View(y1_clean) 
  #Y2
      #Discover the data
      View(y2maindata)
      y2 <- y2maindata
      summary(y2)  
      sum(is.na(y2))
      View(y2)  
      table(y2$class)
      #Split the Data based on class in order to average out the missing values 
      y2class0 <- y2 %>% filter(class==0)
      y2class1 <- y2 %>% filter(class==1)
      table(y2class1$class) 
      #Now replace missing values with col mean and check
      new_y2class1 <- y2class1 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      new_y2class0 <- y2class0 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      sum(is.na(new_y2class0))
      #join the Data again
      y2_preclean <- rbind(new_y2class0, new_y2class1)
      View(y2_preclean)  
      #Randomize the data so that you can split it later on for training the model
      y2_clean <- y2_preclean[sample(nrow(y2_preclean)), ]
      View(y2_clean) 
  #Y3
      #Discover the data
      View(y3maindata)
      y3 <- y3maindata
      summary(y3)  
      sum(is.na(y3))
      View(y3)  
      table(y3$class)
      #Split the Data based on class in order to average out the missing values 
      y3class0 <- y3 %>% filter(class==0)
      y3class1 <- y3 %>% filter(class==1)
      table(y3class1$class) 
      #Now replace missing values with col mean and check
      new_y3class1 <- y3class1 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      new_y3class0 <- y3class0 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      sum(is.na(new_y3class0))
      #join the Data again
      y3_preclean <- rbind(new_y3class0, new_y3class1)
      View(y3_preclean)  
      #Randomize the data so that you can split it later on for training the model
      y3_clean <- y3_preclean[sample(nrow(y3_preclean)), ]
      View(y3_clean) 
  #Y4
      #Discover the data
      View(y4maindata)
      y4 <- y4maindata
      summary(y4)  
      sum(is.na(y4))
      View(y4)  
      table(y4$class)
      #Split the Data based on class in order to average out the missing values 
      y4class0 <- y4 %>% filter(class==0)
      y4class1 <- y4 %>% filter(class==1)
      table(y4class1$class) 
      #Now replace missing values with col mean and check
      new_y4class1 <- y4class1 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      new_y4class0 <- y4class0 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      sum(is.na(new_y4class0))
      #join the Data again
      y4_preclean <- rbind(new_y4class0, new_y4class1)
      View(y4_preclean)  
      #Randomize the data so that you can split it later on for training the model
      y4_clean <- y4_preclean[sample(nrow(y4_preclean)), ]
      View(y4_clean) 
  #y5
      #Discover the data
      View(y5maindata)
      y5 <- y5maindata
      summary(y5)  
      sum(is.na(y5))
      View(y5)  
      table(y5$class)
      #Split the Data based on class in order to average out the missing values 
      y5class0 <- y5 %>% filter(class==0)
      y5class1 <- y5 %>% filter(class==1)
      table(y5class1$class) 
      #Now replace missing values with col mean and check
      new_y5class1 <- y5class1 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      new_y5class0 <- y5class0 %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      sum(is.na(new_y5class0))
      #join the Data again
      y5_preclean <- rbind(new_y5class0, new_y5class1)
      View(y5_preclean)  
      #Randomize the data so that you can split it later on for training the model
      y5_clean <- y5_preclean[sample(nrow(y5_preclean)), ]
      View(y5_clean) 
      
