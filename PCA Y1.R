#########Libraries##########
#make sure libraries are laoded
  library(ggplot2)
  install.packages("sf")
  install.packages("transformr")
  library(dplyr)
  install.packages("plotly")
  library(plotly)
  library(ggplot2)
  library(gganimate)
#######################Y1PCA#############
  str(y1_clean)
#Separate Features and Class
  y1_features <- y1_clean[, -ncol(y1_clean)]
  y1class <- y1_clean[,ncol(y1_clean)]
  View(y1_features)  
  View(y1class)
#scale features
  y1_features_scaled <- scale( y1_features)
#Run PCA
  y1pca_result <- prcomp(y1_features_scaled, center = TRUE, scale. = TRUE)
  summary(y1pca_result)
#Visualize PCA Results
  #Scree Plot: The scree plot shows the proportion of variance explained by each principal component:
  y1explained_variance <- y1pca_result$sdev^2 / sum(y1pca_result$sdev^2)
  qplot(1:length(y1explained_variance), y1explained_variance, geom = "line") +
    geom_point() +
    labs(x = "Principal Component", y = "Proportion of Variance Explained") +
    theme_minimal()
  #Biplot: Visualize the first two principal components### NEEDS WORK!
    biplot(y1pca_result, scale = 0)
  #2D Scatter Plot with Classes: Plot the data in the space of the first two principal components, colored by class:
      y1pca_data <- data.frame(y1pca_result$x, Class = y1class)
      ggplot(y1pca_data, aes(PC1, PC2, color = as.factor(Class))) +
      geom_point() +
      theme_minimal() +
      labs(title = "PCA Scatter Plot", color = "Class")
   #3D Scatter Plot (Optional):If you want a 3D visualization of the first three principal components:   
      
      plot_ly(y1pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Class, type = "scatter3d", mode = "markers")
      5. Select Number of Components
#Cumulative Variance Determine the number of components that explain the desired proportion of variance (e.g., 95%):
      # Extract standard deviations of the principal components
      explained_variance <- (y1pca_result$sdev)^2 / sum((y1pca_result$sdev)^2)
      
      # Calculate cumulative variance
      cumulative_variance <- cumsum(explained_variance)
      #2. Plot Cumulative Variance
      plot(cumulative_variance, type = "b", 
           xlab = "Number of Components", 
           ylab = "Cumulative Variance Explained",
           main = "Cumulative Variance Explained by PCA")
      # Add a threshold line for 95% cumulative variance
      abline(h = 0.95, col = "red", lty = 2)  # Horizontal line at 95%
      
      # Identify the number of components required to explain ~95% variance
      num_components <- which(cumulative_variance >= 0.95)[1]  # First component reaching 95%
      abline(v = num_components, col = "blue", lty = 2)  # Vertical line at the component
      print(num_components)
      #Get new data with only best components 
        # Extract the first 21 components
      y121f <- y1pca_result$x[, 1:21]
     
        # Combine with the class column
      y1_ready<- data.frame(y121f, Class = y1class)
      View(y1_ready)
     