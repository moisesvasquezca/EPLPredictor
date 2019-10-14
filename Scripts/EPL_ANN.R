# Artificial Neural Network to predict the result
# This accepts a file of EPL round statistics used to predict results.
# The implementation uses h2o's ANN to evaluate layer thresholds on prediction accuracy.
# ? 2019 St. Thomas University AI Lab

# Set the java directory for h2o
#Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-8-oracle")  # Optional - Path to java (do not include bin)
#print(Sys.getenv("JAVA_HOME"))

# Configuration variables
ROUND_INPUT_DATA_FILE = "EPL_2018_19_Rounds6-38.csv"
MINIMUM_LAYERS<- 1              # Starting number of layers to evaluate
MAXIMUM_LAYERS<- 20             # Maxium number of layers to train on
MINIMUM_RUNS<- 1                # Starting number of runs
MAXIMUM_RUNS<- 30               # Number of runs to evaluate each hidden layer
MINIMUM_HIDDEN_NEURONS <- 1     # Staring number of neurons to evaluate
MAXIMUM_HIDDEN_NEURONS <- 30    # Number of neuron in each hidden layer
EPOCHS <- 1000                  # Number of epochs to train each ANN
ACTIVATION_FUNCTION <- "Tanh"   # ANN activation function: Tanh, TanhWithDropout, 
                                # Rectifier, RectifierWithDropout

# Output file names
OUTPUT_RUN_RESULTS_FILE <- "EPL_DeepLearning_Results.csv"     # Stores predictions and accuracy of each run
OUTPUT_ACCURACY_FILE <- "EPL_DeepLearning_Accuracy.csv"   # Stores average accuracy and standard dev values

#Installing the h2o package for ANN
#install.packages("RCurl")
#install.packages("caTools")
#install.packages("h2o")

# If the h2o package cannot download from CRAN
#install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-xu/4/R")

library(caTools)
library(h2o)


 

# Loading the round data
data <- read.csv(ROUND_INPUT_DATA_FILE)

# Initialize the h2o engine for the ANN
h2o.init(nthreads = 1)

# Run different combinations of the ANN's configuration 
# (layers and hidden neurons) for a set # of runs each
for( layer in MINIMUM_LAYERS:MAXIMUM_LAYERS){
  
  for( numOfNodes in MINIMUM_HIDDEN_NEURONS:MAXIMUM_HIDDEN_NEURONS){
  
    for(run in MINIMUM_RUNS:MAXIMUM_RUNS){
  
      cat("---------------------------------\n")
      cat("       Layer:", layer, "Nodes:", numOfNodes, "Run:",run,"\n")
      cat("---------------------------------\n")
  
      # Splitting the data set in to training set and test set
      
      split <- sample.split(data$Result,SplitRatio = 0.8)
      training_set<- subset(data,split==TRUE)
      test_set<- subset(data,split==FALSE)
      
      # Creating a new column "Result" to convert Result to a factor. 
      # 0 represents Away team, 0.5 represents Draw, and 1 represents Home team
      
      training_set$actualResult <- ifelse(training_set$Result=="A", 0 ,ifelse(training_set$Result=="H", 1,0.5))
      
      
      # Remove the Result Variable from the Test Set
      ActualResults <- as.character(test_set$Result)
      test_set$Result <- NULL
  
      #Connecting to a h2o cluster
      independentVariableColumns <- c("HomePlayersScore",
                                      "AwayPlayersScore",
                                      "ave_goalsFor_home",
                                      "ave_goalsFor_away",
                                      "ave_goalsAgainst_home",
                                      "ave_goalsAgainst_away",
                                      "ave_points_home",
                                      "ave_points_away")
  
      ANN <- h2o.deeplearning(y = "actualResult",
                              x = independentVariableColumns,
                              training_frame = as.h2o(training_set),
                              activation = ACTIVATION_FUNCTION,
                              hidden = rep(numOfNodes, layer),
                              epochs = EPOCHS)
      summary(ANN)
      
      neural.pred <- h2o.predict(ANN, newdata = as.h2o(test_set[-c(1, 2, 3)]))
      neural.pred <- plogis(as.vector(neural.pred))
      PredictedResult <- ifelse(neural.pred < 0.33, "A", ifelse(neural.pred < 0.66, "D", "H"))
      PredictedResult <- as.vector(PredictedResult)
      
      #m <- table(predictedResult,test_set$predictedResult)
      #m
      
      
      # Calculate the accuracy of the model. Create new accuracy column.
      Accuracy <- ifelse(PredictedResult == ActualResults, 1, 0)
      avgAccuracy <- mean(Accuracy)
      stdAccuracy <- sd(Accuracy)
      cat("Accuracy:", avgAccuracy, "\n")
  
     
      # Organize the prediction results for output
      layerAndRunresults <- data.frame(Layer = layer, 
                                       Nodes = numOfNodes,
                                       Run = run, 
                                       Home = test_set$Home, 
                                       Away = test_set$Away, 
                                       ActualResult = ActualResults,
                                       PredictedResult = PredictedResult, 
                                       Accuracy = Accuracy,
                                       stringsAsFactors = FALSE)
      
      
      
      # Write results dataframe to file.
      write.table(layerAndRunresults, 
                  file = OUTPUT_RUN_RESULTS_FILE,
                  quote = FALSE,
                  append = TRUE, 
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      
      
      # Write the accuray report to file.
      accuracyOutput <- data.frame(Layers = layer, 
                                   Nodes = numOfNodes,
                                   Run = run, 
                                   Accuracy = avgAccuracy, 
                                   STD = stdAccuracy)
      
      write.table(accuracyOutput, 
                  file = OUTPUT_ACCURACY_FILE,
                  quote = FALSE,
                  append = TRUE, 
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
    }#ends the run
  } # end hidden layer count
}#ends the layer

h2o.shutdown()


