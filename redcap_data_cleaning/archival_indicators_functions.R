#################################################################
##     Self Report Archival Indices: Set Analysis Functions    ##
##  https://github.com/isaacahuvia/self-report-achival-indices ##
##              Updated locally beginning 5/4/2020             ##
#################################################################

#Functions used to produce statistics in Bevans et al. 2019, "Assessing Child-Report Data Quality." See paper for an explanation of these methods. 
#See https://github.com/isaacahuvia/self-report-achival-indices/blob/master/Use%20Analysis%20Functions.R for examples

####  Even-Odd Consistency  ####
#scaleLookup must be a data frame with two columns, the first being a unique identifier for scales and the second being a list of all variable names in each scale
evenOdd <- function(df, scaleLookup, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #List unique scales
  scales <- unique(scaleLookup[[1]])
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    #For each row, establish vectors for the means of even and odd responses across scales
    evens <- c()
    odds <- c()
    
    for(k in 1:length(scales)) {
      
      #For each scale, pull its values...
      values <- df[i, names(df) %in% scaleLookup[[2]][scaleLookup[[1]] %in% scales[k]]]
      values <- as.numeric(values)
      
      #And separate the even and odd values, adding their means to the vectors evens and odds
      evens[k] <- mean(values[seq(2, length(values), by = 2)], na.rm = T)
      odds[k] <- mean(values[seq(1, length(values), by = 2)], na.rm = T)
      
    }
    
    #The output is the correlation between these two vectors
    out[i] <- cor(evens, odds)
    
  }
  
  return(out)
  
}



####  Inter-Item Standard Deviation  ####
#scaleLookup must be a data frame with two columns, the first being a unique identifier for scales and the second being a list of all variable names in each scale
interItemSD <- function(df, scaleLookup = NULL, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  if(is.null(scaleLookup)) {
    
    for(i in 1:nrow(df)) out[i] <- sd(df[i,], na.rm = T) #Where there are no scales, the output is simply the standard deviation of the response set, with NA values removed
    
  } else {
    
    #List unique scales
    scales <- unique(scaleLookup[[1]])
    
    for(i in 1:nrow(df)) {
      
      #Initialize a vector of standard deviations for each scale, to be averaged
      SDs <- c()
      
      for(k in 1:length(scales)) {
        
        #For each scale, pull its values...
        values <- df[i, names(df) %in% scaleLookup[[2]][scaleLookup[[1]] %in% scales[k]]]
        values <- as.numeric(values)
        
        #...and take their standard deviation, ignoring NA values
        SDs[k] <- sd(values, na.rm = T)
        
      }
      
      #The output is the mean of the standard deviations by scale
      out[i] <- as.numeric(mean(SDs, na.rm = T))
      
    }
    
  }
  
  return(out)
  
}



####  Longstring  ####
#Can calculate overall longstring, or for each response level
longstring <- function(df, columns = NULL, value = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    #Count the length and type of response strings of the same value
    rle <- rle(df[i,])
    
    if(is.null(value)) {
      
      #If a specific value is not specified, the output is the maximum longstring of any value
      max <- max(rle$lengths)
      
    } else {
      
      if(value %in% df[i,]) {
        
        #If a specific value is specified and the response set includes that value, the output is the maximum longstring of that value
        max <- max(rle$lengths[rle$values == value], na.rm = T)
        
      } else {
        
        #If a specific value is specified but a response set does not include that value, the maximum longstring is 0
        max <- 0
        
      }
      
    }
    
    #For each row, the output is the maximum longstring for that row
    out[i] <- max
    
  }
  
  return(out)
  
}



####  Squared Mahalanobis Distance  ####
#only works with complete cases
mahalanobisDist <- function(df, columns = NULL) {
  
  require(stats)
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #This procedure requires copmplete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
  completeCasesIndex <- which(complete.cases(df))
  
  #Limit the dataset to complete cases
  df.complete <- df[completeCasesIndex,]
  
  #Calculate the mahalanobis distance with those complete cases
  out.complete <- stats::mahalanobis(df.complete, colMeans(df.complete), cov(df.complete))
  
  #Initialize the ouput vector as NA with the full length of nrow(df) so that we can include the complete case (non-NA) values in their right places
  out <- rep(NA, nrow(df))
  
  #Add mahalanobis distance values back into this NA vector in their right places (so that cases with NA values will have an NA value here)
  out[completeCasesIndex] <- out.complete
  
  return(out)
  
}



####  Omitted Items  ####
omittedItems <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    #For each row, the output is the number of NA values in that row
    out[i] <- sum(is.na(df[i,]))
    
  }
  
  return(out)
  
}



####  Person-Total Correlation  ####
personTotalCor <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    responseSet <- as.numeric(df[i,]) #Take the response set...
    meanResponseSet <- sapply(df[-i,], mean, na.rm = T) #...and the mean set of all other response sets...
    out[i] <- cor(responseSet, meanResponseSet, use = "pairwise.complete.obs") #...and output their correlation for each row (response set)
    
  }
  
  return(out)
  
}



####  IRT: Polytomous Guttman Errors  ####
#scaleLookup must be a data frame with two columns, the first being a unique identifier for scales and the second being a list of all variable names in each scale
polyGuttmanErrors <- function(df, nCategories, norm = F, columns = NULL, scaleLookup = NULL) {
  
  require(PerFit) #see https://cran.r-project.org/web/packages/PerFit/PerFit.pdf
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Modify df if needed so that the lowest response level is coded as 0 (required for PerFit functions)
  if(min(df, na.rm = T) != 0) df <- df - min(df, na.rm = T)
  
  if(is.null(scaleLookup)) {
    
    if(norm == T) {
      
      #This procedure requires complete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
      completeCasesIndex <- which(complete.cases(df))
      
      #Limit the dataset to complete cases
      df.complete <- df[completeCasesIndex,]
      
      #Calculate the normed polytomous Guttman errors with those complete cases
      out.complete <- PerFit::Gnormed.poly(matrix = df.complete, Ncat = nCategories)[["PFscores"]][[1]]
      
      #Initialize the ouput vector as NA with the full length of nrow(df) so that we can include the complete case (non-NA) values in their right places
      out <- rep(NA, nrow(df))
      
      #Add normed error values back into this NA vector in their right places (so that cases with NA values will have an NA value here)
      out[completeCasesIndex] <- out.complete
      
    } else {
      
      #If values are not to be normed, simply return the unnormed values. This procedure does not require complete cases. 
      out <- PerFit::Gpoly(matrix = df, Ncat = nCategories)[["PFscores"]][[1]]
      
    }
    
  } else {
    
    #List unique scales
    scales <- unique(scaleLookup[[1]])
    
    #Initialize a vector of Guttman errors for each scale, to be summed
    errors <- matrix(ncol = length(scales), nrow = nrow(df))
    
    for(k in 1:length(scales)) {
      
      if(norm == T) {
        
        stop("This function cannot calculate normed Guttman errors by scale")
        
      } else {
        
        #If scales are supplied, calculate Guttman errors by scale...
        errors[,k] <- PerFit::Gpoly(matrix = df[,names(df) %in% scaleLookup[[2]][scaleLookup[[1]] %in% scales[k]]],
                                    Ncat = nCategories)[["PFscores"]][[1]]
        
      }
      
    }
    
    #... and output the sum of Guttman errors by scale
    out <- as.numeric(rowSums(errors, na.rm = T))
    
  }
  
  return(out)
  
}



####  Psychometric Synonyms  ####
#See Psychometric Synonyms with Highly Correlated Data Demonstration.R for a demonstration of why this was not as useful in our analysis
psychSyn <- function(df, critval = .6, columns = NULL) {
  
  require(careless) #see https://cran.r-project.org/web/packages/careless/careless.pdf
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  out <- careless::psychsyn(df, critval = critval)
  
  return(out)
  
}



####  Resampled Individual Consistency  ####
#like even-odd consistency, but instead of arbitrarily using even and odd numbered responses, use a series of random splits
#scaleLookup must be a data frame with two columns, the first being a unique identifier for scales and the second being a list of all variable names in each scale
resampledConsistency <- function(df, scaleLookup, iterations = 100, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #List unique scales
  scales <- unique(scaleLookup[[1]])
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  for(i in 1:nrow(df)) {
    
    #Since this process can take a while, this code tracks its progress and outputs a % complete indicator
    progress <- round(seq(nrow(df) / 20, nrow(df), length.out = 20))
    if(i %in% progress) print(paste0(round(100 * i / nrow(df)), "% complete"))
    
    #Establish a vector of correlations, to be populated during resampling
    cors <- c()
    
    for(n in 1:iterations) {
      
      #For each iteration, establish vectors a and b, which will be the means of random halves of each scale
      a <- c()
      b <- c()
      
      for(k in 1:length(scales)) {
        
        #For each scale, take its values and count its length
        values <- as.numeric(df[i, names(df) %in% scaleLookup[[2]][scaleLookup[[1]] %in% scales[k]]])
        index <- 1:length(values)
        
        #Randomly determine which vector will have which values
        index.a <- sample(index, floor(length(index)) / 2, replace = F)
        index.b <- index[!index %in% index.a]
        
        #Take the mean of each randomly sampled vector
        a[k] <- mean(values[index.a], na.rm = T)
        b[k] <- mean(values[index.b], na.rm = T)
        
      }
      
      #Correlate the means of random halves of each scale
      cors[n] <- cor(a, b)
      
    }
    
    #Output the mean of these correlations
    out[i] <- mean(cors)
    
  }
  
  return(out)
  
}



####  Reversed Question Difference  ####
#If the reversed items you provide this function are already coded to be in the same direction as other items, then a larger difference between means indicates careless responding
#If the reversed items you provide this function are still reverse-coded relative to the other items, then small (near-zero) difference between means indicates careless responding 
reversedItemDifference <- function(df, scaleLookup, reversedItems, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  #Limit to only scales with reversed items
  scaleLookup <- scaleLookup[scaleLookup[[1]] %in% (scaleLookup[[1]][scaleLookup[[2]] %in% reversedItems]),]
  
  #List unique scales
  scales <- unique(scaleLookup[[1]])
  
  for(i in 1:nrow(df)) {
    
    diff <- c()
    
    for(k in 1:length(scales)) {
      
      scale
      
      scale.normal <- df[i, names(df) %in% scaleLookup[scaleLookup[[1]] == scales[k],][[2]] & !names(df) %in% reversedItems]
      scale.reversed <- df[i, names(df) %in% scaleLookup[scaleLookup[[1]] == scales[k],][[2]] & names(df) %in% reversedItems]
      
      scale.normal <- as.numeric(scale.normal)
      scale.reversed <- as.numeric(scale.reversed)
      
      normal <- mean(scale.normal, na.rm = T)
      reversed <- mean(scale.reversed, na.rm = T)
      
      diff[k] <- abs(normal - reversed)
      
    }
    
    out[i] <- mean(diff)
    
  }
  
  return(out)
  
}



####  IRT: U3 Person Fit Statistic  ####
#See https://www.jstatsoft.org/article/view/v074i05/v74i05.pdf for rationale for choosing the U3 statistic for use with polytomous data
u3 <- function(df, nCategories, columns = NULL) {
  
  require(PerFit) #see https://cran.r-project.org/web/packages/PerFit/PerFit.pdf
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Modify df if needed so that the lowest response level is coded as 0 (required for PerFit functions)
  if(min(df, na.rm = T) != 0) df <- df - min(df, na.rm = T)
  
  #This procedure requires copmplete cases. Therefore we'll limit the dataset to complete cases, and return a vector which is NA when a case is not complete
  completeCasesIndex <- which(complete.cases(df))
  
  #Limit the dataset to complete cases
  df.complete <- df[completeCasesIndex,]
  
  #Calculate the normed polytomous Guttman errors with those complete cases
  out.complete <- PerFit::U3poly(matrix = df.complete, Ncat = nCategories)[["PFscores"]][[1]]
  
  #Initialize the ouput vector as NA with the full length of nrow(df) so that we can include the complete case (non-NA) values in their right places
  out <- rep(NA, nrow(df))
  
  #Add U3poly values back into this NA vector in their right places (so that cases with NA values will have an NA value here)
  out[completeCasesIndex] <- out.complete
  
  return(out)
  
}



####  Univariate Outlier Analysis  ####
zScore <- function(df, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Calculate the mean value by row
  rowMean <- rowMeans(df, na.rm = T)
  
  #Output the z-score of that value compared to all other values
  out <- (rowMean - mean(rowMean, na.rm = T)) / sd(rowMean, na.rm = T)
  
  return(out)
  
}