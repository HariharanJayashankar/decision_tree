#Trying to code decision tree classifiers for a small toy dataset

rm(list = ls())

#Decision tree classifiers are just a set of questions (asked to input variables), upon which the
#output is seperated out. A good decision tree is essentially a good list of questions for our
#inputs such that at the end of the "interrogation", our output is neatly sorted out

#Questions are essentially of the form, "is the observation colour yellow"? "No"s go to one side
#"Yes"s go to the other.

#Getting the toy dataset
#We will create 10 observations, 2 input variables and 1 output variable. THe output variable
#will classify an observation into 3 categories

x1 <- sample(1:4, size = 10, replace = TRUE)
x2 <- sample(1:2, size = 10, replace = TRUE)
y <- sample(1:3, size = 10, replace = TRUE)

data <- data.frame(x1, x2, y)


#Doing the decision tree stuff

#defining a function for GINI impurity

gini <- function(df){
    
    impurity <- 1 - 1/nrow(df[, df$y])
    return(impurity)
    
}

#decision tree step 1
#Ask a question for x1


q_step_1 <- function(a){
    
    improvement<- data.frame(n = as.numeric(), gini_impr = as.numeric(), stringsAsFactors=FALSE)

    for(i in unique(a$x1)){
        
    
        df_i <- a[a$x1 >= i,]
        gini_0 <- gini(a)
     
        improvement_i <- gini_0 - gini(df_i)
        improvement[i, ] <- c(i,  improvement_i)
        
    }
    
    improvement <- improvement[complete.cases(improvement$n), ]

    question <- (improvement[improvement$gini_impr == max(improvement$gini_impr), ])
    
    if (question$gini_impr <= 0){
        print("There is no GINI Impurity Improvement")
    } else{
        
        data_1_y <- a[a$x1 >= question$n, ]
        data_1_n <- a[a$x1 < question$n, ]
    }
    

    return( cbind(data_1_y, question$gini_impr) )
    
}