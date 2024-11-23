# CHAPTER 7 - Bootstrapping

# PROGRAM   - PCA Bootstrapping Assignment Question

# SUMMARY   - In this program we analyse whether the mean travel time from a 
#             certain suburb to the city centre differs by mode of transport 
#             (specifically bus, car or cycle).
#
#             Section 1:
#             Read in original NZDEP18 Study and obtaining the eigenvalue & eigenvector corresponding
#             to the first principal component
#             
#             Section 2:
#             Constructing bootstrapping procedure to obtain confidence intervals for
#               - The proportion of variance explained by the 1st principal component
#               - The loadings of the first principal component
#
#             Section 3:
#             Constructing confidence intervals




# Clear all variables before starting
rm(list = ls())

# Calling required packages



# Section 1
#-------------------------------------------------------------------------------
# Read in original NZDEP18 Study and obtaining the eigenvalue & eigenvector corresponding
# to the first principal component

# 1.1a - Read in the 'NZDEP18_SA2_Data.csv' data set
Dep1a <- read.csv("G:\\Academic\\Lecture Courses\\STAT 432\\Additional Material\\Assignments & Tests\\R Data\\NZDEP18_SA2_Data.csv")
str(Dep1a)
Dep1a


# 1.1b - Refine to required variables and specifying
  # - the number of rows (i.e. # of SA2s # appearing in the study)
  # - the number of variables (measures of socio-economic deprivation)
Dep1b = subset(Dep1a, select = -c(SA2_2018_Code, SA2_2018_Name))
Dep1b
  rows <- nrow(Dep1b)
  cols <- ncol(Dep1b)


# 1.1c - Obtaining the covariance matrix of the normalised data (i.e. obtaining 
# correlation matrix of the non-normalised data)
Dep1c <- cor(Dep1b)
Dep1c


# 1.1d - Obtaining the eigenvalues and eigenvectors of the correlation matrix
Dep1d <- eigen(Dep1c)

    # Obtain eigenvalues
    Dep1d1 <- Dep1d$values
    Dep1d1
    
    # obtain proportion of variance in the data explained by 1st principal component
    Dep1d2 <- Dep1d1[[1]] / cols
    Dep1d2
    
    # Obtain eigenvectors. 
    Dep1d3 <- Dep1d$vectors
    Dep1d3
    
    # Obtain the the first principal component.
    # Note that eigenvectors are unique up to a factor of +-1. Since we know from
    # the previous output that the elements (loadings) of the 1st principle component
    # all have the same (negative) sign, we are perfectly within our rights simply
    # to take the absolute value of each element
    Dep1d4 <- abs(Dep1d3[,1])
    Dep1d4
    



    
# Section 2
#-------------------------------------------------------------------------------
# Constructing bootstrapping procedure to obtain confidence intervals for
    # - The proportion of variance explained by the 1st principal component
    # - The loadings of the first principal component

# 2.1a - Define # of bootstrap samples to take and output vectors / matrices
B <- 5000
Dep2a1 <- rep(NA,B)
Dep2a2 <- matrix(NA,B,cols)
 


    
# 2.1b - Conduct bootstrapping procedure

    # 2.1b1 - Set seed to ensure same bootstrap samples are created in each run
    set.seed(12345)
    
    # 2.1b2 - Create required bootstrap samples and extract required metrics
    for (i in 1:B) {    
        
        # 2.1b3 - Obtain a list of data rows to re-sample from
        Dep2b3 <- sample(1:rows, rows, replace=T)
        
        # 2.1b4 -Obtain bootstrap sample from Dep1b data set
        Dep2b4 <- Dep1b[Dep2b3,]
        
        # 2.1b5 - Obtaining the covariance matrix of the normalised data 
        # (i.e. obtaining correlation matrix of the non-normalised data)
        Dep2b5 <- cor(Dep2b4)
 
        # 2.1b6 - Obtain eigenvalues
        Dep2b6 <- eigen(Dep2b5)$values
        
        # 2.1b7 - Obtain proportion of variance explained by 1st principal component
        Dep2b7 <- Dep2b6[[1]] / cols
        
        # 2.1b8 - Obtain eigenvectors
        Dep2b8 <- eigen(Dep2b5)$vectors
        
        # 2.1b9 - Obtain the the 1st principal component
        Dep2b9 <- abs(Dep2b8[,1])
        Dep2b9
        
        # 2.1b10 - Insert results into output vectors
        Dep2a1[i]  <- Dep2b7
        Dep2a2[i,] <- Dep2b9
    }    
    
    

    
    
    
    
# Section 3
#-------------------------------------------------------------------------------
# Constructing confidence intervals

# 3.1a - Constructing a histogram of the data in the Dep2a1 data set (i.e. of the
# proportion of variance explained by the 1st PC
hist(Dep2a1, prob=T, main="", xlab="% of Var Explained by 1st PC")

    
# 3.1b - Calculate the standard error of the proportion of variance explained by 
# the 1st PC and the associated standard 95% bootstrapping confidence interval
Dep3b <- sd(Dep2a1)
Dep3b

Dep3b1 <- c(Dep1d2 - 1.96 * Dep3b, Dep1d2 + 1.96 * Dep3b)
Dep3b1  
    
    
# 3.1c - Convert to Dep2a2 to a dataframe and rename columns
Dep3c <- as.data.frame(Dep2a2)
names(Dep3c)[names(Dep3c)=='V1'] <- 'JSS_PC'
names(Dep3c)[names(Dep3c)=='V2'] <- 'SPS_PC'  
names(Dep3c)[names(Dep3c)=='V3'] <- 'MTB_PC'  
names(Dep3c)[names(Dep3c)=='V4'] <- 'No_Quals_PC'  
names(Dep3c)[names(Dep3c)=='V5'] <- 'No_Own_PC'  
names(Dep3c)[names(Dep3c)=='V6'] <- 'Damp_Mould_PC'  
names(Dep3c)[names(Dep3c)=='V7'] <- 'No_Int_PC'  
names(Dep3c)[names(Dep3c)=='V8'] <- 'Inc_Leq_50k_PC'  
names(Dep3c)[names(Dep3c)=='V9'] <- 'Crowded_PC'  
Dep3c   


# 3.1d - Obtaining histograms of the data in each column
par(mfrow=c(3,3))
hist(Dep3c$JSS_PC,         prob=T, main="JSS_PC", xlab="% of Var Explained")
hist(Dep3c$SPS_PC,         prob=T, main="SPS_PC", xlab="% of Var Explained")
hist(Dep3c$MTB_PC,         prob=T, main="MTB_PC", xlab="% of Var Explained")
hist(Dep3c$No_Quals_PC,    prob=T, main="No_Quals_PC", xlab="% of Var Explained")
hist(Dep3c$No_Own_PC,      prob=T, main="No_Own_PC", xlab="% of Var Explained")
hist(Dep3c$Damp_Mould_PC,  prob=T, main="Damp_Mould_PC", xlab="% of Var Explained")
hist(Dep3c$No_Int_PC,      prob=T, main="No_Int_PC", xlab="% of Var Explained")
hist(Dep3c$Inc_Leq_50k_PC, prob=T, main="Inc_Leq_50k_PC", xlab="% of Var Explained")
hist(Dep3c$Crowded_PC,     prob=T, main="Crowded_PC", xlab="% of Var Explained")


# 3.1e - Obtaining the standard error for each of the loadings    
Dep3e <- apply(Dep2a2,2,sd)   
Dep3e
    

# 3.1f - Obtaining the standard 95% Bootstrap confidence interval for each loading
    
    # Create blank 9x3 matrix
    loading.matrix <- matrix(NA,9,3)
    
    # Specify names of rows and columns of loading.matrix
    dimnames(loading.matrix) <- list(c("JSS_PC",     "SPS_PC",        "MTB_PC",
                                    "No_Quals_PC", "No_Own_PC",       "Damp_Mould_PC",
                                    "No_Int_PC",   "Inc_Leq_50k_PC",  "Crowded_PC"    ), 
                                  c("Estimated","Lower","Upper"))

    # Entering data into the matrix
    loading.matrix [,1] <- Dep1d4
    loading.matrix [,2] <- Dep1d4 - 1.96*Dep3e
    loading.matrix [,3] <- Dep1d4 + 1.96*Dep3e

    loading.matrix














