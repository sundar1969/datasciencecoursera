corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
                
        corrs <- numeric()
        

        comp <- complete(directory, 1:332)
        cond <- comp$nobs > threshold
        compt <- subset(comp,cond)        
        idvec <- as.numeric(unlist(compt["id"]))

        for (i in idvec) {
                filename <- paste(directory,"/",sprintf("%03d",i),".","csv",sep="")
                pfile <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
                nitvec = pfile["nitrate"]
                sulvec = pfile["sulfate"]
                corrs <- c(corrs,cor(sulvec, nitvec, use="pairwise.complete.obs"))
        }
        corrs
} 