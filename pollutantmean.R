pollutantmean <- function(directory, pollutant, id = 1:332){
        old.dir <- getwd()
        
        setwd(file.path(directory))
        
        if(pollutant == "sulfate"){
                pollutant.column <- 2
        }
        if(pollutant == "nitrate"){
                pollutant.column <- 3
        }
        
        pollutant.means <- rep(0, length(id))
        
        k <- 0
        cntr <- 1
        for(k in 1:332){
                if(!(k %in% id)){
                        next
                }
                else {
                        if(k < 10){
                                x <- read.csv(paste("00", k, ".csv", sep = ""))
                        }
                        else if(k < 100){
                                x <- read.csv(paste("0", k, ".csv", sep = ""))
                                
                        }
                        else{
                                x <- read.csv(paste(k,".csv", sep=""))
                        }
                        
                        pollutant.means[cntr] <- mean(x[,pollutant.column], na.rm = TRUE)
                        
                        cntr <- cntr + 1
                }
        }
        
        setwd(old.dir)
        mean(pollutant.means)
}