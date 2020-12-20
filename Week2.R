print(R.version.string)
setwd("C:/Users/Jiu Xiao/Downloads/rprog_data_specdata")

pollutantmean <- function(directory, pollutant, id = 1:332){
    setwd(paste("C:/Users/Jiu Xiao/Downloads/rprog_data_specdata",directory,sep = "/"))
    vals <- vector()
    for (i in id){
        filename <- sprintf("%03d.csv",i)
        excel <- read.csv(filename)
        data <- excel[,pollutant]
        data <- data[!is.na(data)]
        vals <- c(vals, data)
    }
    mean(vals)
}

pollutantmean("specdata", "nitrate", 70:72)

complete <- function(directory, id = 1:332){
    setwd(paste("C:/Users/Jiu Xiao/Downloads/rprog_data_specdata",directory,sep = "/"))
    df <- data.frame()
    for (i in id){
        filename <- sprintf("%03d.csv", i)
        excel <- read.csv(filename)
        data <- excel[complete.cases(excel),]
        df <- rbind(df, c(i,nrow(data)))
    }
    colnames(df) <- c("id","nobs")
    df
}

complete("specdata", 70:72)

corr <- function(directory, threshold = 0){
    completes <- complete(directory, 1:332)
    completes <- subset(completes, nobs > threshold )
    correlations <- vector()
    
    for(i in completes$id){
        filename <- sprintf("%03d.csv", i)
        excel <- read.csv(filename)
        data <- excel[complete.cases(excel),]
        if(nrow(data) >= threshold){
            correlations <- c(correlations, cor(data$nitrate, data$sulfate))
        }
    }
   correlations
}
cr <- corr("specdata", 400)
head(cr)












