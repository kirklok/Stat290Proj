#sample call of the function
#source("Engagement.R")
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2) 


report <- function(){
        
        period_start = "2014-12-01"
        period_end = "2015-01-29"
        split_by = "weeks"
        condition = ""
        
        events <- read.csv("testdata.csv", sep = ',')
        colnames(events)[2] <- "uid"
        colnames(events)[3] <- "events"
        colnames(events)[5] <- "timestamp"
        retention_matrix <<- retention_matrix(events, period_start, split_by, condition)
        plots <<- retention_charts(retention_matrix)
        print(plots$dynamic_analysis)
        print(plots$cycle_plot)
        print(plots$active_user_chart)      
}

filter <- function(triggerEvent, followupEvent, data, periodStart = NULL, periodEnd = NULL){
    #check it is dataframe and all columns are correct
    if (!is.data.frame(data)) stop("\"data\" must have a data frame")
    if (!("events" %in% colnames(data))) stop("Data frame \"data\" must have column events")
    if (!("timestamp" %in% colnames(data))) stop("Data frame \"data\" must have column time stamp")
    if (!("uid" %in% colnames(data))) stop("Data frame \"data\" must have column uid")
    #check whether time stamp has correct format    
    d <- try(as.Date(data$timestamp, format= "%Y-%m-%d %H:%M:%S"))
    if(class(d) == "try-error" || is.na(d)) stop( "Timestap has incorrect format - it should be %d-%m-%Y %H:%M:%S" )
    
    #check trigger events
    events <- unique(data$events)
    if (!(triggerEvent %in% events | followupEvent %in% events)) stop("Don't see triggerEvent or followupEvent in events column")
    if (!is.character(triggerEvent)) stop("\"triggerEvent\" should be a character")
    if (!is.character(followupEvent)) stop("\"followupEvent\" should be a character") 

    if (!is.null(periodStart) & !is.null(periodEnd)){
            df <- subset.data.frame(data, (as.Date(timestamp) >= as.Date(periodStart) & as.Date(timestamp) <= as.Date(periodEnd)) 
                & (events == triggerEvent | events == followupEvent))
    } else {
        df <- subset.data.frame(data, (events == triggerEvent | events == followupEvent))
        if (!is.null(periodStart)) {
            df <- subset.data.frame(data, (as.Date(timestamp) >= periodStart) 
                & (events == triggerEvent | events == followupEvent))
        }
        if (!is.null(periodEnd)) {
            df <- subset.data.frame(data, (as.Date(timestamp) <= periodEnd) 
                & (events == triggerEvent | events == followupEvent))
        }
    } 

    df[,c("uid", "timestamp", "events")]    
}

###### create retention_matrix
retentionMatrix <- function(triggerEvent, followupEvent, data, periodStart = NULL, periodEnd = NULL,
    groupBy ){
        if (!(groupBy %in% c("days", "weeks", "months"))) stop ("groupBy should be days, weeks or months")
        sink("5555")

        events <- filter(triggerEvent, followupEvent, data, periodStart, periodEnd)
        events$cohort <- cut(as.Date(events$timestamp), breaks = groupBy)
        cohorts <- unique(events$cohort)   
        
        sink(events)

        m <- ddply(events, "cohort", function(x) {
                
                #identify those users who had triggerEvent during that period
                uidTriggered <- subset(x$uid, x$events == triggerEvent)
                
                #count those who were active in each of the cohorts
                signed_in <- ddply(events, "cohort", function(x) {
                        uidFollowup <- unique(subset(x$uid, x$events == followupEvent))
                        length(subset(uidFollowup, uidFollowup %in% uidTriggered))
                })

        })

        sink(m)
        
        # collapsing data.frame by cohort within data.frame
        m <- ddply(m, "cohort", function(x){t <- t(x)[2,]})
        colnames(m) <- c("cohort", as.character(as.Date(m$cohort)))
        
        # transposing lower triangular matrix to upper triangular matrix
        names <- m$cohort
        m <- as.data.frame(t(m[,-1]))
        colnames(m) <- names
        
        # adding signed up column
        triggered <- daply(events, "cohort", function(x){
                #count those users who signed up during cohort period
                sum(x$events == triggerEvent)  
        })
        
        #making signed_up the first column
        m <- cbind(cohort = rownames(m), triggered, m)
        m$cohort <- NULL
        
        #converting factors to numerics
        m <- sapply(m, function(x) as.numeric(as.character(x))) 
        rownames(m) <- names
        
        #retention matrix in absolute terms
        retentionNumber <- m
        
        #Calculation of relative matrix
        
        #Let's convert absolute values to percentages (% of the registered users remaining active)
        m <- data.frame(retentionNumber)
        tcols <- ncol(m)
        
        for (i in 1:nrow(m)) {
                #select row from data frame
                df <- m[i, ]
                #remove columns with zeros
                df <- data.frame(df[ , !df[]==0])
                #count number of columns in row (w/o zeros)
                pcols <- NCOL(df)   
                #fill columns after values by zeros
                if (pcols < tcols) df[, c((pcols+1):tcols)] <- 0
                #replace initial row by new one
                m[i,] <- df #replace initial row by new one
        }   
        
        #calculate retention
        x <- m
        y <- m[,1]
        m <- apply(x, 2, function(x) x/y )
        
        #add cohort column
        m <- data.frame(cohort=(rownames(m)), m)
        rownames(m) <- NULL
        
        #generate period names
        for (i in 3:ncol(m)) colnames(m)[i]<-paste("P",i-2,sep="")
        
        retentionPercent <- m
        
        retentionMatrix <- list(numberer = retentionNumber,
                                 percentages = retentionPercent)
}

###### create retention charts
retention_charts <- function(retention_matrix){
        
        reten.r <- retention_matrix$retention_relative
        #remove signup and cohort data because it is always 100%
        reten.r <- reten.r[,-2]
        
        #dynamics analysis chart
        
        cohort.chart1 <- melt(reten.r, id.vars = 'cohort')
        colnames(cohort.chart1) <- c('cohort', 'period', 'retention')
        cohort.chart1 <- filter(cohort.chart1, retention != 0)
        da <- ggplot(cohort.chart1, aes(x=period, y=retention, group=cohort, colour=cohort))
        da <- da + geom_line(size=2, alpha=1/2) +
                geom_point(size=3, alpha=1) +
                geom_smooth(aes(group=1), method = 'loess', size=2, colour='red', se=FALSE) +
                labs(title="Cohorts Retention ratio dynamics")
        
        #cycle plot
        
        cohort.chart3 <- cohort.chart1
        cohort.chart3 <- mutate(cohort.chart3, period_cohort = paste(period, cohort))
        cp <- ggplot(cohort.chart3, aes(x=period_cohort, y=retention, group=period, colour=period))

        #choose any cohorts instead of cohort in row#1 and row#2
        m1 <- filter(cohort.chart3, cohort==reten.r[1,1])
        m2 <- filter(cohort.chart3, cohort==reten.r[2,1])
        
        cp <- cp + geom_point(size=3) +
                geom_line(aes(group=period), size=2, alpha=1/2) +
                labs(title="Cohorts Retention ratio cycle plot") +
                #adding connectors for cohort points for better visualisation
                geom_line(data=m1, aes(group=1), colour='blue', size=2, alpha=1/5) +
                geom_line(data=m2, aes(group=1), colour='blue', size=2, alpha=1/5) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

        #user chart
        
        #we count users as active only if they returned during the cohort period
        reten.r <- retention_matrix$retention_absolute[,-1]
        
        #we need to melt data
        cohort.chart.cl <- melt(reten.r, id.vars = 'cohort')
        colnames(cohort.chart.cl) <- c('cohort', 'period', 'users')
        
        #define palette
        reds <- colorRampPalette(c('light blue', 'dark blue'))
        
        #plot data
        uc <- ggplot(cohort.chart.cl, aes(x=period, y=users, group=cohort))
        uc <- uc + geom_area(aes(fill = cohort)) +
                scale_fill_manual(values = reds(nrow(reten.r))) +
                ggtitle('Active users by Cohort')
        
        retention_charts <- list(dynamic_analysis = da, 
                                 cycle_plot = cp, 
                                 active_user_chart = uc)
}


