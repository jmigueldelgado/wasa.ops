read_tc_theta_hourly<- function(l)
    {
        require("xts")
        output_dir <- l$wasa_output_dir
        df <- read.table(paste0(output_dir,"tc_theta.out"),skip=1,header=TRUE)        
        y <- df[,1]
        doy <- df[,2]
        h <- df[,3]
        df <- df[,-1:-3]
        xdate <- strptime(x=paste(y,doy,h,sep="-"),"%Y-%j-%H",tz="BRT")
        colnames(df) <- substr(colnames(df),2,nchar(colnames(df)))
        xtsObj <- xts(df,as.POSIXlt(xdate),tz="BRT")
        return(xtsObj)
    }

read_RiverFlow_daily <- function(l,spObj=NULL)
    {
        output_dir <- l$wasa_output_dir
        df <- read.table(paste0(output_dir,"/River_Flow.out"),skip=1,header=TRUE)
        y <- df[,1]
        doy <- df[,2]
        df <- df[,-1:-2]
        xdate <- strptime(x=paste(y,doy,sep="-"),"%Y-%j")
        if(is.null(colnames(df)))
            {
                xtsObj <- xts(df,as.POSIXlt(xdate))
                colnames(xtsObj) <- "1"
            } else
                {
                    colnames(df) <- substr(colnames(df),2,nchar(df))
                    xtsObj <- xts(df,as.POSIXlt(xdate))
                }
        return(xtsObj)
    }

read_qhorton_hourly <- function(l)
    {
        output_dir <- l$wasa_output_dir
        df <- read.table(paste0(output_dir,"/qhorton.out"),skip=1,header=TRUE)
        y <- df[,1]
        doy <- df[,2]
        h <- df[,3]
        df <- df[,-1:-3]
        xdate <- strptime(x=paste(y,doy,sep="-"),"%Y-%j",tz="BRT")
        colnames(df) <- substr(colnames(df),2,nchar(df))
        xtsObj <- xts(df,as.POSIXlt(xdate),tz="BRT")
        if(is.null(spObj))
        {
            return(xtsObj)
        } else
        {
            x <- coredata(xtsObj)
            xtime <- data.frame(time=time(xtsObj))
            xx <- cbind(xtime,x)
            xtsmelt <- melt(xx,id.vars="time")
            names(xtsmelt) <- c("time","ID","values")
            xtsmelt$ID <- as.numeric(xtsmelt$ID)
            xxx <- join(xtsmelt,as.data.frame(spObj),by="ID")
            xxxx <- xxx[with(xxx,order(time,ID)), ]
            stObj <- STFDF(sp=spObj,time=unique(xxxx$time),data=xxxx[c("values")])
            return(stObj)
        }
    }

read_rainfall_daily <- function(l)
    {
        input_dir <- l$wasa_input_dir
        cols <- scan(paste0(input_dir,"/rain_daily.dat"),nlines=1,skip=2)
        cols <- cols[-1:-2]
        x <- read.table(paste0(input_dir,"/rain_daily.dat"),header=FALSE,skip=3)
        xdate <- x[,1]
        xyear <- substrRight(xdate,4)
        xmonth <- substrRight(xdate,6,4)
        xday <- substrRight(xdate,8,6)
        xdatetime <- as.POSIXlt(ISOdate(year=xyear,month=xmonth,day=xday,tz="GMT"))
        dfObj <- as.data.frame(x[,-1:-2])
        colnames(dfObj) <- paste0("X",as.character(cols))
        xtsObj <- xts(x=dfObj,order.by=xdatetime)

        return(xtsObj)
    }


read_rainfall_hourly <- function(l)
    {
        input_dir <- l$wasa_input_dir
        cols <- scan(paste0(input_dir,"/Time_series/rain_hourly.dat"),nlines=1,skip=2)
        cols <- cols[-1:-2]
        x <- read.table(paste0(input_dir,"/Time_series/rain_hourly.dat"),header=FALSE,skip=3)
        xdate <- x[,1]
        xhour <- x[,2]
        xyear <- substrRight(xdate,4)
        xmonth <- substrRight(xdate,6,4)
        xday <- substrRight(xdate,8,6)
        xdatetime <- as.POSIXlt(ISOdate(year=xyear,month=xmonth,day=xday,hour=xhour,tz="GMT"))
        dfObj <- as.data.frame(x[,-1:-2])
        xtsObj <- xts(x=dfObj,order.by=xdatetime)
        colnames(xtsObj) <- as.character(cols)
        return(xtsObj)
    }
