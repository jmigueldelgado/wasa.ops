#' returns a data frame with columns id for each subbasin and upstream.
#' @param df is a data frame imported from wasa with columns upstream and downstream
#' @export
updown2idup <- function(df)
    {
        rout <- as_data_frame(df)
        ids <- data_frame(id=sort(unique(c(rout$upstream,rout$downstream))))
        routm <- data_frame(id=integer(),upstream=integer())
        for(i in ids$id)
        {
            tmp <- select(rout,downstream,upstream) %>% filter(downstream==i) %>% rename(id=downstream,upstream=upstream)
            routm <- bind_rows(routm,tmp)
        }
        routm <- left_join(ids,routm)
        return(routm)
}

#' attributes strahler order to each basins after reading the routing table from wasa
#' @param df is a data frame imported from wasa with columns upstream and downstream
#' @export
updown2strahler <- function(df)
{

    routm <- updown2idup(df)
    cnt <- count(routm,id)   
    routm <- left_join(cnt,routm)
    routm$n[is.na(routm$upstream)] <- 0

    str <- 1
    
    routm <- routm %>%
        mutate(strahler=NA,strahler_upstream=NA) %>%
        filter(n==0) %>%
        mutate(strahler=str) %>%
        left_join(routm,.)
    
    while(sum(is.na(routm$strahler))>0)
    {
        rout0 <- filter(routm,strahler==str)
        routm$strahler_upstream[routm$upstream %in% rout0$id] <- str

        str <- str+1
        rout_na <- filter(routm,is.na(strahler))
        rout1 <- group_by(rout_na,id) %>%
            summarise(strahler=max(strahler_upstream)+1)
        routm$strahler[routm$id %in% rout1$id] <- rout1$strahler
    }
   strh <- group_by(routm,id) %>% summarise(strahler=max(strahler))
        return(strh)
}
