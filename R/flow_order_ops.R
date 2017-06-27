#' returns a data frame with columns id for each subbasin and upstream.
#' @param df is a data frame imported from wasa with columns upstream and downstream
#' @export
updown2idup <- function(df)
    {
        rout <- as_data_frame(df)
        ids <- data_frame(id=sort(unique(rout$upstream)))
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
    routm$strahler_upstream <- NA

    ids <- data_frame(id=sort(unique(routm$id)))
    
    strh <- data_frame(id=ids$id,strahler=NA)
    strh$strahler[strh$id %in% routm$id[routm$n==0]] <- 1
    str0 <- filter(strh,strahler==1)
    strh <- str0
    
    while(sum(is.na(filter(routm,!is.na(upstream))))>0)
    {
            routm$strahler_upstream[routm$upstream %in% strh$id] <- strh$strahler
            str1 <- group_by(routm,id) %>% summarise(strahler=sum(strahler_upstream)+1) %>% filter(!is.na(strahler))
            strh <- bind_rows(str1,str0)
    }
    
        return(strh)
}
