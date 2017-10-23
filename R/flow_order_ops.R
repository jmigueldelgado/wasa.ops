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

    routm <- df %>%
        rename(id=upstream) %>%
        mutate(strahler=NA)

    while(sum(is.na(routm$strahler))>0)
    {
        rout0 <-  filter(routm,is.na(strahler)) %>% slice(1) %>%
            mutate(strahler=ifelse(sum(.$id %in% routm$downstream)==0,1,max(routm$strahler[routm$downstream %in% .$id])+1))
        routm[routm$id==rout0$id,] <- rout0
    }

    return(routm)
}
