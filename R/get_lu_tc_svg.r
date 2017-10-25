get_tc_area <- function(l,sbi=1)
    {
        wasa_input_dir <- l$wasa_input_dir
        LUMP_tables <- l$lump_out
        lu <- getLUinfo(input_dir)
        tc <- get_tc_properties(input_dir)
        tclist <- unique(tc["tc_id"])
        lulist <- unique(tc["lu_id"])

        horizons <- read.table(paste0(LUMP_tables,"/horizons.dat"),header=TRUE) ### read soil properties and obtain porosity of soilto calculate available water holding capacity of LU
        theta_s_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        volume_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        area_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        
#        sbi <- 1
        
        for(tci in tclist[,1])
            {
                soil_in_tc <- tc$soil_id[tc$tc_id %in% tci]
                TCarea <- tc$TC_area[tc$tc_id %in% tci]
                soil_area <- tc$frac[tc$tc_id %in% tci]*TCarea
                lui <- unique(tc$lu_id[tc$tc_id %in% tci])
                theta_s_in_soil <- numeric()
                depth_in_soil <- numeric()
                for(soili in soil_in_tc)
                    {
                        hor_in_soil <- horizons[horizons$soil_id %in% soil_in_tc,]
                        theta_s_in_soil <- c(theta_s_in_soil,sum(hor_in_soil$theta_s*hor_in_soil$thickness)/sum(hor_in_soil$thickness))
                        depth_in_soil <- c(depth_in_soil,sum(hor_in_soil$thickness))
                    }
                theta_s_in_tc <- sum(tc$frac[tc$tc_id %in% tci]*theta_s_in_soil)
                depth_in_tc <- sum(tc$frac[tc$tc_id %in% tci]*depth_in_soil)
                
                position <- tc$TCposition[tc$tc_id %in% tci]

                switch(
                    position[1],
                    theta_s_lu$cabeceira[theta_s_lu$pid==lui] <- theta_s_in_tc,
                    theta_s_lu$encosta[theta_s_lu$pid==lui] <- theta_s_in_tc,
                    theta_s_lu$vale[theta_s_lu$pid==lui] <- theta_s_in_tc
                    )
                switch(
                    position[1],
                    volume_lu$cabeceira[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000,
                    volume_lu$encosta[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000,
                    volume_lu$vale[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000 #### in m3
                    )
                switch(
                    position[1],
                    area_lu$cabeceira[area_lu$pid==lui] <- TCarea[1]*1000000,
                    area_lu$encosta[area_lu$pid==lui] <- TCarea[1]*1000000,
                    area_lu$vale[area_lu$pid==lui] <- TCarea[1]*1000000 ### in m2
                    )
                
            }
        
        volume_lu$total <- apply(volume_lu[c("vale","encosta","cabeceira")],1,sum)
        area_lu$total <- apply(area_lu[c("vale","encosta","cabeceira")],1,sum)
        
        theta_s_lu$total <- apply(area_lu[c("vale","encosta","cabeceira")]*theta_s_lu[c("vale","encosta","cabeceira")],1,sum)/area_lu$total
        return(area_lu)
    }

get_tc_volume <- function(l,sbi=1)
    {
        wasa_input_dir <- l$wasa_input_dir
        LUMP_tables <- l$lump_out
        cat(LUMP_tables,"\n")
        lu <- getLUinfo(l)
        tc <- get_tc_properties(l)
        tclist <- unique(tc["tc_id"])
        lulist <- unique(tc["lu_id"])

        horizons <- read.table(paste0(LUMP_tables,"/horizons.dat"),header=TRUE) ### read soil properties and obtain porosity of soilto calculate available water holding capacity of LU
        theta_s_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        volume_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        area_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        
#        sbi <- 1
        
        for(tci in tclist[,1])
            {
                soil_in_tc <- tc$soil_id[tc$tc_id %in% tci]
                TCarea <- tc$TC_area[tc$tc_id %in% tci]
                soil_area <- tc$frac[tc$tc_id %in% tci]*TCarea
                lui <- unique(tc$lu_id[tc$tc_id %in% tci])
                theta_s_in_soil <- numeric()
                depth_in_soil <- numeric()
                for(soili in soil_in_tc)
                    {
                        hor_in_soil <- horizons[horizons$soil_id %in% soil_in_tc,]
                        theta_s_in_soil <- c(theta_s_in_soil,sum(hor_in_soil$theta_s*hor_in_soil$thickness)/sum(hor_in_soil$thickness))
                        depth_in_soil <- c(depth_in_soil,sum(hor_in_soil$thickness))
                    }
                theta_s_in_tc <- sum(tc$frac[tc$tc_id %in% tci]*theta_s_in_soil)
                depth_in_tc <- sum(tc$frac[tc$tc_id %in% tci]*depth_in_soil)
                
                position <- tc$TCposition[tc$tc_id %in% tci]

                switch(
                    position[1],
                    theta_s_lu$cabeceira[theta_s_lu$pid==lui] <- theta_s_in_tc,
                    theta_s_lu$encosta[theta_s_lu$pid==lui] <- theta_s_in_tc,
                    theta_s_lu$vale[theta_s_lu$pid==lui] <- theta_s_in_tc
                    )
                switch(
                    position[1],
                    volume_lu$cabeceira[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000,
                    volume_lu$encosta[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000,
                    volume_lu$vale[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000 #### in m3
                    )
                switch(
                    position[1],
                    area_lu$cabeceira[area_lu$pid==lui] <- TCarea[1]*1000000,
                    area_lu$encosta[area_lu$pid==lui] <- TCarea[1]*1000000,
                    area_lu$vale[area_lu$pid==lui] <- TCarea[1]*1000000 ### in m2
                    )
                
            }
        
        volume_lu$total <- apply(volume_lu[c("vale","encosta","cabeceira")],1,sum)
        area_lu$total <- apply(area_lu[c("vale","encosta","cabeceira")],1,sum)
        
        theta_s_lu$total <- apply(area_lu[c("vale","encosta","cabeceira")]*theta_s_lu[c("vale","encosta","cabeceira")],1,sum)/area_lu$total
        return(volume_lu)
    }

get_tc_theta_s <- function(l,sbi=1)
    {
        wasa_input_dir <- l$wasa_input_dir
        LUMP_tables <- l$lump_out
        lu <- getLUinfo(l)
        tc <- get_tc_properties(l)
        tclist <- unique(tc["tc_id"])
        lulist <- unique(tc["lu_id"])

        horizons <- read.table(paste0(LUMP_tables,"/horizons.dat"),header=TRUE) ### read soil properties and obtain porosity of soilto calculate available water holding capacity of LU
        theta_s_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        volume_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        area_lu <- data.frame(pid=lu$id,total=rep(NA,nrow(lu)),vale=rep(NA,nrow(lu)),encosta=rep(NA,nrow(lu)),cabeceira=rep(NA,nrow(lu))) #### in LUMP TC count from bottom to top of hillslope
        
#        sbi <- 1
        
        for(tci in tclist[,1])
            {
                soil_in_tc <- tc$soil_id[tc$tc_id %in% tci]
                TCarea <- tc$TC_area[tc$tc_id %in% tci]
                soil_area <- tc$frac[tc$tc_id %in% tci]*TCarea
                lui <- unique(tc$lu_id[tc$tc_id %in% tci])
                theta_s_in_soil <- numeric()
                depth_in_soil <- numeric()
                for(soili in soil_in_tc)
                    {
                        hor_in_soil <- horizons[horizons$soil_id %in% soil_in_tc,]
                        theta_s_in_soil <- c(theta_s_in_soil,sum(hor_in_soil$theta_s*hor_in_soil$thickness)/sum(hor_in_soil$thickness))
                        depth_in_soil <- c(depth_in_soil,sum(hor_in_soil$thickness))
                    }
                theta_s_in_tc <- sum(tc$frac[tc$tc_id %in% tci]*theta_s_in_soil)
                depth_in_tc <- sum(tc$frac[tc$tc_id %in% tci]*depth_in_soil)
                
                position <- tc$TCposition[tc$tc_id %in% tci]

                switch(
                    position[1],
                    theta_s_lu$cabeceira[theta_s_lu$pid==lui] <- theta_s_in_tc,
                    theta_s_lu$encosta[theta_s_lu$pid==lui] <- theta_s_in_tc,
                    theta_s_lu$vale[theta_s_lu$pid==lui] <- theta_s_in_tc
                    )
                switch(
                    position[1],
                    volume_lu$cabeceira[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000,
                    volume_lu$encosta[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000,
                    volume_lu$vale[volume_lu$pid==lui] <- 0.001*depth_in_tc*TCarea[1]*1000000 #### in m3
                    )
                switch(
                    position[1],
                    area_lu$cabeceira[area_lu$pid==lui] <- TCarea[1]*1000000,
                    area_lu$encosta[area_lu$pid==lui] <- TCarea[1]*1000000,
                    area_lu$vale[area_lu$pid==lui] <- TCarea[1]*1000000 ### in m2
                    )
                
            }
        
        volume_lu$total <- apply(volume_lu[c("vale","encosta","cabeceira")],1,sum)
        area_lu$total <- apply(area_lu[c("vale","encosta","cabeceira")],1,sum)
        
        theta_s_lu$total <- apply(area_lu[c("vale","encosta","cabeceira")]*theta_s_lu[c("vale","encosta","cabeceira")],1,sum)/area_lu$total
        return(theta_s_lu)
    }


getLUinfo <- function(l)
    {
        wasa_input_dir <- l$wasa_input_dir
        flu <- file(paste0(wasa_input_dir,"/Hillslope/hymo.dat"))
        lulines <- readLines(flu)
        lus <- data.frame(subbas_id=numeric(),subbas_area=numeric(),id=numeric(),areakm2=numeric(),LUfrac=numeric())
        for(i in seq(3,length(lulines)))
            {
                linei <- scan(flu,skip=i-1,nlines=1)
                nLU <- linei[3]
                area <- linei[2]
                subbas_id <- data.frame(subbas_id=rep(linei[1],nLU))
                subbas_area <- data.frame(subbas_area=rep(area,nLU))
                LUarea <- data.frame(areakm2=area*linei[(3+nLU+1):(3+2*nLU)])
                LUid <- data.frame(id=linei[(3+1):(3+nLU)])
                LUfrac <- data.frame(frac=linei[(3+nLU+1):(3+2*nLU)])
                lus <- rbind(lus,cbind(subbas_id,subbas_area,LUid,LUarea,LUfrac))            
            }
        return(lus)
    }


getTCfrac <- function(l)
    {
        wasa_input_dir <- l$wasa_input_dir
        ftc <- file(paste0(wasa_input_dir,"/Hillslope/terrain.dat"))
        tclines <- readLines(ftc)
        tcs <- data.frame(id=numeric(),frac=numeric(),position=numeric())

        for(i in seq(3,length(tclines)))
            {
                linei <- scan(ftc,skip=i-1,nlines=1)
                id <- linei[1]
                frac <- linei[2]
                position <- linei[4]
                tcs <- rbind(tcs,cbind(id,frac,position))            
            }
        return(tcs)
        
    }
#

get_tc_properties <- function(l)
{
    wasa_input_dir <- l$wasa_input_dir
    
    f <- file(paste0(wasa_input_dir,"/Hillslope/soil_vegetation.dat"))
    svclines <- readLines(f)


    TCfrac <- getTCfrac(l)
    LUinfo <- getLUinfo(l)
    
            
    getsoils <- function(i)
        {
            linei <- scan(f,skip=i-1,nlines=1)
            soils <- linei[(5+1):(5+linei[5])]
            return(soils)
        }
    getvegs <- function(i)
        {
            linei <- scan(f,skip=i-1,nlines=1)
            vegs <- linei[(5+1):(5+linei[5])]
            return(vegs)
        }
    getfracs <- function(i) 
        {
            linei <- scan(f,skip=i-1,nlines=1)
            fracs <- linei[(5+1):(5+linei[5])]
            return(fracs)
        }
    getnbrSVCs <- function(i) 
        {
            linei <- scan(f,skip=i-1,nlines=1)
            nbrSVCs <- linei[5]
            return(nbrSVCs)
        }
    getsubbasID <- function(i) 
        {
            linei <- scan(f,skip=i-1,nlines=1)
            subbasID <- linei[1]
            return(subbasID)
        }
    getLUid <- function(i) 
        {
            linei <- scan(f,skip=i-1,nlines=1)
            LUid <- linei[2]
            return(LUid)
        }
    getLUarea <- function(i)
        {
            id <- getLUid(i)
            area <- LUinfo$areakm2[LUinfo$id==id]
            return(area)
        }
    getTCid <- function(i) 
        {
            linei <- scan(f,skip=i-1,nlines=1)
            TCid <- linei[3]
            return(TCid)
        }
    getTCarea <- function(i)
        {
            id <- getTCid(i)
            area <- TCfrac$frac[TCfrac$id==id]*LUareai
            return(area)
        }
    getTCposition <- function(i)
        {
            id <- getTCid(i)
            position <- TCfrac$position[TCfrac$id==id]
            return(position)
        }
    
    tbl <- data.frame(subbas_id=numeric(),LU_id=numeric(),LUarea=numeric(),TC_id=numeric(),TCarea=numeric(),soils=numeric(),vegs=numeric(),fracs=numeric(),TCposition=numeric())

    for(i in seq(4,length(svclines),3))
        {
            nbrSVCs <- getnbrSVCs(i)
            soils <- data.frame(soil_id=getsoils(i))
            vegs <- data.frame(veg_id=getvegs(i+1))
            fracs <- data.frame(frac=getfracs(i+2))
            subbas <- data.frame(subbas_id=rep(getsubbasID(i),nbrSVCs))

            lui <- getLUid(i)
            LU <- data.frame(lu_id=rep(lui,nbrSVCs))
            
            LUareai <- getLUarea(i)
            LUarea <- data.frame(LU_area=rep(LUareai,nbrSVCs))

            tci <- getTCid(i)
            TC <- data.frame(tc_id=rep(tci,nbrSVCs))

            TCareai <- getTCarea(i)
            TCarea <- data.frame(TC_area=rep(TCareai,nbrSVCs))

            TCposition <- getTCposition(i)
            tbl <- rbind(tbl,cbind(subbas,LU,LUarea,TC,TCarea,soils,vegs,fracs,TCposition))           
        }
    return(tbl)
}
