
locals <- function(cenario,nome,wasa_home,lump_home)
    {
        l <- list()
        l$cenario <- cenario
        l$cenario0 <- "default"
        l$wasa_home <- wasa_home
        l$lump_home <- lump_home
        l$lump_out <- paste(l$lump_home,cenario,nome,"LUMP_out",sep="/")
        l$lump_out0 <- paste(l$lump_home,l$cenario0,nome,"LUMP_out",sep="/")
        l$soil_data <- paste(l$lump_home,"out_t",sep="/")
        l$db_out <- paste(l$lump_home,cenario,nome,"db_out",sep="/")
        l$dbname <- "mysqlitedb"

        l$wasa_input_dir <- paste(l$wasa_home,cenario,nome,"Input/",sep="/")

        l$wasa_output_dir <- paste(l$wasa_home,cenario,nome,"Output/",sep="/")
        return(l)
    }

