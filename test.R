require(dplyr)

df <- read.table("test.tbl",header=T,sep=" ") %>% as_tibble()
