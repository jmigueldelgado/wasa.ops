require(dplyr)

df <- read.table("test.tbl",header=T,sep=" ") %>% as_tibble()

updown2strahler(df)
