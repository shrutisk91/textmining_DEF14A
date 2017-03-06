library(rvest)
library(plyr)

For year 2016
url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312516534905/d147599ddef14a.htm"
gs2016 <- url %>%
  read_html(encoding="ASCII") %>%
  html_nodes(xpath='/html/body/document/type/sequence/filename/description/text/table[188]') %>%
  html_table(fill=TRUE)
gs2016 <- gs2016[[1]]
gs2016 <- gs2016[3:5, c("X1", "X3", "X5", "X7", "X9", "X13", "X15")]
gssummary2016 <- rename(gs2016, c("X1"="Name and Principal Position","X3"="Year", "X5"="Salary", "X7"= "Bonus","X9"="Stock Awards", "X13" = "All Other Compensations" , "X15"= "Total Compensation")) 
write.csv(gssummary2016,file="gs2016.csv")

For year 2015
url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312515125238/d904615ddef14a.htm"
gs2015 <- url %>%
  read_html(encoding="ASCII") %>%
  html_nodes(xpath='/html/body/document/type/sequence/filename/description/text/table[164]') %>%
  html_table(fill=TRUE)
gs2015<- gs2015[[1]]
gs2015 <- gs2015[3:5, c("X1", "X4", "X8", "X12", "X16", "X24", "X28")]
gssummary2015 <- rename(gs2015, c("X1"="Name and Principal Position","X4"="Year", "X8"="Salary", "X12"= "Bonus","X16"="Stock Awards", "X24" = "All Other Compensations" , "X28"= "Total Compensation")) 
write.csv(gssummary2015,file="gs2015.csv")

For year 2014
url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312514131134/d627791ddef14a.htm"
gs2014 <- url %>%
  read_html(encoding="ASCII") %>%
  html_nodes(xpath='/html/body/document/type/sequence/filename/description/text/table[176]') %>%
  html_table(fill=TRUE)
gs2014 <- gs2014[[1]]
gs2014 <- gs2014[3:5, c("X1", "X4", "X8", "X12", "X16", "X28", "X32")]
gssummary2014 <- rename(gs2014, c("X1"="Name and Principal Position","X4"="Year", "X8"="Salary", "X12"= "Bonus","X16"="Stock Awards", "X28" = "All Other Compensations" , "X32"= "Total Compensation")) 
write.csv(gssummary2014,file="gs2014.csv")

For year 2013
url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312513152411/d447333ddef14a.htm"
gs2013 <- url %>%
  read_html(encoding="ASCII") %>%
  html_nodes(xpath='/html/body/document/type/sequence/filename/description/text/table[173]') %>%
  html_table(fill=TRUE)
gs2013 <- gs2013[[1]]
gs2013 <- gs2013[3:5, c("X1", "X4", "X8", "X12", "X16", "X28", "X32")]
gssummary2013 <- rename(gs2013, c("X1"="Name and Principal Position","X4"="Year", "X8"="Salary", "X12"= "Bonus","X16"="Stock Awards", "X28" = "All Other Compensations" , "X32"= "Total Compensation")) 
write.csv(gssummary2013,file="gs2013.csv")

For year 2010
url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312510078005/ddef14a.htm"
gs2010 <- url %>%
  read_html(encoding="ASCII") %>%
  html_nodes(xpath='/html/body/document/type/sequence/filename/description/text/table[102]') %>%
  html_table(fill=TRUE)
gs2010 <- gs2010[[1]]
gs2010 <- gs2010[3:5, c("X1", "X3", "X6", "X9", "X12", "X15", "X21", "X24")]
gssummary2010 <- rename(gs2010, c("X1"="Name and Principal Position","X3"="Year", "X6"="Salary", "X9"= "Bonus","X12"="Stock Awards","X15" = "Option Awards", "X21" = "All Other Compensations" , "X24"= "Total Compensation")) 
write.csv(gssummary2010,file="gs2010.csv")

#For year 2011
url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312511086865/ddef14a.htm"
gs2011 <- url %>%
  read_html(encoding="ASCII") %>%
  html_nodes(xpath='/html/body/document/type/sequence/filename/description/text/table[114]') %>%
  html_table(fill=TRUE)
gs2011 <- gs2011[[1]]
gs2011 <- gs2011[3:5, c("X1", "X4", "X8", "X12", "X16", "X20", "X28", "X32")]
gssummary2011 <- rename(gs2011, c("X1"="Name and Principal Position","X4"="Year", "X8"="Salary", "X12"= "Bonus","X16"="Stock Awards","X20" = "Option Awards", "X28" = "All Other Compensations" , "X32"= "Total Compensation")) 
write.csv(gssummary2011,file="gs2011.csv")