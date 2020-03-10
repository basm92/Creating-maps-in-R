getwd()
setwd("C:/Users/Machi003/RWD/Creating-maps-in-R")

library(rgdal)
library(rvest)
library(stringr)
library(tmap)

#Data import
cntry1914 <- readOGR(dsn = "data", layer = "cntry1914")

wiki_read <- read_html("https://en.wikipedia.org/wiki/1912_Summer_Olympics")

countries <- wiki_read %>%
    html_nodes('#mw-content-text > div > table:nth-child(120) > tbody > tr:nth-child(2) > td > div ') %>%
    html_text() %>%
    str_split("\n") %>%
    data.frame() %>%
    separate(1, into = c("country", "athletes"), sep = "\\(") %>%
    mutate(athletes = as.numeric(as.character(str_replace(athletes, "\\)", "")
                                              )
                                 )
    ) %>%
    filter(!is.na(athletes)) %>%
    mutate(country = str_extract(country, "([aA-Zz].+[a-z])")) %>%
    mutate(country = recode(country, 
                            Austria = "Austro-Hungarian Empire",
                            Germany = "German Empire",
                            `Russian Empire` = "Russia", 
                            Turkey = "Turkish Empire"
                            ))
    

a <- wiki_read %>%
    html_nodes("table")

countries2 <- html_table(a[[6]]) %>%
    mutate(Nation = str_replace(Nation, "\\((.+)\\)", "")) %>%
    mutate(Nation = str_extract(Nation, "([aA-Zz].+[a-z])")) %>%
    mutate(Nation = recode(Nation, 
                           Hungary = "Austro-Hungarian Empire",
                           Germany = "German Empire"))

#Data treatment
cntry1914$OLYM <- is.element(cntry1914$NAME, countries$country)
cntry1914$ATHL <- countries$athletes[match(cntry1914$NAME,countries$country)]

cntry1914@data <- left_join(cntry1914@data, countries2, by = c('NAME' = 'Nation'))


#Data visualization
plot(cntry1914, col = "lightgrey")
plot(cntry1914[cntry1914$OLYM == TRUE,], col = "turquoise", add = TRUE)

qtm(cntry1914, "ATHL", text = "ATHL", scale = 0.5)
qtm(cntry1914, "Total")

#Write the dataset
write.csv(cntry1914@data, "olympicgames1912.csv")

#Tutorial from this document:
#https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf

#Data from this: 
#https://www.gislounge.com/find-gis-data-historical-country-boundaries/
