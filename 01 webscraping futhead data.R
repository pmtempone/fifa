### Prepare ####   
pkg <- c("cluster","fpc","digest","ggplot2","foreign","ggdendro","reshape2")   
inst <- pkg %in% installed.packages()   
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])   
lapply(pkg,library,character.only=TRUE)   
rm(inst,pkg)   
set.seed(4444)  



### Control panel for screen-scraping ####   
sleep.time <- 0.01   
pagecount <- 6   
pc.ignore <- 0   
names.page <- 48   
names.lastpage <- 38   
name.gaplines <- 71   
namLine1 <- 1723   
posLine1 <- 1723+5   
RATLine1 <- 1723+10   
PACLine1 <- 1723+15   
SHOLine1 <- 1723+20   
PASLine1 <- 1723+25   
DRILine1 <- 1723+30   
DEFLine1 <- 1723+34   
PHYLine1 <- 1723+37   

### Create custom urls to scrape ####   
pageSeq <- seq(from=1,to=pagecount,by=1)   
urls.df <- data.frame(pageSeq)   
for(i in 1:length(urls.df$pageSeq)){   
  urls.df$url[i] <- paste0("http://www.futhead.com/15/players/?club=all&league=353&page=",   
                           urls.df$pageSeq[i]   
                           )   
}   

### Scrape html from custom urls ####   
pages <- as.list("na")   
for(j in 1:length(urls.df$pageSeq)){   
  pages[[j]] <- urls.df$pageSeq[j]   
}   
for(j in 1:length(urls.df$pageSeq)){   
  download.file(urls.df$url[j],destfile=paste0(urls.df$pageSeq[j],".txt"))   
  Sys.sleep(sleep.time)   
}   


### Identify which lines store player statistics ####   
namSeq <- seq(from=namLine1,by=name.gaplines,length.out=names.page)   
posSeq <- seq(from=posLine1,by=name.gaplines,length.out=names.page)   
RATSeq <- seq(from=RATLine1,by=name.gaplines,length.out=names.page)   
PACSeq <- seq(from=PACLine1,by=name.gaplines,length.out=names.page)   
SHOSeq <- seq(from=SHOLine1,by=name.gaplines,length.out=names.page)   
PASSeq <- seq(from=PASLine1,by=name.gaplines,length.out=names.page)   
DRISeq <- seq(from=DRILine1,by=name.gaplines,length.out=names.page)   
DEFSeq <- seq(from=DEFLine1,by=name.gaplines,length.out=names.page)   
PHYSeq <- seq(from=PHYLine1,by=name.gaplines,length.out=names.page)   

### Create empty dataframe for storing player stats   
attribs <- data.frame(matrix(nrow=names.page*(pagecount-1)+names.lastpage,ncol=9))   
colnames(attribs) <- c("Name","Position","RAT","PAC","SHO","PAS","DRI","DEF","PHY")   

### Store lines from full pages containing player stats to dataframe ####   
for(m in 1:(pagecount-1-pc.ignore)){   
  page <- readLines(paste0(urls.df$pageSeq[m],".txt"))   
  for(k in 1:names.page){   
    n <- (m-1)*names.page+k   
    attribs$Name[n] <- page[namSeq[k]]   
    attribs$Position[n] <- page[posSeq[k]]   
    attribs$RAT[n] <- page[RATSeq[k]]   
    attribs$PAC[n] <- page[PACSeq[k]]   
    attribs$SHO[n] <- page[SHOSeq[k]]   
    attribs$PAS[n] <- page[PASSeq[k]]   
    attribs$DRI[n] <- page[DRISeq[k]]   
    attribs$DEF[n] <- page[DEFSeq[k]]   
    attribs$PHY[n] <- page[PHYSeq[k]]   
  }   
}   

### Store lines from partial last page containing player stats to dataframe ####   
pagelast <- readLines(paste0(urls.df$pageSeq[pagecount],".txt"))   
for(p in 1:names.lastpage){   
  q <- (pagecount-1)*names.page+p   
  attribs$Name[q] <- pagelast[namSeq[p]]   
  attribs$Position[q] <- pagelast[posSeq[p]]   
  attribs$RAT[q] <- pagelast[RATSeq[p]]   
  attribs$PAC[q] <- pagelast[PACSeq[p]]   
  attribs$SHO[q] <- pagelast[SHOSeq[p]]   
  attribs$PAS[q] <- pagelast[PASSeq[p]]   
  attribs$DRI[q] <- pagelast[DRISeq[p]]   
  attribs$DEF[q] <- pagelast[DEFSeq[p]]   
  attribs$PHY[q] <- pagelast[PHYSeq[p]]   
}   

### Remove html wrapped around player stats in each line ####   
attribs$Name <- gsub("^.*<span class="name">","",attribs$Name)   
attribs$Name <- gsub("</span>.*$","",attribs$Name)   
attribs$Name <- gsub("^\s+|\s+$","",attribs$Name)   
attribs$Position <- gsub("^ *","",attribs$Position)   
attribs$Position <- gsub("^\s+|\s+$","",attribs$Position)   
attribs$RAT <- gsub("^.*<span>","",attribs$RAT)   
attribs$RAT <- gsub("</span>.*$","",attribs$RAT)   
attribs$RAT <- gsub("^\s+|\s+$","",attribs$RAT)   
attribs$PAC <- gsub("^.*<span class="attribute">","",attribs$PAC)   
attribs$PAC <- gsub("</span>.*$","",attribs$PAC)   
attribs$PAC <- gsub("^\s+|\s+$","",attribs$PAC)   
attribs$SHO <- gsub("^.*<span class="attribute">","",attribs$SHO)   
attribs$SHO <- gsub("</span>.*$","",attribs$SHO)   
attribs$SHO <- gsub("^\s+|\s+$","",attribs$SHO)   
attribs$PAS <- gsub("^.*<span class="attribute">","",attribs$PAS)   
attribs$PAS <- gsub("</span>.*$","",attribs$PAS)   
attribs$PAS <- gsub("^\s+|\s+$","",attribs$PAS)   
attribs$DRI <- gsub("^.*<span class="attribute">","",attribs$DRI)   
attribs$DRI <- gsub("</span>.*$","",attribs$DRI)   
attribs$DRI <- gsub("^\s+|\s+$","",attribs$DRI)   
attribs$DEF <- gsub("^.*<span class="attribute">","",attribs$DEF)   
attribs$DEF <- gsub("</span>.*$","",attribs$DEF)   
attribs$DEF <- gsub("^\s+|\s+$","",attribs$DEF)   
attribs$PHY <- gsub("^.*<span class="attribute">","",attribs$PHY)   
attribs$PHY <- gsub("</span>.*$","",attribs$PHY)   
attribs$PHY <- gsub("^\s+|\s+$","",attribs$PHY)   

### Remove statistics from duplicated players ####   
attribs <- attribs[!(attribs$Name=="Cristiano Ronaldo"&attribs$RAT=="93"),]   
attribs <- attribs[!duplicated(attribs$Name),]   
rownames(attribs) <- NULL   

### Clean up foreign characters in names ####   
Encoding(attribs$Name) <- "UTF-8"   
attribs$Name <- iconv(attribs$Name,"UTF-8","UTF-8",sub='')   

