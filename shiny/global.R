library(shiny)
library(pdftools)
library(XML)
library(stringr)
library(RCurl)

#Locate and download file==============

#parse the page
htmlp <- htmlParse('http://www.pc.gc.ca/en/pn-np/ab/banff/activ/randonee-backpacking')

#find all the PDFs (should only be one but this will work if they add more)
links <- htmlp["//a[contains(@href,'.pdf')]"]

#get description for those links and grab the one we want
link_desc <- sapply(links,function(x){xmlValue(x)})
index <- grep("Backcountry campground vacancy",link_desc)
if(length(index)==0) stop("Vacancy Report Not Found")

#then pull the link for the report
xml_data <- unlist(links)[[index]]
camp_link <- str_extract(xmlAttrs(xml_data),"//.+")
camp_link <- paste0("https:",camp_link)

#download the PDF
#https://stackoverflow.com/questions/25816986/using-a-downloaded-external-file-in-shiny-app
camp_pdf <- RCurl::getURLContent(camp_link)

#store the modified date which is in the link
modified <- as.Date(substring(str_extract(camp_link,"modified=\\d{8}"),10,18),format="%Y%m%d")


#Read and transform file=============

#read in PDF
pdf_txt <- pdf_text(camp_pdf)

#convert text to list
#note I had orginially split on "*\r\n" but when you publish there is no \r for some reason so I changed to just \n 
campList <- strsplit(pdf_txt,"*\n")

#remove extra header from first page
campList[[1]] <- campList[[1]][-(1:2)] 

#transform each element (a.k.a. page) of list into data.frame
camp_df <- lapply(campList,
                 function(x)
                 {vecLength <- length(x)
                 #remove 5 rows of header data and 3 rows of footer data to only keep the dates (only substract 2 to grab 3 rows)
                 dateChar <- x[-c(1:5,(vecLength-2):vecLength)]
                 dfLength <- length(dateChar)
                 #remove spaces from date string, will use this in later transformation
                 noSpaceVar <- gsub("[[:blank:]]","",dateChar)
                 #first number on third line of page contains total number of sites at campground
                 totalSites <- as.numeric(str_extract(x[3],"\\d+"))
                 #create our data frame
                 return(data.frame(rawTxt=dateChar,
                            noSpace=noSpaceVar,
                            area=rep(x[1],dfLength), #first line is area
                            campground=rep(x[2],dfLength), #second is campground
                            totalSites=rep(totalSites,dfLength)))
                 }
                 )

#each data.frame is a separate item in list. combine all into one
camp_df <- as.data.frame(do.call(rbind,camp_df))

#clean up column types
camp_df$rawTxt <- as.character(camp_df$rawTxt)
camp_df$noSpace <- as.character(camp_df$noSpace)
camp_df$area <- as.character(camp_df$area)
camp_df$campground <- as.character(camp_df$campground)

#add new columns
camp_df$year <- str_extract(camp_df$rawTxt,"\\d{4}") #the 4 digit year

#this grabs the month which is between the two commas
#had to str_extract twice to remove the first comma
camp_df$month <- str_extract(str_extract(camp_df$noSpace,",\\w+[^0-9,]"),"[A-z]+")

camp_df$day <- str_extract(camp_df$noSpace,"\\d+") #the first digit in the string is the day
camp_df$date <- paste(camp_df$year,camp_df$month ,camp_df$day,sep="-")
camp_df$date <- as.Date(camp_df$date,'%Y-%B-%d')

#the last number in the row is the number of sites available, but need to make sure we include if it is negative
camp_df$sitesAvail <- as.numeric(str_extract(camp_df$rawTxt,"-?\\d+$"))

#remove unecessary columns
camp_df <- camp_df[,!(names(camp_df) %in% c("rawTxt","noSpace","year","month","day"))]

#report any null values
#Takakkaw Falls has an extra header line so that is why it shows up (3 entries for the three pages it is on)
camp_df[!complete.cases(camp_df),]

#remove null values
camp_df <- camp_df[complete.cases(camp_df),]
