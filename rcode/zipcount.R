zipcount <- function(fileURL,zipCode) {
## requires library RCurl and XML
  # takes an XML URL, parses the data and returns number of zipcodes matching 21231
  xData <- getURL(fileURL)
  doc <- xmlParse(xData)
  zip <- xpathSApply(doc, "/response/row/row/zipcode", xmlValue)
  sum(zip==zipCode)
}