#Loading Library
library(rvest)
library(dplyr)

#Creating data frame
Movies = data.frame()

for (page_result in seq(from = 1, to = 2000, by = 100)) {
  #URL link
  link = paste0("https://www.the-numbers.com/box-office-records/worldwide/all-Movies/cumulative/all-time/",page_result)

  #Read the HTML content of the page
  page = read_html(link)
  
  #Fetching data from webpage
  rank = page %>% html_nodes(".data:nth-child(1)") %>% html_text() 
  year = page %>% html_nodes(".data a") %>% html_text()
  name = page %>% html_nodes("#page_filling_chart b a") %>% html_text()
  worldwide_box_office = page %>% html_nodes("td:nth-child(4)") %>% html_text()
  domestic_box_office = page %>% html_nodes("td:nth-child(5)") %>% html_text()
  international_box_office = page %>% html_nodes("td:nth-child(6)") %>% html_text()
  
  #Adding data into Data Frame
  Movies = rbind(Movies, data.frame(rank,name,year,worldwide_box_office,domestic_box_office,
                                      international_box_office, stringsAsFactors = FALSE))

}

#Storing the data frame into csv file for future use
write.csv(Movies, "Movies_data.csv", append = TRUE)
print(Movies)
