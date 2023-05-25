#Loading CSV file into Data Frame
Movies_DF <- read.csv("Movies_data.csv")

Movies_DF

#Dropping the X column
Movies_DF <- Movies_DF[c(-1)] 

#Handling Missing Data
#Replacing "missing value" with NA  
Movies_DF[Movies_DF == ""] <- NA

#Dropping rows with NA 
Movies_DF<- na.omit(Movies_DF)

#Removing $ and , from the dataset
Movies_DF$rank <- gsub("\\$|,", "", as.character(Movies_DF$rank))
Movies_DF$worldwide_box_office <- gsub("\\$|,", "", as.character(Movies_DF$worldwide_box_office))
Movies_DF$domestic_box_office<-gsub("\\$|,", "", as.character(Movies_DF$domestic_box_office))
Movies_DF$international_box_office<-gsub("\\$|,", "", as.character(Movies_DF$international_box_office))

#Data Transformation
#Converting data into numeric
Movies_DF <- transform(Movies_DF,
                       rank = as.numeric(rank),
                       year = as.numeric(year),
                       worldwide_box_office = as.numeric(worldwide_box_office), 
                       domestic_box_office = as.numeric(domestic_box_office), 
                       international_box_office = as.numeric(international_box_office))

#Smoothing Noisy Data
#Finding Outliers
max(Movies_DF$worldwide_box_office)
min(Movies_DF$worldwide_box_office)
boxplot(Movies_DF$worldwide_box_office)
worldwide_box_office_sorted <- Movies_DF[order(Movies_DF$worldwide_box_office),"worldwide_box_office"]
worldwide_box_office_sorted

max(Movies_DF$international_box_office)
min(Movies_DF$international_box_office)
boxplot(Movies_DF$international_box_office)
international_box_office_sorted <- Movies_DF[order(Movies_DF$international_box_office),"international_box_office"]
international_box_office_sorted

max(Movies_DF$domestic_box_office)
min(Movies_DF$domestic_box_office)
boxplot(Movies_DF$domestic_box_office)
domestic_box_office_sorted <- Movies_DF[order(Movies_DF$domestic_box_office),"domestic_box_office"]
domestic_box_office_sorted

#Removing Outlier
Movies_DF <- subset(Movies_DF, international_box_office >= 100000)
Movies_DF <- subset(Movies_DF, domestic_box_office >= 100000)



#Data Reduction
# Create a new column called "worldwide_box_office_million" by dividing "worldwide_box_office" by 1 million
Movies_DF$worldwide_box_office_million <- Movies_DF$worldwide_box_office / 1000000

# Create a new column called "domestic_box_office_million" by dividing "domestic_box_office" by 1 million
Movies_DF$domestic_box_office_million <- Movies_DF$domestic_box_office / 1000000

# Create a new column called "international_box_office_million" by dividing "international_box_office" by 1 million
Movies_DF$international_box_office_million <- Movies_DF$international_box_office / 1000000

#Dropping the column
Movies_DF <- Movies_DF[c(-4,-5,-6)] 

#Renaming the column
library(dplyr)
Movies_DF <- rename(Movies_DF, worldwide_box_office = "worldwide_box_office_million",
               domestic_box_office= "domestic_box_office_million",
               international_box_office = "international_box_office_million")

#Descriptive statistics
#Mean
mean_worldwide_box_office <- mean(Movies_DF$worldwide_box_office)
paste("Mean of worldwide box office :", mean_worldwide_box_office)

mean_domestic_box_office <- mean(Movies_DF$domestic_box_office)
paste("Mean of domestic box office :", mean_domestic_box_office)

mean_international_box_office <- mean(Movies_DF$international_box_office)
paste("Mean of international box office :", mean_international_box_office)

#Median
median_worldwide_box_office <- median(Movies_DF$worldwide_box_office)
paste("Median of worldwide box office :", median_worldwide_box_office)

median_domestic_box_office <- median(Movies_DF$domestic_box_office)
paste("Median of domestic box office :", median_domestic_box_office)

median_international_box_office <- median(Movies_DF$international_box_office)
paste("Median of international box office :", median_international_box_office)

#Mode
mode <- function(x){
  unique_values <- unique(x)
  table <- tabulate(match(x, unique_values))
  unique_values[table == max(table)]
}

paste("Mode of year :",mode(Movies_DF$year))

#Range
range_worldwide_box_office <- max(Movies_DF$worldwide_box_office) - min(Movies_DF$worldwide_box_office)
paste("Range of worldwide box office :", range_worldwide_box_office)

range_domestic_box_office <- max(Movies_DF$domestic_box_office) - min(Movies_DF$domestic_box_office)
paste("Range of domestic box office :", range_domestic_box_office)

range_international_box_office <- max(Movies_DF$international_box_office) - min(Movies_DF$international_box_office)
paste("Range of international box office :", range_international_box_office)

#variance
variance_worldwide_box_office <- var(Movies_DF$worldwide_box_office)
paste("Variance of worldwide box office :", variance_worldwide_box_office)

variance_domestic_box_office <- var(Movies_DF$domestic_box_office)
paste("Variance of domestic box office :", variance_domestic_box_office)

variance_international_box_office <- var(Movies_DF$international_box_office)
paste("Variance of international box office :", variance_international_box_office)

#Standard Deviation
standard_deviation_worldwide_box_office <- sd(Movies_DF$worldwide_box_office)
paste("Standard Deviation of worldwide box office :", standard_deviation_worldwide_box_office)

standard_deviation_domestic_box_office <- sd(Movies_DF$domestic_box_office)
paste("Standard Deviation of domestic box office :", standard_deviation_domestic_box_office)

standard_deviation_international_box_office <- sd(Movies_DF$international_box_office)
paste("Standard Deviation of international box office :", standard_deviation_international_box_office)

#Quantile
quantile(Movies_DF$worldwide_box_office)

quantile(Movies_DF$domestic_box_office)

quantile(Movies_DF$international_box_office)

#Percentiles
percentiles_worldwide_box_office <- IQR(Movies_DF$worldwide_box_office)
paste("Percentiles of worldwide box office :", percentiles_worldwide_box_office)

percentiles_domestic_box_office <- IQR(Movies_DF$domestic_box_office)
paste("Percentiles of domestic box office :", percentiles_domestic_box_office)

percentiles_international_box_office <- IQR(Movies_DF$international_box_office)
paste("Percentiles of international box office :", percentiles_international_box_office)

#Data Visualization
#Geom-point
library(ggplot2)
ggplot(data = Movies_DF, mapping = aes(x = year, y = worldwide_box_office)) + geom_point(color='blue', alpha = .7, size = 1.5)+geom_smooth(color="black",method =lm, se= FALSE)


#piechart
library(ggpie)
Movies_DF %>% ggpie(group_key = "year",count_type = "full", label_type = "circle",
               label_info = "ratio", label_pos = "out",label_size = 3,nudge_x = 20)


#Geom-bar
ggplot(Movies_DF,aes(x=year, fill=international_box_office))+
  geom_bar()+
  labs(title = "Contribution Of International Box Office", x ="Year", y="International Box Office")


Movies_DF %>% ggplot(aes(x= year,y= worldwide_box_office, fill=year))+
  geom_bar(stat = "identity")+
  labs(x="Year",y="Worldwide_box_office", title = "Year By Worldwide Box Office")


#Density
Movies_DF%>% ggplot(aes(x= year, y= domestic_box_office))+
  geom_density(stat = "identity", fill="red", bw= 1)+
  labs(x="Year",y="Domestic Box Office", title = "Year Vs Domestic Box Office")

#New Data Frame

# Create a new dataframe from a subset of the old dataframe "Movies_DF"
New_Movies_DF <- Movies_DF[, c("name", "year", "domestic_box_office","international_box_office",
                               "worldwide_box_office")]

# Change the column names of the new dataframe
colnames(New_Movies_DF) <- c("Name", "Year", "Domestic Box Office ($ Million)", "International Box Office ($ Million)", 
                             "Worldwide Box Office ($ Million)")



#Interactive dashboard
library(shiny)
library(DT)
library(ggplot2)

ui = fluidPage(
  titlePanel("Box Office Insights: Analyzing Worldwide Movie Revenue"),
  tabsetPanel(
    tabPanel("Table", div(dataTableOutput("table"), style="margin-top: 20px")),
    tabPanel("Statistics", 
             verbatimTextOutput("stats1"),
             verbatimTextOutput("stats2"),
             verbatimTextOutput("stats3"),
             verbatimTextOutput("stats4")),
    
    tabPanel("Graph", 
             fluidRow(
               column(3, 
                      wellPanel(
                        selectInput("x_var", "X Variable",
                                    choices = c("domestic_box_office", "international_box_office","worldwide_box_office","year"),
                                    selected = "year"
                        )
                      )
               ),
               column(3, 
                      wellPanel(
                        selectInput("y_var", "Y Variable",
                                    choices = c("domestic_box_office", "international_box_office","worldwide_box_office","year"),
                                    selected = "domestic_box_office"
                        )
                      )
               ),
               column(3, 
                      wellPanel(
                        selectInput("plot_type", "Select Plot Type",
                                    choices = c("Point", "Bar", "Density"),
                                    selected = "Point"
                        )
                      )
               )
             ),
             plotOutput("plot"))
  )
)


server = function(input, output) {
  output$table <- renderDataTable({
    datatable(New_Movies_DF, 
              options = list(pageLength = 15, 
                             lengthMenu = c(5, 10, 15, 20), 
                             searching = TRUE))
  })
  
  output$stats1 <- renderPrint({
    # Calculate the variance, standard deviation and range
    variance_domestic <- var(Movies_DF$domestic_box_office)
    sd_domestic <- sd(Movies_DF$domestic_box_office)
    range_domestic <- range(Movies_DF$domestic_box_office)
    
    variance_international <- var(Movies_DF$international_box_offic)
    sd_international <- sd(Movies_DF$international_box_offic)
    range_international <- range(Movies_DF$international_box_office)
    
    variance_worldwide <- var(Movies_DF$worldwide_box_office)
    sd_worldwide <- sd(Movies_DF$worldwide_box_office)
    range_worldwide <- range(Movies_DF$worldwide_box_office)
    
    # Print the results
    cat("Mode of year :",mode(Movies_DF$year), "\n\n")
    cat("Variance Domestic Box Office ($ Million): ", variance_domestic, "\n")
    cat("Standard Deviation Domestic Box Office ($ Million): ", sd_domestic, "\n")
    cat("Range: ", range_domestic[1], " - ", range_domestic[2], "\n")
    cat("\n")
    cat("Variance International Box Office ($ Million): ", variance_international, "\n")
    cat("Standard Deviation International Box Office ($ Million): ", sd_international, "\n")
    cat("Range: ", range_international[1], " - ", range_international[2], "\n")
    cat("\n")
    cat("Variance Worldwide Box Office ($ Million): ", variance_worldwide, "\n")
    cat("Standard Deviation Worldwide Box Office ($ Million): ", sd_worldwide, "\n")
    cat("Range: ", range_worldwide[1], " - ", range_worldwide[2], "\n")
    
  })
  
  output$stats2 <- renderPrint({
    cat("Summary of Domestic Box Office ($ Million)", "\n\n")
    summary(Movies_DF$domestic_box_office)
  })
  
  output$stats3 <- renderPrint({
    cat("Summary of International Box Office ($ Million)", "\n\n")
    summary(Movies_DF$international_box_office)
  })
  
  output$stats4 <- renderPrint({
    cat("Summary of Worldwide Box Office ($ Million)", "\n\n")
    summary(Movies_DF$worldwide_box_office)
  })
  
  output$plot <- renderPlot({
    
    # get user inputs
    x_col <- input$x_var
    y_col <- input$y_var
    plot_type <- input$plot_type
    
    # check if the input columns exist in the data frame
    if(!all(c(x_col, y_col) %in% colnames(Movies_DF))) {
      return(NULL)
    }
    
    # create plot based on selected plot type
    if(plot_type == "Point") {
      ggplot(Movies_DF, aes_string(x = x_col, y = y_col)) + 
        geom_point() + 
        labs(x = x_col, y = y_col)
    } else if (plot_type == "Bar") {
      ggplot(Movies_DF, aes_string(x = x_col, y = y_col, fill = factor(Movies_DF$year))) + 
        geom_bar(stat = "identity") + 
        labs(x = x_col, y = y_col, fill = "Year")
    } else if (plot_type == "Density") {
      ggplot(Movies_DF, aes_string(x = y_col)) +
        geom_density() +
        labs(x = y_col, y = "Density")
    }
  })
}

shinyApp(ui, server)