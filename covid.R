library(maps)
library(ggplot2)
library(dplyr)

library(magrittr)
library(tidyverse)
library(rvest)

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)

covid = covid[,c(1,2,3,4,5,8,27,28,29,30,31,32,40)]
covid$month = format(as.Date(covid$date), "%m")
covid$year = format(as.Date(covid$date), "%Y")


covid_index_data <- as.data.frame(covid)

covid_index_data[is.na(covid_index_data)] <- 0

covid_total_cases = covid_index_data %>%
  mutate(month = format(as.Date(date), "%m"), year = format(as.Date(date), "%Y")) %>%
  group_by(month,year,location) %>%
  dplyr::summarise(value = sum(total_cases))

covid_total_deaths = covid_index_data %>%
  mutate(month = format(as.Date(date), "%m"), year = format(as.Date(date), "%Y")) %>%
  group_by(month,year,location) %>%
  dplyr::summarise(value = sum(total_deaths))

covid_index_data = covid_index_data %>% select(iso_code, continent,location,population,population_density,median_age,aged_65_older,aged_70_older,gdp_per_capita,life_expectancy,month,year) %>% distinct

covid['ISO3'] <- iso_codes$ISO3[match(covid$location, iso_codes$Country)]

world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]

df <-
  dplyr::left_join(covid_index_data,
                   covid_total_cases,
                   by = c("location" = "location","month" = "month","year" = "year"))

df1 <-
  dplyr::left_join(df, covid_total_deaths, by = c("location" = "location","month" = "month","year" = "year"))


df1=df1[,-c(4:10)]

df1 = df1 %>% 
  rename(
    total_cases = value.x,
    total_deaths = value.y
  )
library(reshape2)



df2 = melt(df1, id = c("iso_code", "continent", "location","month","year"), 
           variable.name = "Key", value.name = "Value")


df2 = df2 [-c(2918:2930)]


worldMaps <- function(df2, world_data , Key , year , month){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () {
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       legend.position = "right",
                       panel.border = element_blank(),
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  print(Key)
  print(year)
  print(month)
  # Select only the data that the user has selected to view
  plotdf <- df2[df2$month == month &  df2$year == year & df2$Key == Key,]
  plotdf <- plotdf[!is.na(plotdf$iso_code), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['Key'] <- rep(Key, nrow(world_data))
  world_data['year'] <- rep(year, nrow(world_data))
  world_data['month'] <- rep(month, nrow(world_data))
  world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$iso_code)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(Key == "total_cases",  "total_cases", "total_deaths"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5,"Blues"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = Key, color = Key, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}

install.packages('shiny')
install.packages('ggiraph')

library(shiny)
library(ggiraph)


# Define the UI
ui = fluidPage(
  
  # App title
  titlePanel("COVID data"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: Type of data
      selectInput(inputId = "Key",
                  label = "Choose the type of data you want to see:",
                  choices = list("total_cases" = "total_cases", "total_deaths" = "total_deaths")),
      
      # Second input (choices depend on the choice for the first input)
      uiOutput("secondSelection"),
      
      # Third input (choices depend on the choice for the first and second input)
      uiOutput("thirdSelection")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Hide errors
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      # Output: interactive world map
      girafeOutput("distPlot")
      
    )
  )
)

# Define the server
server = function(input, output) {
  
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(df2, world_data, input$Key,  input$year ,input$month )))
  })
  
  # Change the choices for the second selection on the basis of the input to the first selection
  output$secondSelection <- renderUI({
    lab <- ifelse(input$Key == "total_cases", "total_cases", "total_deaths")
    choice_second <- as.list(unique(df2$year[which(df2$Key == input$Key)]))
    selectInput(inputId = 'year', choices = choice_second,
                label = "Choose the year for which you want to see the data:")
  })
  
  # Change the choices for the third selection on the basis of the input to the first and second selections
  output$thirdSelection <- renderUI({
    lab <- ifelse(input$Key == "total_cases", "total_cases", "total_deaths")
    choice_third <- as.list(unique(df2$month[df2$Key == input$Key & df2$year == input$year]))
    selectInput(inputId = 'month', choices = choice_third,
                label = paste0("Choose the month for ", lab,"which you want to see the dat:"))
  })
}

# Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
shinyApp(ui = ui, server = server)

