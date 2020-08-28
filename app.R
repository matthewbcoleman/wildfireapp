#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("USAboundaries", repos = "http://packages.ropensci.org", type = "source")
#install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")

#devtools::install_github("ropensci/USAboundaries")
#devtools::install_github("ropensci/USAboundariesData")

library(shiny)
#library(USAboundaries)
#library(USAboundariesData)
library(getlandsat)
library(ggthemes)
library(units)
library(plotly)
library(sf)
library(tidyverse)
library(rsconnect)
library(shinycssloaders)
library(shinycustomloader)

point_in_polygon = function(points, polygon, id){
    st_join(points, polygon) %>%
        st_drop_geometry() %>%
        count(.data[[id]]) %>%
        setNames(c(id, "n")) %>%
        left_join(polygon, by = id) %>%
        st_as_sf()
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    
    
    div(titlePanel("Daily California Wildfire Map"), align = 'center'),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            div(h4('(May take a couple moments to load)'), align = 'center'),
            
            #%>% withSpinner(type = 6)
                  
            div({plotlyOutput("mapPlot", height = '1000px', width = '1000px')  %>% withLoader(type = 'html', loader = 'loader6')}, 
                align = 'center'),
            width  = 12
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ######============================READ IN DATA============================######
    
    temp <- tempfile()
    url <- 'https://firms.modaps.eosdis.nasa.gov/data/active_fire/suomi-npp-viirs-c2/shapes/zips/SUOMI_VIIRS_C2_Global_24h.zip'
    download.file(url,temp)
    unzip(temp,exdir = 'data')
    unlink(temp)
    
    counties_ca <- read_sf('data/cacounties.shp')
    data <- read_sf('data/SUOMI_VIIRS_C2_Global_24h.shp', crs = 4326) %>% st_transform(5070)
    
    
    ######============================DATA MANIPULATION============================######
    # counties_raw <- USAboundaries::us_counties(resolution = 'low') %>%  
    #     st_transform(crs = 4326) %>% st_transform(5070)
    
    # counties_ca <- counties_raw %>% filter(state_name == 'California') #%>% st_combine() 
    # 
    california <- counties_ca %>% st_combine() 
    
    ca_fires <- st_filter(data, california, .predicate = st_within) %>% 
        st_as_sf() %>% 
        mutate(area_km = (187.5^2)/1000000)
    
    county_indices <- st_within(ca_fires, counties_ca)
    
    ca_fires <- ca_fires %>% mutate(county = counties_ca$name[unlist(county_indices)]) %>% 
        st_buffer(187.5)
    
    fire_counts <- point_in_polygon(ca_fires, counties_ca, 'contyfp')
    
    ######============================PLOTTING============================######
    
    output$mapPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        g <- ggplot() +
            geom_sf(data = california, col = 'snow3') +
            geom_sf(data = fire_counts, aes(fill = n, text = { paste0('# of Fire Pixels: ', n, '\n',
                                                                      'County: ', name)}),
                    alpha = .9) +
            geom_sf(data = ca_fires, col = 'red4', alpha = .5, size = .5) +
            scale_fill_gradient(low = 'grey', high = "orange", name ='Fire Pixel \nCount' ) +
            theme_linedraw() +
            labs(title = paste0('Location of Fires in California on: ', format(Sys.time()-7*60*60, format = '%b %d, %Y')),
                 subtitle = 'Highlighted counties have at least one active fire')
        
        ggplotly(g, tooltip = 'text')
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
