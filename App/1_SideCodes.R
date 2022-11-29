library(jsonlite)
library(dplyr)
library(threejs)
library(igraph)

# globejs 
airports <- fromJSON("https://raw.githubusercontent.com/jbrooksuk/JSON-Airports/master/airports.json")

# create a dataframe key of size to colors
sizecolors <-
  data_frame(size = c("small","medium","large"),
             color = c("lightblue", "gold","firebrick"))

airports <- airports %>%
  # some of the data is missing lat and lon
  filter(!is.na(lat) & !is.na(lon)) %>%
  # join with colors
  left_join(sizecolors)

globejs(lat = airports$lat, 
        lon = airports$lon,
        color = airports$color)


library(maps)
data(world.cities, package="maps")
cities <- world.cities[order(world.cities$pop, decreasing=TRUE)[1:1000],]
value  <- 100 * cities$pop / max(cities$pop)
col <- colorRampPalette(c("cyan", "lightgreen"))(10)[floor(10 * value/100) + 1]
globejs(lat=cities$lat, long=cities$long, value=value, color=col, atmosphere=TRUE)

library(maptools)
library(threejs)
data(wrld_simpl)

bgcolor <- "#ffffff"
earth <- tempfile(fileext=".jpg")

# NOTE: Use antialiasing to smooth border boundary lines. But! Set the jpeg
# background color to the globe background color to avoid a visible aliasing
# effect at the the plot edges.

jpeg("C:\\Users\\COWBOYBEBOP\\Downloads\\earth.jpg", width=2048, height=1024, quality=1200, bg=bgcolor, antialias="default")
par(mar = c(0,0,0,0), pin = c(4,2), pty = "m",  xaxs = "i",
    xaxt = "n",       xpd = FALSE,  yaxs = "i", bty = "n", yaxt = "n")
plot(wrld_simpl, col="red", bg=bgcolor, border="gray", ann=FALSE,
     setParUsrBB=TRUE)
dev.off()
globejs(earth,height = "100px",width = "100px")

# A shiny example:
shiny::runApp(system.file("examples/globe",package="threejs"))




library("threejs")
earth <- "C:\\Users\\COWBOYBEBOP\\Downloads\\earth.jpg"
globejs(img=earth, bg="white")


library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))





