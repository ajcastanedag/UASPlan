```{=html}
<style>
p {
  style="text-align: justify;
}
</style>
```
# Introduction

Important for M300, Wingtra, M600 and if you want to increase your accuracy with Phantom 4

### Equipment

  *Emlid station 
  *Tripod
  *Tripod head (the head is stored separately to avoid damage to the head)
  * panels (take care to store them separately in the carbord to avoid damage on the reflective side of the panels)
  * UAS phone with app (Emlid Flow) 
  * measurement 
  * monopol (optional, if you are taking GSP)
  
    if you are not taking the UAS phone you need to:
      *preinstall the app Emlid Flow, 
      *open app
      *turn on emlid station
      *establish a WiFi connection with you phone to the Emlid WiFi network (reach: 05:74 / password: emlidreach)
      *go back to your app - tap update in your app if station doesn't appear in the menu 
      now you are ready for the field 

  
### Emlid station setup for RTK (for Lidar missions)

 setup Emlid station and check connection and measure height:
 
  - steps:
  
      * set up tripod on a clear space (within 60 km radius from you AOI)
      * put tripod head on tripod
      * while setting up take care of the level (bubble on the head)
      * measure height of your base station (from ground to bottom of the Emlid base station)
      * turn on emlid
      * open app Emlid Flow
      * connect your phone to the WiFi connection of the Emlid station
      * check app if connection is established (WiFi Network "reach 192.168.42.1" appears on your app) - if not press update in the app until it is
      * click on "reach 192.168.42.1" and a drop down menu will appear
      * click on "Status"
      
  #-----
     
      * check that the coordinate precision is desirable (should be in a rage of 2m - WITHIN BAVARIA ONLY, if correction time exceeds 5 min change network. Navigate back to main menu, tap "correction input", select XXXX a new network, wait for message change - waiting correction to recieving correction (turning from orange to green, if there is no connection, try the procedure with the next network )
      * coordinate correction is disabled outside Bavaria and precision range should be 5m or below
      * if your range is too large, change position of the base station and start again
  
  #-------
  
  * now that you established a connection with a reasonable correction you need to wait until the message in the upper right corner of the main menu of the app is turning from single to fixed (red to green, a beep will occur). This will happen when enough GPS satellite connections are established and can take up to 15 min.
  * if the message is "float" (yellow), the precision is almost achieved, be patient! :)

please note: the station is recording as soon as it is turned on, but the precision is only reliable after reaching fixed status (after the beep) and only then the flight mission should start

(note to add to the Post flight list: stop RTK Logging via app and download - 3 files are needed)


  
### Ground Sample Points (GSP)

to achieve ....

 #  setup - before your flight mission!
 
 the panels should be distributed evenly thorugh the area of interest that is covered by your mission
the panels should be placed in an open space (need to be visible in the images!)

# get position of your GSP 
 after the flight (also possible before or during the flight) you need to record the coordinates of your GSP
 therefore you need to:
 - mount the Emlid on the monopol
 - go to your panel
 - place the Emlid with monopol on the center of your panel
 - 
 
 


