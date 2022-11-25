<style>body {text-align: left}</style>
<style>p {text-align: justify;}</style>

Phantom 4
=======================
In this document you will find all the proceadures to... 


Required Equipment
=======================
The required equipment to map with the P4 drone are listed as follows;
* DJI Phantom 4 drone with all accessories
* Smartphone or tablet with DJI GO4 app support
* Pix4D capture, DJI GO4, Any supported flight planning application


Flight Workflow
=======================
The checklist below is a summary of the workflow

Plan the mapping mission using Pix4D Capture.

<input type="checkbox" unchecked> Make sure the Base map is cached as part of the mission. </input>

<input type="checkbox" unchecked> Ensure the Drone batteries as well as the RC battery and Smartphone are fully charged. </input>

<input type="checkbox" unchecked> lay out a landing mat or search for a flat surface to set up the drone on. </input>

<input type="checkbox" unchecked> Install propellers and battery on the drone. </input>

<input type="checkbox" unchecked> Check for any installed storage device. </input>

<input type="checkbox" unchecked> Clamp the smartphone with the holder on the RC. </input>

<input type="checkbox" unchecked> Connect the Smartphone to the RC with the appropriate USB cable. </input>

<input type="checkbox" unchecked> Turn on Remote controller and afterwards the drone. </input>

<input type="checkbox" unchecked> Open your planned mission in Pix4D Capture. </input>

<input type="checkbox" unchecked> Check for a working connection between drone and RC. </input>

<input type="checkbox" unchecked> Verify that the Flight path will not be obstructed by any tall feature. </input>

<input type="checkbox" unchecked> Flight hold mode should be in Position mode (P-Mode). </input>

<input type="checkbox" unchecked> Deploy drone to make a mapping mission. </input>

<input type="checkbox" unchecked> After mission Drone lands automatically, please pay attention to landing.</input>

<input type="checkbox" unchecked> Turnoff the drone and the RC. </input>

<input type="checkbox" unchecked> Box the drone and RC. </input>

Flight mission is complete.


Data Download 
=======================

Data Processing
=======================

<input type="checkbox" unchecked> Login into the computer (name: lsfegast-05 login: fkt12rk pw: EaQ%RMqNCR94) </input>

<input type="checkbox" unchecked> Open the UAS platform, and load your project. </input>

<input type="checkbox" unchecked> On the Log section, add as much information as possible on the flight you want to process.
Add updated information such as: main pilot, copilot, time of flights, objective of the flight, etc.</input>

<input type="checkbox" unchecked> Copy the images from the Phantom memory card into 0_Images or alternitvelly use the App wizzard to copy the images.</input>

<input type="checkbox" unchecked> Open  Agisoft Metashape software and go to File -> Save As... and save your empty project 
   inside the folder 1_Agisoft </input>

<input type="checkbox" unchecked> You can import now the images in two ways: one is drag and drop the images into Agisoft or you 
   can go to Worflow -> Add Floder... then select your 0_Images folder, click in select folder and then on the window that opens select single cameras and then ok.</input>

You can automate the following steps accessing to Workflow -> Batch Process and add 
all the following steps by their name. If you do, please add each job and finish clicking ok and 
then Add... to add the foloowing step. Before clicking OK in the main window, make sure you enable 
the option "Save project after each step".

<input type="checkbox" unchecked> Go to Workflow -> Align Photos, select High accuracy, enable Generic preselection and reference 
   preselection and then OK.  </input>

<input type="checkbox" unchecked> Once its done processing, go to Workflow -> Build Dense Cloud... and select the quality you 
   want. Generally, high is enough. Then click ok. </input>

<input type="checkbox" unchecked> Now you can generate the DSM by going to Workflow -> Build DEM... and make sure the source data 
is set as Dense Cloud</input>

<input type="checkbox" unchecked> To generate the Orthomosaic, go to Workflow -> Build Orthomosaic... make sure the Surface 
option is set to DEM</input>

<input type="checkbox" unchecked> Export all the products (Points - DEM - Orthomosaic) by clicking on File -> Export -> and the 
product you want to export. Here you must use the \2_Results\3_Raster\ folder for the raster files
and \2_Results\2_PointCloud\ to store the dense cloud as las file. For the point cloud is important
to use a projected system and NOT EPSG:4326.</input>

<input type="checkbox" unchecked> Generate report by going to File -> Export -> Generate Report. There add a Title, the name of the Pilot and
copilot and click OK. Save the report naming it "Report" under 0_Reports folder. When it finishes, it will automatically
open and you will be able to check all your project.  </input>

<input type="checkbox" unchecked> Open a new QGis project and save it \2_Results\1_Qgis\. This project is made to generate quick maps afterwards so add here
all your products and create a basic layout. </input>



