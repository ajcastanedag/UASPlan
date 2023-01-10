DJI-M300 & Micasense Altum
=======================
---
**Disclamer**

In this document you will find all the steps to operate the DJI-M300 in combination with the Micasense Altum camera. When a word is highlighted, it contains an additional explanation when hovering the mouse on top of it, for example terms such as <div title="Downwelling Light Sensor 2"><span style="color:rgb(253,163,126)">DLS 2</span></div>. Additionally, at the end of this document you can download a field version that contains all the summariced steps on the equipment operation.

**⚠ You must read through all this document before proceeding any further with the equipment you will use, its zour responsibilitz to have clearence on how to operate the 
equipment and how to act in case of emergencies ⚠**

---
**Aircraft set-up**


---
**Camera set-up**
* Mount Camera on Sky port by aligning the the longest flap 
* Mount DLS 2 sensor on the aircraft
* Connect DLS 2 to skyport 
* Connect blue cable (altum / skyport)
* Insert memory (USB)
* Turn aircraft on and wait until the camera has finished initializing (around 2 min). 
* Once the DLS sensor is blinking green, you can proceed.
* With a phone or laptop, login into the camere wifi:
	* Name: Altum_ALO5-1934088-SC
	* Password: micasense
* Open any browser of your choice and type: **192.168.10.254**
* This will take you to the main camera menu called "HOME", from it you can visualize the status of the camera:
	* <div title="This will report your available and maximum storage space, as well as the file system (e.g. FAT32).">Storage</div>
	* <div title="Indicates the amount of satellites that are detected and currently being used. There is also an area at the bottom of the page that shows the signal strength of detected satellites.">GPS Sats</div>
	* <div title="Will be green if DLS is powered and communicating with the camera.">DLS Status</div>
	* <div title="Will initially be set at UNIX time (1970-01-01 00:00:00 UTC). Once GPS has a lock, this will update to the current date/time in UTC (Universal Time). It cannot be changed to a different time zone, as many data processing programs rely on UTC for accurate calculations.">Time</div>
	* <div title="This is the detected location coming from GPS, written in Decimal Degrees. ">Location</div>
	* <div title="This indicates your current altitude in Mean Seal Level (MSL) and Above Ground Level (AGL). The altitude AGL reported on the status page is an estimate based on the initial GPS lock received by the camera. If the initial GPS lock is not very accurate, it's possible for the AGL reported to be significantly off, especially as a better GPS lock is established over time. Powering the camera off and then back on will reset the ground estimate.">Altitude</div>
	* <div title="Current speed detected in meters per second. These unit of measurement cannot be converted at this time.">Speed</div>
	* <div title="Your current magnetic heading. If properly calibrated, you can check the reading against a handheld compass to verify its accuracy. We recommend re-calibrating the compass regularly, but at the very least when it has been moved to another location on the aircraft, or if you are flying in a different time zone (which would slightly alter the magnetic heading).">Heading</div>
	* <div title="This shows your current setting for your Automatic Triggering Mode. ">Capture Config</div>

**⚠ You must verify that all items are <span style="color:rgb(26,175,79)">green</span> colored to proceed.**


* Access the configuration tab and under the Basic Configuration section select the following parameters:
	* Auto-Capture Mode: Overlap
	* Along-Track Overlap: 80%
	* Target Alt: 80m (This may depend on your selected flight altitud, leave this parameter as 10 meters less than your chosen flight altitud).

* Locate the calibration panel in a clear (no shadeded) flat area.
* Stand in front of the panel with the sun to your back holding the aircraft with the camera in front of you.
* Take one large step to the right or left to avoid any shade on the panel. 
* Access the live-view tab, tick the "Streaming" option and then click on the QR button. 	 
* The camera will start beeping trying to find the QR code of the panel you located previously on the ground and it will take a calibration image. The sucess of this operation will be indicated by a high pitch sound from the camrera.
* 

---
**Flight**
* Deploy the aircraft using the Pilot 2 App

---
**Post-Flight**
* Once the aircraft has landed, access the browser interface of the camera once more and take a second calibration image. (Repeat previous steps)


Disconnect data cable (Blue) bz pressing on both sides
Disconnect data cable from sky port by pressing on the flap and gently pulling
safe the cable on the box 
unpluy usb and safe it on the box
unplug DLS 2 

---
**References**
For aditional information you can consult:
* [Altum user guide](https://support.micasense.com/hc/en-us/articles/360039671254-User-Guide-for-MicaSense-Sensors)
* [Altum integration guide](https://support.micasense.com/hc/article_attachments/360073921173/AltumDLS2IntegrationGuideRev10.pdf)






