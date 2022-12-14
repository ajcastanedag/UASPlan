pacman::p_load(hexView,raster, ijtiff,exifr)


### dir where DJI Thermal SDK is located (select appropriate release according to your OS).
sdk_dir = "D:/0_Document/dji_thermal_sdk_v1.3_20220517/"
setwd(sdk_dir) # convinient way to set SDK paths across OS systems (path compatibility)

# short version for running commands in terminal
run<-function(x) shell(x, intern=FALSE, wait=TRUE)

### acquisitions / environmental properties:
emissivity = 0.96 # default: 1.0 range: 0.1-1.0 https://royalsocietypublishing.org/doi/pdf/10.1098/rsos.181281 (e.g., average 0.957 for vegetation)
humidity = 78 # default: 70 %  range: 20-100 %
distance = 25 # default: 5 m   range: 1-25 m    # altitude - object height (set to 25 if camera-target-distance > 25 m)


###  dir where your raw *.rpeg thermal images are placed. A output directory will be placed inside this folder.
in_dir <- "D:\\1_Projects\\NewUni\H20T\\Images\\T" # rf 29 %

out_dir = "D:\\1_Projects\\NewUni\\H20T\\Images\\TCal\\"
dir.create(out_dir)

in_files = list.files(in_dir, full.names = T, pattern = "_T")


### calibration/conversion procedure (...could be paralleled)
for(i in 1:length(in_files)){
  
  # calibration to celsius
  in_exif = exifr::read_exif(in_files[i])
  in_name = in_files[i]
  out_name = paste0(out_dir, substr(basename(in_files[i]), 0, nchar(basename(in_files[i]))-4), ".raw")
  run(paste0("D:/0_Document/dji_thermal_sdk_v1.3_20220517/utility/bin/windows/release_x64/dji_irp.exe -s ", in_name, " -a measure -o ", out_name, " --measurefmt float32", 
             " --emissivity ", emissivity, " --humidity ", humidity, " --distance ", distance))
  
  # from .raw (hex) to .tif (celsius in float)
  raw_data <- readBin(out_name, "double", size = 4, n = in_exif$ImageWidth*in_exif$ImageHeight)
  image_matrix <- matrix(raw_data, nrow = in_exif$ImageHeight, ncol = in_exif$ImageWidth, byrow = T)
  out_name_tif = paste0(substr(out_name, 0, nchar(out_name)-4), ".tif")
  write_tif(image_matrix, path = out_name_tif, overwrite = TRUE)
  
  # transfer metadata (exif)
  exiftool_call(paste0("-Model=", in_exif$Model[1]), out_name_tif)
  exiftool_call(paste0("-Make=", in_exif$Make[1]), out_name_tif)
  exiftool_call(paste0("-Orientation=", in_exif$Orientation[1]), out_name_tif)
  exiftool_call(paste0("-FocalLength=", in_exif$FocalLength[1]), out_name_tif)
  exiftool_call(paste0("-FocalLengthIn35mmFormat=", in_exif$FocalLengthIn35mmFormat[1]), out_name_tif)
  exiftool_call(paste0("-DigitalZoomRatio=", in_exif$DigitalZoomRatio[1]), out_name_tif)
  exiftool_call(paste0("-ApertureValue=", in_exif$ApertureValue[1]), out_name_tif)
  exiftool_call(paste0("-GPSAltitude=", in_exif$GPSAltitude[1]), out_name_tif)
  exiftool_call(paste0("-GPSLatitude=", in_exif$GPSLatitude[1]), out_name_tif)
  exiftool_call(paste0("-GPSLongitude=", in_exif$GPSLongitude[1]), out_name_tif)
}

### remove temp files
file.remove(list.files(out_dir, recursive = TRUE, full.names = T, pattern = "_original"))
file.remove(list.files(out_dir, recursive = TRUE, full.names = T, pattern = "_T.raw"))
