# measuRe v0.0.0.9000

Measure length and area from images in R

<img src= "https://user-images.githubusercontent.com/37153494/92573295-7b331300-f27d-11ea-82e8-1ccd15116f35.jpg" width="500" height="500" />


# Installation

devtools::install_github("joelpick/measuRe")

library(measuRe)


# Usage

In order to bring up the UI you need to run the "measuRe" function:

all_data <- *measuRe("folder path where images are located")*

The application runs best in standalone R, though if you add x11 = T it'll run fine in R Studio.


After you've measured all images:

*extract_length/extract_area(all_data)* to extract length/area for each object within an image measured in total number of pixels.


# Application

**Prev**: Previous image in folder.

**Next**: Next image in folder. When the image is the last in the list, the application will close.

**Add**: Allows for measuring of more than one object per image.

**Zoom**: Zoom in on a part of image by clicking the Zoom button and then the top left and bottom right around the point of interest.

**Zoom Out**: Return the image to the original size pre-zoom.

**Delete Item**: Allows for removal of one object.

**Delete All**: Removes all points from the image.

**Finish**: Closes the window and saves a R datafile with existing coordinates.

The number in the bottom right denotes which image you are currently working on/total number of images within the folder path.


<img src="https://user-images.githubusercontent.com/37153494/93581817-8e3e9500-f999-11ea-9c24-d5707808cf53.png" width="1000" height="700" />>
