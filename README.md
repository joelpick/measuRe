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


After you've measured all photos:

*extract_length/extract_area(all_data)* to extract length/area for each photo measured in total number of pixels.


# Application

**Prev**: Previous image in folder;
**Next**: Next image in folder, when the image is the last in the list, the application will close;
**Add**: Allows for mesuring of more than one object per image;
**Zoom**: Zoom in on a part of image by clicking the Zoom button and then the top left and bottom right around the point of interest;
**Zoom Out**: Zooms the image out to the original size;
**Delete Item**: Allows for removal of one object;
**Delete All**: Removes all points from the image;
**Finish**: Closes the window and saves a R datafile with existing coordinates.
