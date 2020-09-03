
#library(scales)




#' measuRe Function
#'
#' This function allows to measure objects on images in pixels
#' @param image_folder Directory path with images inside. Should only contain images to 
#' @param x11 Logical. use x11? defaults to FALSE
#' @export
#' @examples
#' #measuRe()
measuRe <- function(image_folder, x11=FALSE){
	#	image_folder="~/Dropbox/measuRe/test"
	image_names <- list.files(image_folder)
	all_data <- list()
	add_removeQ="a"

	if(x11) x11(type = "cairo", bg = "black", width = 15, height = 15)


	for(i in 1:length(image_names)){
	#i=1
		if(add_removeQ!="f"){ ## if not finished
			image <- magick::image_read(paste0(image_folder,"/",image_names[i]))
			
			image_dat <- basic_plot(image)
			group_data <- data.frame()
				add_removeQ="a"
				while(add_removeQ=="a"){
						select_points <- graphics::locator(1)
					
					##next
					if( select_points$x<image_dat$next_max_x & select_points$y<max(image_dat$box_y) & select_points$x>image_dat$next_min_x & select_points$y>min(image_dat$box_y)) {
						add_removeQ <- "b"
						##save data
						all_data[[image_names[i]]] <- group_data
					}
					##zoom
					else if(select_points$x<image_dat$zoom_max_x & select_points$y<max(image_dat$box_y) & select_points$x>image_dat$zoom_min_x & select_points$y>min(image_dat$box_y)){
						##deleted data and replot
						graphics::polygon(image_dat$background_x,image_dat$background_y, col="white", border=FALSE)
						text(mean(image_dat$background_x),mean(image_dat$box_y),"Click on top left hand corner and bottom right hand corner of where you want to zoom in on")
						corners <- locator(2)
						image_dat <- basic_plot(image,xlim=sort(corners$x),ylim=sort(corners$y))
						lines(group_data$x,group_data$y, type="o", col="red", pch=19, cex=1)
					}					
					##redo
					else if(select_points$x<image_dat$redo_max_x & select_points$y<max(image_dat$box_y) & select_points$x>image_dat$redo_min_x & select_points$y>min(image_dat$box_y)){
						##deleted data and replot
						group_data <- data.frame()
						image_dat <- basic_plot(image)
					}
					##finish
					else if(select_points$x<image_dat$finish_max_x & select_points$y<max(image_dat$box_y) & select_points$x>image_dat$finish_min_x & select_points$y>min(image_dat$box_y)){
						add_removeQ <- "f"
						## only save data if some points have been clicked
						if(nrow(group_data)>0) all_data[[image_names[i]]] <- group_data
					}
					##if click on white box at bottom do nothing
					else if(select_points$x>min(image_dat$background_x) & select_points$y<max(image_dat$background_y)){
						add_removeQ="a"
					}
					else{ 
						points(select_points$x,select_points$y, col="red", pch=20, cex=1)
						if(nrow(group_data)>0) lines(c(select_points$x,group_data$x[nrow(group_data)]),c(select_points$y,group_data$y[nrow(group_data)]), col="red", lwd=2)	
						group_data <- rbind(group_data, data.frame(file=image_names[i], x=select_points$x, y=select_points$y) )
		
					}
				}
		}

	}
	dev.off()
	return(all_data)
}
