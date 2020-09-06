#' image_details Function
#'
#' Plots image
#' @param image path to image
#' @param xlim subset of image to plot. Default plots all x
#' @param ylim subset of image to plot. Default plots all y
image_details <- function(image, xlim=NULL, ylim=NULL){

}


#' plot_image Function
#'
#' Plots image
#' @param image path to image
#' @param xlim subset of image to plot. Default plots all x
#' @param ylim subset of image to plot. Default plots all y
#' @examples
#' #plot_image()
plot_image <- function(image, xlim=NULL, ylim=NULL){
	image_details <- c(width = magick::image_info(image)["width"][[1]], height = magick::image_info(image)["height"][[1]])
		
	if(is.null(xlim)) xlim <- c(0,image_details[1])
	if(is.null(ylim)) ylim <- c(0,image_details[2])

	length_y <- diff(ylim)
	min_y <- ylim[1]
	min_y_new <- min_y + length_y * - 0.16
	ylim[1] <- min_y_new
	box_y <- c(min_y+ length_y * - 0.04,min_y_new,min_y_new, min_y+ length_y * - 0.04)
	
	background_y <- c(min_y+ length_y * - 0.2,min_y,min_y, min_y+ length_y * - 2)
	background_x <- rep(xlim+xlim*c(-0.1,0.1),each=2)


	length_x <- diff(xlim) / 16
	add_min_x <- xlim[1] + length_x*0.5
	add_max_x <- xlim[1] + length_x*2.1
	next_min_x <- xlim[1] + length_x*2.4
	next_max_x <- xlim[1] + length_x*4.1
	zoom_min_x <- xlim[1] + length_x*4.4
	zoom_max_x <- xlim[1] + length_x*6.1
	zoomOut_min_x <- xlim[1] + length_x*6.4
	zoomOut_max_x <- xlim[1] + length_x*8.1
	redo_min_x <- xlim[1] + length_x*8.4
	redo_max_x <- xlim[1] + length_x*10.1
	redoAll_min_x <- xlim[1] + length_x*10.4
	redoAll_max_x <- xlim[1] + length_x*12.1
	finish_min_x <- xlim[1] + length_x*12.4
	finish_max_x <- xlim[1] + length_x*14.1
	progress <- xlim[1] + length_x*15

	op <- graphics::par(mar=c(0,0,0,0), mfrow=c(1,1))
	#on.exit(graphics::par(op))

	plot(NA, xlim=xlim, ylim=ylim)
	plot(image,add=TRUE)


	return(list(box_y=box_y,
		length_x=length_x,
		add_min_x=add_min_x,
		add_max_x=add_max_x,
		next_min_x=next_min_x,
		next_max_x=next_max_x,
		zoom_min_x=zoom_min_x,
		zoom_max_x=zoom_max_x,
		zoomOut_min_x=zoomOut_min_x,
		zoomOut_max_x=zoomOut_max_x,
		redo_min_x=redo_min_x,
		redo_max_x=redo_max_x,
		redoAll_min_x=redoAll_min_x,
		redoAll_max_x=redoAll_max_x,
		finish_min_x=finish_min_x,
		finish_max_x=finish_max_x,
		background_y=background_y,
		background_x=background_x,
		progress=progress))
}

#' button_plot Function
#'
#' Plots buttons on image
#' @param min_x minimum x coord
#' @param max_x max x coord
#' @param y y cords of box
#' @param text text in box
#' @param back_col background colour
#' @param text_col text colour
button_plot <- function(min_x,max_x,y, text,back_col,  text_col){
	x <- c(min_x,min_x,max_x,max_x)
	graphics::polygon(x,y, col=back_col, border=TRUE,xpd=TRUE)
	text(mean(x), mean(y),text, col = text_col, font = 2, cex = 1.2)
}

#' plot_buttons Function
#'
#' Plots buttons on image
#' @param id image details
#' @param n number of image
plot_buttons <- function(id,n){
	# id <- image_dat 
	graphics::polygon(id$background_x,id$background_y, col="white", border=FALSE)

	button_plot(id$add_min_x,id$add_max_x,id$box_y,"Add", text_col = "white", back_col="gray4")
	button_plot(id$next_min_x,id$next_max_x,id$box_y,"Next", text_col = "white", back_col="gray24")
	button_plot(id$zoom_min_x,id$zoom_max_x,id$box_y,"Zoom", text_col = "white", back_col="coral4")
	button_plot(id$zoomOut_min_x,id$zoomOut_max_x,id$box_y,"Zoom\nOut", text_col = "white", back_col="coral1")
	button_plot(id$redo_min_x,id$redo_max_x,id$box_y,"Redo", text_col = "white", back_col="dodgerblue4")
	button_plot(id$redoAll_min_x,id$redoAll_max_x,id$box_y,"Redo\nAll", text_col = "white", back_col="dodgerblue1")

	button_plot(id$finish_min_x,id$finish_max_x,id$box_y,"Finish", text_col = "white", back_col="red4")

	text(id$progress, mean(id$box_y),n, col = "black", font = 2, cex = 1.2)

		
}

#' basic_plot Function
#'
#' plots image and buttons
#' @param image path to image
#' @param xlim subset of image to plot. Default plots all x
#' @param ylim subset of image to plot. Default plots all y
#' @param n number of image
#' @examples
#' #basic_plot()
basic_plot<-function(image, xlim=NULL, ylim=NULL,n){
		
		image_details<- plot_image(image, xlim=xlim, ylim=ylim)
		
		plot_buttons(image_details,n)
		
		return(image_details)
}

# zoom_coords <- function(){

# image_dat <- basic_plot(image)
# 	corners<-locator(2)
# 	polygon(rep(corners$x,each=2),c(corners$y,corners$y[2:1]))
# }
#TeachingDemos::zoomplot(locator(1))
