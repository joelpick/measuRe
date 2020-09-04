
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
		min_y_new <- min_y + length_y * - 0.15
		ylim[1] <- min_y_new
		box_y <- c(min_y+ length_y * - 0.05,min_y_new,min_y_new, min_y+ length_y * - 0.05)
		
		background_y <- c(min_y+ length_y * - 0.2,min_y,min_y, min_y+ length_y * - 2)
		background_x <- rep(xlim+xlim*c(-0.1,0.1),each=2)


		length_x <- diff(xlim) / 15
		next_min_x <- xlim[1] + length_x
		next_max_x <- xlim[1] + length_x*3
		zoom_min_x <- xlim[1] + length_x*4
		zoom_max_x <- xlim[1] + length_x*6
		redo_min_x <- xlim[1] + length_x*7
		redo_max_x <- xlim[1] + length_x*9
		finish_min_x <- xlim[1] + length_x*10
		finish_max_x <- xlim[1] + length_x*12
		progress <- xlim[1] + length_x*14

		op <- graphics::par(mar=c(0,0,0,0), mfrow=c(1,1))
		#on.exit(graphics::par(op))

		plot(NA, xlim=xlim, ylim=ylim)
		plot(image,add=TRUE)


		return(list(box_y=box_y,
			length_x=length_x,
			next_min_x=next_min_x,
			next_max_x=next_max_x,
			zoom_min_x=zoom_min_x,
			zoom_max_x=zoom_max_x,
			redo_min_x=redo_min_x,
			redo_max_x=redo_max_x,
			finish_min_x=finish_min_x,
			finish_max_x=finish_max_x,
			background_y=background_y,
			background_x=background_x,
			progress=progress))
}

#' plot_buttons Function
#'
#' Plots buttons on image
#' @param id image details
#' @param n number of image
plot_buttons <- function(id,n){
		next_x <- c(id$next_min_x,id$next_min_x,id$next_max_x,id$next_max_x)
		zoom_x <- c(id$zoom_min_x,id$zoom_min_x,id$zoom_max_x,id$zoom_max_x)
		redo_x <- c(id$redo_min_x,id$redo_min_x,id$redo_max_x,id$redo_max_x)
		finish_x <- c(id$finish_min_x,id$finish_min_x,id$finish_max_x,id$finish_max_x)
		
		graphics::polygon(id$background_x,id$background_y, col="white", border=FALSE)

		graphics::polygon(next_x,id$box_y, col="black", border=TRUE,xpd=TRUE)
		graphics::polygon(zoom_x,id$box_y, col="coral2", border=TRUE,xpd=TRUE)
		graphics::polygon(redo_x,id$box_y, col="cornflowerblue", border=TRUE,xpd=TRUE)
		graphics::polygon(finish_x,id$box_y, col="red4", border=TRUE,xpd=TRUE)

		text(mean(c(id$next_min_x,id$next_max_x)), mean(id$box_y),"Next", col = "white", font = 2, cex = 1.2)
		text(mean(c(id$zoom_min_x,id$zoom_max_x)), mean(id$box_y),"Zoom", col = "white", font = 2, cex = 1.2)
		text(mean(c(id$redo_min_x,id$redo_max_x)), mean(id$box_y),"Redo", col = "white", font = 2, cex = 1.2)
		text(mean(c(id$finish_min_x,id$finish_max_x)), mean(id$box_y),"Finish", col = "white", font = 2, cex = 1.2)
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
