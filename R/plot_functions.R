#' image_details Function
#'
#' Plots image
#' @param image path to image
#' @param xlim subset of image to plot. Default plots all x
#' @param ylim subset of image to plot. Default plots all y
#' @param buttons vector of button names
image_details <- function(image, xlim=NULL, ylim=NULL, buttons){
	if(!is.null(image)) image_details <- c(width = magick::image_info(image)["width"][[1]], height = magick::image_info(image)["height"][[1]])
	if(is.null(xlim)) xlim <- c(0,image_details[1])
	if(is.null(ylim)) ylim <- c(0,image_details[2])
	
	length_y <- diff(ylim)
	box_y <- c(
		min=ylim[1] + length_y * -0.16, 
		mid=ylim[1] + length_y * -0.1, 
		max=ylim[1] + length_y * -0.04)
	background_y <- c(
		min=ylim[1] + length_y * -0.25,
		mid=ylim[1] + length_y * -0.1,
		max=ylim[1])
	background_x <- c(
		min=xlim[1] + diff(xlim) * -0.1,
		mid=mean(xlim),
		max=xlim[2] + diff(xlim) * 0.1)

	length_x <- diff(xlim) / (length(buttons)+1)

	button_pos <- list()
	for(i in seq_along(buttons)) {
		mid_x <- xlim[1] + length_x*(i-0.5)
		min_x <- mid_x - length_x*0.4
		max_x <- mid_x + length_x*0.4
		button_pos[[buttons[i]]] <- c(min=min_x, mid=mid_x, max=max_x)
	}

	progress <- xlim[1] + length_x*(length(buttons)+0.5)

	return(list(
		xlim=xlim,
		ylim=ylim,
		box_y=box_y,
		buttons_x=button_pos,
		progress=progress,
		background_y=background_y,
		background_x=background_x))
}


#' button_plot Function
#'
#' Plots buttons on image
#' @param x min and max x coords
#' @param y min and max y coords
#' @param text text in box
#' @param back_col background colour
#' @param text_col text colour
button_plot <- function(x, y, text, back_col, text_col,border=TRUE,text_cex=1.2){
	button_y <- c(y[1],y[3],y[3],y[1])
	button_x <- c(x[1],x[1],x[3],x[3])
	graphics::polygon(button_x,button_y, col=back_col, border=border,xpd=TRUE)
	text(x[2], y[2],text, col = text_col, font = 2, cex = text_cex)
}



#' plot_image Function
#'
#' Plots image
#' @param image path to image
#' @param id image details from image_details()
#' @examples
#' #plot_image()
plot_image <- function(image, id){
	
	op <- graphics::par(mar=c(0,0,0,0), mfrow=c(1,1))
	#on.exit(graphics::par(op))
	id$ylim[1] <- id$box_y["min"]
	plot(NA, xlim=id$xlim, ylim=id$ylim)
	if(!is.null(image)) plot(image,add=TRUE)
}


#' plot_buttons Function
#'
#' Plots buttons on image
#' @param id image details
#' @param n number of image
#' @param button_cols button colours
plot_buttons <- function(id, n, button_cols){
	# white background at the bottom
	button_plot(id$background_x,id$background_y,"", text_col = "white", back_col="white", border=FALSE)

	# plot all buttons
	for(i in seq_along(id$buttons_x))  button_plot(id$buttons_x[[i]],id$box_y,names(id$buttons_x[i]), text_col = "white", back_col=button_cols[i])

	## plot progress
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
basic_plot<-function(image, xlim=NULL, ylim=NULL,buttons, button_cols,n){

	image_details<- image_details(image, xlim=xlim, ylim=ylim, buttons)
	
	plot_image(image, image_details)
			
	plot_buttons(image_details,n,button_cols=button_cols)
	
	return(image_details)
}


#' insideFunc Function
#'
#' Are coords within button borders?
#' @param coords coords to check
#' @param x x box coords
#' @param y y box coords
insideFunc<-function(coords,x,y){
	coords$x<max(x) & 
	coords$y<max(y) & 
	coords$x>min(x) & 
	coords$y>min(y)
}

#' insideFunc Function
#'
#' Are coords within button borders?
#' @param coords coords to check
#' @param id Image details
clickPosition <- function(coords, id){
	out <- names(which(c(image= insideFunc(coords,id$xlim, id$ylim),sapply(id$buttons,function(z)insideFunc(coords,z, id$box_y)))))
	if(length(out)==0) out<-"background"
	return(out)
}


#' plot_points Function
#'
#' Plot previously extracted points
#' @param group_data data to plot from
# plot_points <- function(group_data, col="red"){
# 	for(k in unique(group_data$item)) lines(y~x,group_data[group_data$item==k,],type="o", col=col, pch=19, cex=1)
# }
plot_points <- function(group_data, current_item, current_col="red", done_col="blue"){
	for(k in unique(group_data$item)){
		col <- if(k==current_item){ current_col }else{ done_col }
		lines(y~x,group_data[group_data$item==k,],type="o", col=col, pch=19, cex=1)
	}
}

# zoom_coords <- function(){

# image_dat <- basic_plot(image)
# 	corners<-locator(2)
# 	polygon(rep(corners$x,each=2),c(corners$y,corners$y[2:1]))
# }
#TeachingDemos::zoomplot(locator(1))
