

#' line_distance Function
#'
#' Function to measure length
#' @param x vector of x coordinated
#' @param y vector of y coordinated
#' @export
#' @examples
#' #line_distance()
line_distance <- function(x,y) sum(sqrt(diff(x)^2+diff(y)^2))



#' extract_length Function
#'
#' Extract total length of plotted line in pixels
#' @param data 
#' @export
#' @examples
#' #extract_length()
extract_length <- function(data) do.call(rbind,c(lapply(data,function(w) do.call(rbind,lapply(split(w, w$item),function(z)data.frame(file=as.character(z$file[1]),item=as.character(z$item[1]),length=line_distance(z$x,z$y))))),make.row.names = FALSE)) 

#' get_outline Function
#'
#' Impute outline from plotted points. connects sequentially plotted points with straight line, then finds max and min for each pixel, to get outline
#' @param group_data 
get_outline <- function(group_data){
	mod <- lm(group_data[c(nrow(group_data),1),"y"]~group_data[c(nrow(group_data),1),"x"])
		xR <- (round(group_data[nrow(group_data),"x"]):round(group_data[1,"x"]))
		yR <- round(coef(mod)[1] + coef(mod)[2]*xR)
		new_data <- cbind(xR,yR)

		for(i in 2:nrow(group_data)){
			mod <- lm(group_data[(i-1):i,"y"]~group_data[(i-1):i,"x"])
			xR <- (round(group_data[(i-1),"x"]):round(group_data[i,"x"]))
			yR <- round(coef(mod)[1] + coef(mod)[2]*xR)
			new_data <- rbind(new_data,cbind(xR,yR))
		}
	return(new_data)
}

#' extract_length Function
#'
#' Extract total area inside plotted points in pixels. Uses get_outline to get the outline, then works out how many pixels are inside it, include the outline
#' @param data 
#' @export
#' @examples
#' #extract_length()
extract_area <- function(data){
	do.call(rbind,lapply(data,function(group_data) {
		new_data <- get_outline(group_data)
		#points(new_data[,1],new_data[,2], col="red")

		all_points<-NULL
		for(j in unique(new_data[,1]))	all_points <- rbind(all_points,cbind(j,range(new_data[new_data[,1]==j,2])[1]:range(new_data[new_data[,1]==j,2])[2]))
		area <- nrow(all_points)
		return(area)
	}))
}

###############
# - plot extracted area for a single figure 
##############

# plot_area <- function(id,data,image_folder){
# 	group_data <- data[[id]]
# 	image <- magick::image_read(paste0(image_folder,"/",group_data[1,1]))
# 	new_data<-get_outline(group_data)
# 	plot(image)
# 	polygon(new_data[,1],new_data[,2], col=alpha("red",0.3), border="red")
# }

