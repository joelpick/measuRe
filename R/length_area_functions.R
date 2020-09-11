

#' line_distance Function
#'
#' Function to measure length
#' @param x vector of x coordinated
#' @param y vector of y coordinated
#' @export
#' @examples
#' #line_distance()
line_distance <- function(x,y) sum(sqrt(diff(x)^2+diff(y)^2))



#' extract_length_image Function
#'
#' Extract total length of plotted line in pixels
#' @param data 
#' @param metric
#' @param pixels_per 
extract_length_image <- function(data,metric="pixels",pixels_per=NULL) {
	if(metric!="pixels" & is.null(pixels_per)) stop("If metric is not pixels, pixels_per must be numeric")
	out <- do.call(rbind,lapply(split(data, data$item),function(z){
		length <- if(metric=="pixels"){ 
			line_distance(z$x,z$y) 
		}else{
			line_distance(z$x,z$y) / pixels_per
		}
		return(data.frame(file=as.character(z$file[1]),item=as.character(z$item[1]),length=length,metric=metric))
	}))
	return(out)
}


#' extract_length Function
#'
#' Extract total length of plotted line in pixels
#' @param data 
#' @param metric
#' @param pixels_per 
#' @export
#' @examples
#' #extract_length()
extract_length <- function(data,metric="pixels",pixels_per=NULL){
	z <-  do.call(rbind,c(lapply(data,extract_length_image,metric=metric,pixels_per=pixels_per),make.row.names = FALSE)) 
	return(z)
	}

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
extract_area_image <- function(data,metric="pixels",pixels_per=NULL){
	if(metric!="pixels" & is.null(pixels_per)) stop("If metric is not pixels, pixels_per must be numeric")
	out <- do.call(rbind,lapply(split(data, data$item),function(z){
		new_data <- get_outline(z)
		#points(new_data[,1],new_data[,2], col="red")

		all_points<-NULL
		for(j in unique(new_data[,1]))	all_points <- rbind(all_points,cbind(j,range(new_data[new_data[,1]==j,2])[1]:range(new_data[new_data[,1]==j,2])[2]))

		area <- if(metric=="pixels"){ 
			nrow(all_points)
		}else{
			nrow(all_points) / pixels_per
		}
		return(data.frame(file=as.character(z$file[1]),item=as.character(z$item[1]),area=area,metric=metric))
	}))
	return(out)
}


#' extract_area Function
#'
#' Extract total area inside plotted points in pixels. Uses get_outline to get the outline, then works out how many pixels are inside it, include the outline
#' @param data 
#' @export
#' @examples
#' #extract_length()
# extract_area <- function(data){
# 	do.call(rbind,lapply(data,function(group_data) {
# 		new_data <- get_outline(group_data)
# 		#points(new_data[,1],new_data[,2], col="red")

# 		all_points<-NULL
# 		for(j in unique(new_data[,1]))	all_points <- rbind(all_points,cbind(j,range(new_data[new_data[,1]==j,2])[1]:range(new_data[new_data[,1]==j,2])[2]))
# 		area <- nrow(all_points)
# 		return(area)
# 	}))
# }
extract_area <- function(data) do.call(rbind,c(lapply(data,extract_area_image),make.row.names = FALSE)) 




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

