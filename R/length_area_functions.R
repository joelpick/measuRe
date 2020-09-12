

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



#' integer_predict Function
#'
#' predict integers coordinates between two points
#' @param x x coords
#' @param y y coords
integer_predict <- function(x,y){
	mod <- lm(y~x)
	xR <- (round(x[1]):round(x[2]))
	yR <- round(coef(mod)[1] + coef(mod)[2]*xR)
	new_data <- cbind(xR,yR)
	return(new_data)
}

#' get_outline Function
#'
#' Impute outline from plotted points. connects sequentially plotted points with straight line, then finds max and min for each pixel, to get outline
#' @param x x coords
#' @param y y coords

get_outline <- function(x,y){
	## predict all integer points (pixels) in a straight line between each two sets of points 
	## first predict based for all x values

	new_data_x <- integer_predict(x[c(length(x),1)],y[c(length(y),1)])
	for(i in 2:length(x)){
		new_data_x <- rbind(new_data_x,integer_predict(x[(i-1):i],y[(i-1):i]))
	}
	## then predict based for all y values
	new_data_y <- integer_predict(y[c(length(y),1)],x[c(length(x),1)])
	for(i in 2:length(y)){
		new_data_y <- rbind(new_data_y,integer_predict(y[(i-1):i],x[(i-1):i]))
	}

	# remove any predicted coords outside range of clicked points
	new_data_y<-new_data_y[new_data_y[,2]%in%min(round(x)):max(round(x)),]
	new_data_x<-new_data_x[new_data_x[,2]%in%min(round(y)):max(round(y)),]

	## get rid of duplicate coordinates
	new_data_long<-rbind(new_data_x,new_data_y[,2:1])
	new_data<-new_data_long[!duplicated(new_data_long), ]
	return(new_data)
}

#' extract_area_item Function
#'
#' Extract all areas from xy coords
#' @param x x coords 
#' @param y y coords 
extract_area_item <- function(x,y){
	d<-get_outline(x,y)
	i<- min(d[,1]):max(d[,1])
	j<- min(d[,2]):max(d[,2])
	fill_x <- sapply(min(d[,1]):max(d[,1]), function(i) j >=min(d[d[,1]==i,2]) & j <= max(d[d[,1]==i,2]) )
	fill_y <- sapply(min(d[,2]):max(d[,2]), function(j) i >=min(d[d[,2]==j,1]) & i <= max(d[d[,2]==j,1]))
	overlap <- fill_x & t(fill_y)
	return(sum(overlap))
}


#' extract_area_image Function
#'
#' Extract all areas from one image
#' @param data 
#' @param metric
#' @param pixels_per 
extract_area_image <- function(data,metric="pixels",pixels_per=NULL){
	if(metric!="pixels" & is.null(pixels_per)) stop("If metric is not pixels, pixels_per must be numeric")
	out <- do.call(rbind,lapply(split(data, data$item),function(z){

		area <- if(metric=="pixels"){ 
			extract_area_item(z$x,z$y)
		}else{
			extract_area_item(z$x,z$y) / pixels_per
		}
		return(data.frame(file=as.character(z$file[1]),item=as.character(z$item[1]),area=area,metric=metric))
	}))
	return(out)
}


#' extract_area Function
#'
#' Extract total area inside plotted points in pixels. Uses get_outline to get the outline, then works out how many pixels are inside it. Includes the outline
#' @param data 
#' @param metric
#' @param pixels_per 
#' @export
#' @examples
#' #extract_area()
extract_area <- function(data,metric="pixels",pixels_per=NULL) do.call(rbind,c(lapply(data,extract_area_image,metric=metric,pixels_per=pixels_per),make.row.names = FALSE)) 




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

