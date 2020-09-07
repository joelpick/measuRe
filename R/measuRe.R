
#library(scales)




#' measuRe Function
#'
#' This function allows to measure objects on images in pixels
#' @param image_folder Directory path with images inside. Should only contain images 
#' @param x11 Logical. use x11? defaults to FALSE
#' @export
#' @examples
#' #measuRe()
measuRe <- function(image_folder, x11=FALSE){
	#	image_folder="~/Dropbox/measuRe_package/test"
	#	output_file="~/Dropbox/measuRe/lengths.Rdata"
	
	output_file <- paste0(image_folder,"/SavedData.Rdata")
	
	if(file.exists(output_file)){ 
		load(output_file) 
	}else{
		all_data <- list()
	}

	all_image_names <- list.files(image_folder)
	all_image_names <- all_image_names[!all_image_names %in% "SavedData.Rdata"]
	
	done_images <- all_image_names[all_image_names %in% names(all_data)]
	new_images <- all_image_names[!all_image_names %in% names(all_data)]
	image_names <- c(done_images,new_images)

	n_done <- length(done_images)
	n_new <- length(new_images)
	n_all <- length(image_names)
	
	if(n_new==0){
		add_removeQ="f"
		message("Good news - All images are processed!!!\n")
	}else{
		add_removeQ="a"
	}
	

	if(x11) x11(type = "cairo", bg = "black", width = 15, height = 15)


	buttons <- c("Prev", "Next","Add","Zoom","Zoom\nOut","Redo","Redo\nAll","Finish")
	button_cols <- c("tomato3", "tomato3","tomato4","darkorchid3","darkorchid4","dodgerblue3","dodgerblue4","black")

	i<-n_done
	while(i<n_all){
		i<-i+1

	#for(i in (n_done+1):n_all){
	#i=1
		n <- paste0((i),"/",n_all)

		if(add_removeQ!="f"){ ## if not finished
			image <- magick::image_read(paste0(image_folder,"/",image_names[i]))
			
			image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)

			### need to load in group data and plot it!!!!!!
			if(is.null(all_data[[image_names[i]]])){
				group_data <- data.frame()
				j <- 1
			}else{
				group_data <- all_data[[image_names[i]]]
				j <- unique(group_data$item)+1
				plot_points(group_data)
			}
			
			add_removeQ="a"
			while(add_removeQ=="a"){
				select_points <- graphics::locator(1)
				clicked <- clickPosition(select_points,image_dat)
				##next
				if( clicked=="Next") {
					##save data
					if(nrow(group_data)>0) all_data[[image_names[i]]] <- group_data
					add_removeQ <- "b"
				}	
				##prev
				if( clicked=="Prev") {
					##save data
					if(nrow(group_data)>0) all_data[[image_names[i]]] <- group_data
					i <- if(i>1){ i-2 }else{ i-1 } 
					add_removeQ <- "b"
				}
				##add
				if( clicked=="Add") {
					##save data
					#all_data[[image_names[i]]] <- group_data
					j=j+1
				}				
				##zoom
				else if(clicked=="Zoom"){
					button_plot(image_dat$background_x,image_dat$background_y,"Click on top left hand corner and bottom right hand corner\nof where you want to zoom in on", text_col = "black", back_col="white", border=FALSE, text_cex=1)

					# click on left hand corner and bottom right hand corner of where you want to zoom in on
					corners <- locator(2)

					## if clicked points are inside image then zoom, other cancel zoom
					if(sum(insideFunc(corners,image_dat$xlim,image_dat$ylim))==2) {
						image_dat <- basic_plot(image,xlim=sort(corners$x),ylim=sort(corners$y),n=n,buttons=buttons,button_cols=button_cols)
						plot_points(group_data)
					}else{
						plot_buttons(image_dat,n,button_cols=button_cols)
					}
				}					
				##zoom out
				else if(clicked=="Zoom\nOut"){
					##deleted data and replot
					image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
					plot_points(group_data)
				}##redo all
				else if(clicked=="Redo\nAll"){
					##deleted data and replot
					group_data <- data.frame()
					image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
				}
				##redo one item
				else if(clicked=="Redo"){
					##deleted data and replot
					group_data <- subset(group_data, item<j)
					image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
					plot_points(group_data)
				}
				##finish
				else if(clicked=="Finish"){
					add_removeQ <- "f"
					## only save data if some points have been clicked
					if(nrow(group_data)>0) all_data[[image_names[i]]] <- group_data
				}
				##if click on white box at bottom do nothing
				else if(clicked=="background"){
					add_removeQ="a"
				}
				else if(clicked=="image"){
					points(select_points$x,select_points$y, col="red", pch=20, cex=1)
					if(nrow(group_data[group_data$item==j,])>0) lines(c(select_points$x,group_data$x[nrow(group_data)]),c(select_points$y,group_data$y[nrow(group_data)]), col="red", lwd=2)	
					group_data <- rbind(group_data, data.frame(file=image_names[i], item=j, x=select_points$x, y=select_points$y) )
				}
			}
		}
	}
	
	if(n_new>0){
	dev.off()
	save(all_data, file=output_file)
	}
	
	return(all_data)
}
