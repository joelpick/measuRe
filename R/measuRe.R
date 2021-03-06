
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
	
	# if(n_new==0){
	# 	add_removeQ="f"
	# 	message("Good news - All images are processed!!!\n")
	# }else{
	# 	add_removeQ="a"
	# }
	add_removeQ="a"

	if(x11) x11(type = "cairo", bg = "black", width = 15, height = 15)


	buttons <- c("Prev", "Next","Add","Zoom","Zoom\nOut","Delete\nItem","Delete\nAll","Finish")
	button_cols <- c("tomato3", "tomato3","tomato4","darkorchid3","darkorchid4","dodgerblue3","dodgerblue4","black")

	i<-n_done
	while(i<(n_all+1)){
		i<-i+1

	#for(i in (n_done+1):n_all){
	#i=1
		n <- if(i<(n_all+1)){ paste0((i),"/",n_all) }else{ paste0(n_all,"/",n_all) }

		if(add_removeQ!="f"){ ## if not finished
			if(i==(n_all+1)){ ##if after last image 
				image_dat<-basic_plot(image=NULL,xlim=c(0,100),ylim=c(0,100),n=n,buttons=buttons,button_cols=button_cols)
				button_plot(c(0,50,100),c(0,50,100),"Good news - All images are processed!!!\n",back_col="white", text_col="black",border=FALSE,text_cex=1.2)
			}else{
				image <- magick::image_read(paste0(image_folder,"/",image_names[i]))
				image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
				
				###  load in group data and plot it
				if(is.null(all_data[[image_names[i]]])){
					group_data <- data.frame()
					j <- 1
				}else{
					group_data <- all_data[[image_names[i]]]
					j <- if(nrow(group_data)>0) { max(group_data$item)+1 }else{ 1 }
					#plot_points(group_data, col="blue")
					plot_points(group_data, current_item=j)
				}
			}


			
			add_removeQ="a"
			while(add_removeQ=="a"){
				select_points <- graphics::locator(1)
				clicked <- clickPosition(select_points,image_dat)
				##next
				if( clicked=="Next") {
					##save data
					#if(nrow(group_data)>0) 
					if(i<(n_all+1)) all_data[[image_names[i]]] <- group_data
					add_removeQ <- "b"
				}	
				##prev
				if( clicked=="Prev") {
					##save data
					#if(nrow(group_data)>0) 
					if(i<(n_all+1)) all_data[[image_names[i]]] <- group_data
					i <- if(i>1){ i-2 }else{ i-1 } 
					add_removeQ <- "b"
				}
				##add
				if( clicked=="Add" & i<(n_all+1)) {
					##save data
					#all_data[[image_names[i]]] <- group_data
					j <- if(nrow(group_data)>0){ max(group_data$item)+1 }else{ j } 
					image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
					#plot_points(group_data, col="blue")
					plot_points(group_data, current_item=j)
				}				
				##zoom
				else if(clicked=="Zoom" & i<(n_all+1)){
					button_plot(image_dat$background_x,image_dat$background_y,"Click on top left hand corner and bottom right hand corner\nof where you want to zoom in on", text_col = "black", back_col="white", border=FALSE, text_cex=1)

					# click on left hand corner and bottom right hand corner of where you want to zoom in on
					corners <- locator(2)

					## if clicked points are inside image then zoom, other cancel zoom
					if(sum(insideFunc(corners,image_dat$xlim,image_dat$ylim))==2) {
						image_dat <- basic_plot(image,xlim=sort(corners$x),ylim=sort(corners$y),n=n,buttons=buttons,button_cols=button_cols)
						#plot_points(group_data)
						if(nrow(group_data)>0) {
							plot_points(group_data, current_item=j)
							plot_buttons(image_dat,n,button_cols=button_cols)
						}
					}else{
						plot_buttons(image_dat,n,button_cols=button_cols)
					}
				}					
				##zoom out
				else if(clicked=="Zoom\nOut" & i<(n_all+1)){
					##deleted data and replot
					image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
					#plot_points(group_data, col="blue")
					plot_points(group_data, current_item=j)
				}##delete all
				else if(clicked=="Delete\nAll" & i<(n_all+1)){
					##deleted data and replot
					if(nrow(group_data)>0){
						group_data <- data.frame()
						image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
					}
					j<-1
				}
				##delete one item
				else if(clicked=="Delete\nItem" & i<(n_all+1)){
					##deleted data and replot
					if(nrow(group_data)>0){
						button_plot(image_dat$background_x,image_dat$background_y,"Click on group that want to redo", text_col = "black", back_col="white", border=FALSE, text_cex=1)
						remove <- locator(1)
						if(insideFunc(remove,image_dat$xlim,image_dat$ylim)){
							distances <- sqrt((remove$x-group_data$x)^2+(remove$y-group_data$y)^2)
							group_data <- subset(group_data,item!=group_data[which(distances == min(distances)),"item"])
							image_dat <- basic_plot(image,n=n,buttons=buttons,button_cols=button_cols)
							#plot_points(group_data, col="blue")
							plot_points(group_data, current_item=j)
						}else{
							plot_buttons(image_dat,n,button_cols=button_cols)
						}
					}
					j <- if(nrow(group_data)>0) { max(group_data$item)+1 }else{ 1 }
				}
				##finish
				else if(clicked=="Finish"){
					add_removeQ <- "f"
					## only save data if some points have been clicked
					if(i<(n_all+1)) all_data[[image_names[i]]] <- group_data
				}
				##if click on white box at bottom do nothing
				else if(clicked=="background"){
					add_removeQ="a"
				}
				else if(clicked=="image" & i<(n_all+1)){
					points(select_points$x,select_points$y, col="red", pch=20, cex=1)
					if(nrow(group_data[group_data$item==j,])>0) lines(c(select_points$x,group_data$x[nrow(group_data)]),c(select_points$y,group_data$y[nrow(group_data)]), col="red", lwd=2)	
					group_data <- rbind(group_data, data.frame(file=image_names[i], item=j, x=select_points$x, y=select_points$y) )
				}
			}
		}
	}
	

	dev.off()
	## get rid of empty list entries
	all_data <- all_data[sapply(all_data, function(x) nrow(x)>0)]
	save(all_data, file=output_file)

	
	return(all_data)
}
