#' Function to load in a specific variable

#' @param dir path to temporary directory.
#' @param verbose  Should feedback be output to the console?

assignInNamespace(
	"loadVar", 
	function(dir, verbose=FALSE){
		# read in the raster
		mod <- platemodel(file.path(dir, "paleomap_model_Scotese2016.mod"))
	
		return(mod)
	}, 
	ns="chronosphere")
