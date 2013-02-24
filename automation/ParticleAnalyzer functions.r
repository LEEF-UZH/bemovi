video_to_morphology <- function(video.dir, difference.lag, thresholds=c(0,1000)) {

# copy master copy of ImageJ macro there for treatment
text <- readLines(paste(to.code, "ImageJ macros/Video_to_morphology.ijm", sep=""))

# use regular expression to insert input & output directory as well as difference lag
text[grep("avi_input = ", text)] <- paste("avi_input = ","'", video.dir,"';", sep = "")
text[grep("avi_output = ", text)] <- paste("avi_output = ","'",sub("1 - raw/","5 - Particle Analyzer data/",video.dir),"';", sep = "")
text[grep("lag = ", text)] <- paste("lag = ",difference.lag,";", sep = "")
text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep="")



# re-create ImageJ macro for batch processing of video files with Particle Analyzer
if(.Platform$OS.type == "windows") 
  writeLines(text,con=paste("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm",sep=""),sep="\n")
if(.Platform$OS.type == "unix") {
  dir.create(sub(raw.video.folder,"ijmacs",video.dir))	
  writeLines(text,con=paste(sub(raw.video.folder,"ijmacs",video.dir), "/Video_to_morphology_tmp.ijm",sep=""))
}

#create directory to store Particle Analyzer data
dir.create(sub(raw.video.folder,"5 - Particle Analyzer data",video.dir),showWarnings = FALSE)

# run to process video files by calling ImageJ
if(.Platform$OS.type == "unix")
  cmd <- paste("java -Xmx8192m -jar /Applications/ImageJ/ImageJ64.app/Contents/Resources/Java/ij.jar -ijpath /Applications/ImageJ -macro ", paste(sub("1 - raw","ijmacs",video.dir), "Video_to_morphology_tmp.ijm",sep=""))
if(.Platform$OS.type == "windows")
  cmd <- c('"C:/Program Files/FIJI.app/fiji-win64.exe" -macro Video_to_morphology_tmp.ijm')
system(cmd)

# delete temporary file after execution
if(.Platform$OS.type == "windows")
  file.remove("C:/Program Files/Fiji.app/macros/Video_to_morphology_tmp.ijm")

}


## This function gets the output files produced by the imagej macros previously created (by function MakeIJMacros)
## and run by function RunIJMacros
LoadIJ_morph_outs <- function(IJ_output.dir) {

## the macro file names
all.files <- dir(path=IJ_output.dir)
ijout.files <- all.files[grep("ijout", all.files)]
	
dd <- read.delim(paste(IJ_output.dir, ijout.files[1], sep="//"))
dd$file <- rep(gsub(".ijout.txt","",ijout.files[1]), length(dd[,1]))

# change column names because R is replacing missing header with X causing confusion with real X and Y positions
colnames(dd) <- c("obs","Area","Mean","Min","Max","X","Y","Perimeter","Major","Minor","Angle","Circ.","Slice","AR","Round","Solidity","file")

if(length(ijout.files)>2) {
	for(i in 2:length(ijout.files)) {

		dd.t <- read.delim(paste(IJ_output.dir, ijout.files[i], sep="//"))
		dd.t$file <- rep(gsub(".ijout.txt","",ijout.files[i]), length(dd.t[,1]))
		# change column names because R is replacing missing header with X causing confusion with real X and Y positions
  		colnames(dd.t) <- c("obs","Area","Mean","Min","Max","X","Y","Perimeter","Major","Minor","Angle","Circ.","Slice","AR","Round","Solidity","file")
		dd <- rbind(dd, dd.t)
	}
}

assign("morphology.data",dd,envir = .GlobalEnv)
write.table(morphology.data, file = paste(IJ_output.dir,"morphology.data.txt", sep = "/"), sep = "\t")
}
