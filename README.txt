README

### 3/2020

Hi! This folder (or whatever containment unit you are reading this from) contains R scripts that I use to download and prepare data for ecological niche modeling, as well as generate and evaluate models in ENMEval. Much of the code requires common packages for niche modeling and ecological analyses, such as dismo and ecospat, and essentiall acts as a wrap-around or master script for many of the commands/tools I use most. I have also added in scripts to visualize and compare distributions of suitabiltiy scores, compute evaluation metrics that are not included in the ENMEval package, and add input-type flexibility for a variety of analyses that are generally performed excusively inside one R package with somewhat strict data format requirements. I will continue to update and add to these scripts.

###################################################################################################
Some input parameters:

	usr.occs
	usr.env
	
###################################################################################################

### Requirements
RequiredLibraries.R

thin_max.R # by Dan Warren, available at http://enmtools.blogspot.com/

CE_equal_widths.R # code from CalibratR
calibratR_CE_functions.R # code from CalibratR

###Pre-Modeling:
OccDatPrep.R
RarefyOccurrences.R
RasterLayerProcessing.R
GetRasterCorr.R
GetBackgroundPoints.R
CalcMahalanobisDist.R

###Modeling:
ENMeval_Modeling.R

###Post-Modeling:
AvgRasterDiff.R
BetaNicheWidth.R
ComparSuitability.R
extract_ENMEval_data.R
GetCBI.R
GetCE.R
GetHypervolume.R
PlotEcoGeoCurves.R
ThresholdModel.R

###################################################################################################

To get started, just source the RequiredLibraries.R script! It will check a list of required libraries against your installed libraries and then install any that are still needed. It will then load all of the libraries and source the functions scripts, which are located in the /Functions/ folder.

###################################################################################################

I also have some other scripts in the "Niche_Things" folder that are either works in progress, or scripts that tie together commands for analyses I've performed prior to the creation of "Niche_Things" (e.g., the ENMTools scripts and the background test from raster script).

