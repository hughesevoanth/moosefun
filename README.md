# Random useful functions for Moose's work

### Authors: David Hughes 
##### Date started: 31st Jan. 2020

### About:

This repo contains a variety of helpful functions that I come back to time and time again.  Rather than adding them into a variety of different packages I will try to maintain this catch-all that I and my colleagues can source when necessary. 

### Installation instructions of the moosefun package

	1. insure that the devtools library is installed on your local machine
		 > ifelse("devtools" %in% rownames(installed.packages()), 
		 NA, 
		 install.packages("devtools"))
		 
	2. install moosefun
		> devtools::install_github("hughesevoanth/moosefun")
		
### List of functions and their utility

	1. ztransform() a function taken from the GenABEL package to perform z-transformations. This function is necessary to run my edited version of the rank normal transformation function.
	2. rntransform() an edited version of the function taken from the GenABEL package to perform rank normal transformations. It has been edited to randomly split tied values.  