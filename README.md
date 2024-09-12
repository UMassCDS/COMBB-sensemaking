# COMBB: Data Sensemaking Tool

The “COMBB: Data Sensemaking Tool” provides an interface for the exploration of dissolved oxygen monitoring data collected using two differing methodologies: grab sample and in-situ automated sensor technologies. The tool allows a user to visualize the data, calculate statistics at varying time-intervals, and compare across the data collected using each methodology.

This tool was created for use during semi-structured interviews as a mechanism for eliciting information about how individuals make sense and interpret data that has been collected at differing time intervals. However, this tool is publicly available and can be used by anyone to examine the datasets included.

This riik was developed by Bradley Compton and Ethan Plunkett of Data Core at the Center for Data Science, Manning College of Information and Computer Sciences, UMass Amherst, for Seema Ravandale and Anita Milman of the Department of Environmental Conservation, UMass Amherst. This tool was developed as part of the Continuous Oxygen Monitoring in Buzzards Bay (COMBB) research project, supported in part by the National Science Foundation under Grant No. 2317235 A.

## Running this app on your own computer

 1.   Go to https://github.com/bwcompton/buzzbay
 1.   Click on the big green Code button and select Download ZIP
 1.   Unzip the resulting file somewhere on your computer
 1.   Fire up RStudio
 1.   Type setwd(path to unzipped file), eg.,

```
     setwd('x:/temp/buzzbay-main/')
```

6.    Open buzzbay.app.R in RStudio (File > Open File..., or the Open icon on the toolbar)
7.    At this point it should suggest packages to install. Say yes. The alternative is to install each one you don't already have by hand.
8.    Once it's done installing, at the top right of the buzzbay.app window, see the Run App button. Click on the little triangle to the right of it and select Run External. This will make it open in your browser.
9.    Cross your fingers and click Run App!
