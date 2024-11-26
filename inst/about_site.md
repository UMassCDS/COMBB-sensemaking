The “COMBB: Data Sensemaking Tool” provides an interface for the exploration of dissolved oxygen monitoring data 
collected using two differing methodologies: grab sample and in-situ automated sensor technologies. The tool allows
a user to visualize the data, calculate statistics at varying time-intervals, and compare across the data collected 
using each methodology.  

The tool was created for use during semi-structured interviews as a mechanism for eliciting 
information about how individuals make sense and interpret data that has been collected at different time intervals. 
The tool is publicly available and can be used by anyone to examine the datasets included. 


#### The **left pane** controls the data displayed in the right pane

- *Site and year* controls which site and year's data will be displayed and summarized.
- *Time period* restricts the graph and statistics to the selected time period within the current site and year.
- *Plot grab sample data* includes grab samples (always in **orange**) in the time series, distribution plots, and summary
statistics. The grab samples are collected manually in the field and thus tend to be infrequent.  
- *Plot sensor data* includes sensor data (always in **purple**) in th etime series, distribution plots, and summary statistics.
- *Comparison threshold* sets a level of DO, in the current units, that the user considers important, typically because they are 
concerned when DO drops below that level. It is used in the statistical summary, and may be displayed on the time series graph 
with *Plot comparison threshold*. 
- *Show distribution plot* turns on plots to the right of the time series for 
sensor data and grab sample data (if selected), 
that show how observations are distributed across the range of dissolved oxygen values. 
- *Aggregation* includes three controls that affect if and how the raw data are summarized.
   - *Interval* turns on aggregation by selecting the interval to summarize over, from 1 hour to the entire year. "None" turns off aggregation.
   - *Statistic* selects from a number of statistics to apply in summarizing. The default is Mean.
   - *Moving window* applies a moving window smooth of the data rather than providing a single summary value for each non-overlapping 
   interval. Note that moving windows are never applied to grab sample data.
   - For example: setting *Interval* to "weekly," 
   *Statistic* to "Mean," and leaving *Moving window* off will result in a
   plot that shows the mean value for each non-overlapping week in the data---a 
   single value for every week.  Turning *Moving window* on will then 
   add intervening values that each represent the mean for a week centered on
   the associated date and time.  


#### The **right pane** displays data

##### Plot tab
- *Time series plot* displays dissolved oxygen from sensor data in **purple** and the grab sample data in **orange** for 
the selected *site and year* and *time period*. You can zoom in to a subset of dates in this plot by dragging 
the mouse across the plot or moving the handles below the plot. Double-click to reset the zoom. This zooming only affects
the time series plot. The time series shows the *comparison threshold* if selected.
- *Distribution plots* are shown for sensor and (optionally) grab sample data to the right of the time series, if selected.
The distribution plots (known as "<a href="https://en.wikipedia.org/wiki/Sina_plot" target="_blank" rel="noopener noreferrer">Sina plots</a>"),
correspond to the y-axis of the time series, showing one point for each 
sample in the data. The width of the distribution plot indicates the number of samples for that value on the y-axis.

##### Sensor tab
- *Sensor table* can be selected on the top menu to show the current sensor data, either raw or aggregated.

##### Grab sample tab
- *Grab sample table* on the top menu shows the current grab sample data, either raw or aggregated.

##### Summary stats tab
- *Summary statistics* shows summaries of the currently selected data. The DO events below the comparison threshold
depend on the *comparison threshold* set in the left pane.

------

This site was developed by Bradley Compton and Ethan Plunkett of 
<a href="https://ds.cs.umass.edu/programs/data-core" target="_blank" rel="noopener noreferrer">Data Core at the Center for
Data Science</a>, Manning College of Information and Computer Sciences, University of Massachusetts, Amherst, 
as part of the 
<a href="https://watergovernance.umasscreate.net/water-quality-monitoring" target="_blank" rel="noopener noreferrer">Continuous Oxygen Monitoring in Buzzards Bay (COMBB) research project</a>, supported in part by the National Science Foundation under
<a href="https://www.nsf.gov/awardsearch/showAward?AWD_ID=2317235&HistoricalAwards=false" target="_blank" rel="noopener noreferrer">Grant No. 2317235 A</a>.


![UMass logo](../www/umass_logo.png){width=20%}
