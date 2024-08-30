   

The Data Sensemaking web application supports the [Continuous Oxygen Monitoring in Buzzards Bay (COMBB) project](https://www.woodwellclimate.org/project/combb/) 
by facilitating exploration of dissolved oxygen (DO) data collected with continuously deployed sensors. 
It allows users to summarize the data in a variety of ways and compare with less frequent grab samples that have
historically been used to monitor Buzzards Bay.

The **left pane** controls the data displayed in the right pane. 

- *Site and year* controls which site and year's data will be displayed and summarized.
- *Time period* restricts the graph and statistics to the selected time period within the current site and year.
- *Units* selects the units to display in the graph.
- *Comparison threshold*, Sets a level of DO, in the current units, that the user considers important, typically because they are concerned when DO drops below that level. It is used in the statistical summary, and may be displayed on the 
time series graph with *Plot comparison threshold*. 
- *Show distribution plot* turns on plots to the right of the time series for 
sensor data and grab sample data (if selected), 
that show how observations are distributed across the range of dissolved oxygen values. 
- *Plot grab sample data* includes grab samples (always in orange) in the time series , distribution plots, and summary
statistics. The grab samples are collected manually in the field and thus tend to be infrequent.  
- *Aggregation* includes three controls that affect if and how the raw data is summarized.
   - *Interval* turns on aggregation by selecting the interval to summarize over, from 1 hour to the entire year. "None" turns off aggregation.
   - *Statistic* selects from a number of statistics to apply in summarizing. The default is mean.
   - *Moving window* does a moving window smoothing of the data rather than providing a single summary value for each complete, non-overlapping, interval. Note that moving windows are never applied to grab sample data.
   - An example of how aggregation works: setting *Interval* to "weekly", 
   *Statistic* to "Mean", and leaving *Moving window* off will result in a
   plot that shows the mean value for each non-overlapping week in the data -
   a single value for every week.  Turning *Moving window* on will then 
   add intervening values that each represent the mean for a week centered on
   the associated date and time.  


The **right pane** displays the following:

- *Time series plot* displays dissolved oxygen from sensor data in **purple** and (optionally) the grab sample data in **orange** for 
the selected *site and year* and *time period*. You can zoom in to a subset of dates in this plot by dragging 
the mouse across the plot or moving the handles below the plot. Double-click to reset the zoom. This zooming only affects
the time series plot. The time series shows the *comparison threshold* if selected.
- *Distribution plots* are shown for sensor and (optionally) grab sample data to the right of the time series, if selected.
The distribution plots (known as "[Sina plots](https://en.wikipedia.org/wiki/Sina_plot)"), correspond to the y-axis of the time series, showing one point for each 
sample in the data. The width of the distribution plot indicates the number of samples for that value on the y-axis.
- *Summary statistics* shows summaries of the currently selected data. The DO events below the comparison threshold
depend on the *comparison threshold* set in the left pane.
- *Sensor table* can be selected on the top menu to show the current sensor data, either raw or aggregated.
- *Grab sample table* on the top menu shows the current grab sample data, either raw or aggregated.

------

This site was developed by Bradley Compton and Ethan Plunkett of [Data Core at the Center for
Data Science](https://ds.cs.umass.edu/programs/data-core), Manning College of Information and Computer Sciences, UMass Amherst, 
for Seema Ravandale and Anita Milman of the [Department of Environmental Conservation](https://www.umass.edu/environmental-conservation/), UMass Amherst.

Funding was provided by the National Science Foundation.