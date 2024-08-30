This site is part of the project "Connecting coastal communities with continuous, sensor-based 
monitoring of water quality." [3 or 4 sentences about the purpose of this site] 

The **left pane** controls the data displayed in the right pane. 

- *Site and year* restricts the graph and statistics to the selected site and year.
- *Time period* restricts the graph and statistics to the selected time period within the current site and year.
- *Units* selects the units to display in the graph.
- *Comparison threshold*, in the current units, is used in the statistical summary, and may be displayed on the 
time series graph with *Plot comparison threshold*.
- *Show distribution plot* turns on distribution plots to the right of the time series for 
sensor data and grab sample data (if selected). 
- *Plot grab sample data* includes grab samples (always in orange) in the time series , distribution plots, and summary
statistics.
- *Aggregation* includes three controls:
   - *Interval* turns on aggregation by selecting the interval to summarize over, from 1 hour to the enitre year.
   - *Statistic* selects from a number of statistics to apply in summarizing. The default is mean.
   - *Moving window* does a moving window smoothing of the data rather than providing a single summary value for each
interval. Note that moving windows are never applied to grab sample data.

The **right pane** displays the following:

- *Time series plot* displays dissolved oxygen from sensor data in **purple** and (optionally) the grab sample data in **orange** for 
the selected *site and year* and *time period*. You can zoom in to a subset of dates in this plot by dragging 
the mouse across the plot or moving the handles below the plot. Double-click to reset the zoom. This zooming only affects
the time series plot. The time series shows the *comparison threshold* if selected.
- *Distribution plots* are shown for sensor and (optionally) grab sample data to the right of the time series, if selected.
The distribution plots (known as "Sina plots"), correspond to the y-axis of the time series, showing one point for each 
sample in the data. The width of the distribution plot indicates the number of samples at value on the y-axis.
- *Summary statistics* shows summaries of the currently selected data. The DO events blow the comparison threshold
depend on the *comparison threshold* set in the left pane.
- *Sensor table* can be selected on the top menu to show the current sensor data, either raw or aggregated.
- *Grab sample table* on the top menu shows the current grab sample data, either raw or aggregated.

This site was developed by Bradley Compton and Ethan Plunkett of Data Core at the Center for
Data Science, Manning College of Information and Computer Sciences, UMass Amherst, 
for Seema Ravandale and Anita Milman of the Department of Environmental Conservation, UMass Amherst.
Funding was provided by the National Science Foundation.