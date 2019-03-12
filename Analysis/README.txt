# Data must be in the form of a tibble and the col type must be a date type 


# Overview of available methods
#
# STL Description
#	Take time series data and decompose into seasonal, trend, and remainder components
#	
# IQR Description
#	Inter-Quartile Range
#	Determine the percentile bouunds in order to determine a datum as an anomaly
#	default alpha = 0.05, IQR factor = 0.15, decrease alpha to reduce anomalies identified
#
# GESD Description
#	Generalize Extreme Studentized Deviate Test
#	Progressively eliminates outliers using a Student T-Test comparison (Grubb's Test)
#	comparing each test statistic to a critical value and removing a single outlier, 
#	the test statistic is updated after each outlier removed
#	once the test drops below a critical value all outliers are considered removed