# Building a New Geom
### Building Data Visualization Tools

Instructions

The purpose of this assignment is to draw on your knowledge of the grid and ggplot2 package to build a new geom. You will then need to load and tidy the provided dataset in order to plot the geom on a map.

Hurricanes can have asymmetrical wind fields, with much higher winds on one side of a storm compared to the other. Hurricane wind radii report how far winds of a certain intensity (e.g., 34, 50, or 64 knots) extended from a hurricane's center, with separate values given for the northeast, northwest, southeast, and southwest quadrants of the storm. The 34 knot radius in the northeast quadrant, for example, reports the furthest distance from the center of the storm of any location that experienced 34-knot winds in that quadrant.

This wind radii data provide a clearer picture of the storm structure than the simpler measurements of a storm's position and maximum winds. For example, if a storm was moving very quickly, the forward motion of the storm might have contributed significantly to wind speeds to the right of the storm's direction of forward motion, and wind radii might be much larger for the northeast quadrant of the storm than the northwest quadrant. These wind radii are available for Atlantic basin tropical storms since 1988 through the Extended Best Tract dataset, available here: http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/

Here is an example of the wind radii chart for Hurricane Katrina for the storm observation recorded at 2005-08-29 12:00:00 UTC, right around the time the storm made landfall.


![alt text](https://github.com/ZeroStack/Build-a-New-Geom/blob/master/tS300L0GEea4MxKdJPaTxA_2bc876f39664b8ee7e2ee77b70e4b5a5_wind_radii_example.png "Example")


