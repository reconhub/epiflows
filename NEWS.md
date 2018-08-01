# epiflows 0.1

## Changes to class

* epiflows class inherits the epicontacts class. This class is built from two
  data frames describing a linlist (referred here as `locations`) and contacts
  (referred here as `flows`). This was chosen over the previous implementation
  of two named vectors of inward and outward flows with a data frame of 
  locations to allow the user to flexibly store more than one focus. An extra
  element called `vars` has been added, storing a dictionary of variables that
  are present in the data frame for use with `estimate_risk_spread()`. These
  variables are accessible via `global_vars()`
  
## Changes to estimate_risk_spread

* The function is now a generic with a default and an epiflows method.
* The number of options have been reduced by combining the function and parameter
  arguments into one.
* All errors are collected and reported if multiple arguments are missing or misspelled
* Default varaibles have been set
* a new parameter, `return_all_simulations` has been added.

## Changes to constructor

* `make_epiflows()` is now a generic with methods for data frame, integer, and 
   numeric input.
  
## Changes to plotting

* `plot()` defaults to an interactive network plot from `visNetwork()` if no
   coordinates are available.
* `map_epiflows()` places flows on a map
* `vis_epiflows()` places flows on a network
* `grid_epiflows()` places flows on a bubble plot/grid

## New functions

* `get_flows()` returns the flows data frame optionally specifying flows from/to a given location
* `get_locations()` returns the locations data frame
* `get_coordinates()` returns coordinates or `NULL` from the locations data frame
* `get_id()` returns the identifier of all locations
* `get_n()` returns a numeric vector of cases from/to a given location
* `get_pop_size()` returns a vector of population sizes for locations
* `get_vars()` returns a specified variable from the locations data frame
   OR returns the defined variables if no arguments are given.
* `set_vars()` allows the user to set or reset the global variables.
* `global_vars()` can return, set, and reset globally recognized variables
* `as.SpatialLinesDataFrame()` converts an epiflows object with coordinates to
   a `SpatialLinesDataFrame` class from the *sp* package.
  
## New data sets

* `Brazil_epiflows` is an epiflows object created from the `YF_Brazil` data
* `YF_flows` is the data frame of flows from `YF_Brazil`
* `YF_locations` is the data frame of locations from `YF_Brazil`
* `YF_coordinates` are the coordinates for the locations in `YF_Brazil`

## Removed functions/data

* `get_codes()` has been removed in favor of `get_id()`
* `get_flow_data()` has been removed in favor of `get_n()` and `get_flows()`
* `get_location_data()` has been removed in favor of `get_locations()`
* `Mex_travel_2009` has been removed

## Misc

* `add_coordinates()` can now take a data frame input
* Magrittr pipes are no longer imported. 
* visNetwork is imported
* vdiffr is used for visual tests
* continuous integration and automated tests have been set up
* A new vignette describing the epiflows class has been added
* README and introduction vignette have been updated.

# epiflows 0.0.1.9000

* Added a `NEWS.md` file to track changes to the package.



