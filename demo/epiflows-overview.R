#' Example: yellow fever in south-east Brazil 
#' ==========================================
#'   
#'   In this Section we provide an example on how to use `epiflows` to
#' calculate the number of yellow fever cases spreading from south-east
#' Brazil to other countries due to human movement. We show how to define
#' the arguments of the `estimate_risk_spread()` function, interpret the
#' results, and make visualizations with the population flows.
#' 
#' Data
#' ----
#' 
#' We use the data `YF_Brazil` which is contained in the `epiflows` package as
#' `data(YF_Brazil)`. `YF_Brazil` is a list containing the population size, the
#' assumed number of yellow fever infections, dates of first and last case
#' reporting, number of travellers and length of stay for the states of Espirito
#' Santo, Minas Gerais, Rio de Janeiro, Sao Paulo, and for the whole region of
#' Southeast Brazil (which comprises the four states of Espirito Santo, Minas
#' Gerais, Rio de Janeiro and Sao Paulo) in the period December 2016 to May
#' 2017.
#' 
#' `YF_Brazil$states` is a data frame that contains, for each Brazilian state
#' considered in our example, the code (`location_code`), the population
#' (`location_population`), the number of assumed infections in the time window
#' (`num_cases_time_window`), and the dates of the first and last case reported
#' (`first_date_cases` and `last_date_cases`, respectively).

library("epiflows")
data("YF_Brazil")

YF_Brazil$states

#' `YF_Brazil$T_D` represents $T^W_{S,D}$ and contains the number of
#' travellers from each of the Brazilian states to other countries.
#' `YF_Brazil$T_O` represents $T^W_{O,S}$ and contains the number of
#' travellers from other countries to each of the Brazilian states.

YF_Brazil$T_D

YF_Brazil$T_O

#' Finally, `YF_Brazil$length_of_stay` is a vector with the lengths of stay
#' (in days) of the travellers visiting Brazil from other countries.

YF_Brazil$length_of_stay

#' An `epiflows` object can be created with the `make_epiflows()` function
#' by providing a data frame `flows` with the number of travellers between
#' locations, a data frame `locations` with information about the
#' locations, and the names of the columns of data frame `locations`
#' indicating the name of each variable.
#' 
#' In the data frame `flows` each row represents the number of travellers
#' from one location to the next. `flows` has at least three columns:
#' columns `from` and `to` indicating where the flow starts and ends,
#' respectively, and column `n` indicating the number of travellers that
#' are in the flow. We can create a data frame `YF_flows` with the
#' population flows of the Brazil data as follows.

from     <- as.data.frame.table(YF_Brazil$T_D, stringsAsFactors = FALSE)
to       <- as.data.frame.table(t(YF_Brazil$T_O), stringsAsFactors = FALSE)
YF_flows <- rbind(from, to)
colnames(YF_flows) <- c("from", "to", "n")

head(YF_flows)

#' In data frame `locations` each row represents a location, and columns
#' specify useful information about the locations such as ID, population,
#' number of cases, dates and length of stay. `locations` must contain at
#' least one column specifying the location ID used in the `flows` data
#' frame. We can create the data frame `YF_locations` by combining the data
#' frame `YF_Brazil$states` containing the Brazil states information, and
#' the vector `YF_Brazil$length_of_stay` containing the lengths of stay.

YF_locations <- YF_Brazil$states
los          <- data.frame(location_code = names(YF_Brazil$length_of_stay), 
                           length_of_stay = YF_Brazil$length_of_stay)
YF_locations <- merge(YF_locations, los, by = "location_code", all = TRUE)

head(YF_locations)

#' Then, we can create an `epiflows` object called `Brazil_epiflows` as follows.

Brazil_epiflows <- make_epiflows(flows         = YF_flows, 
                                 locations     = YF_locations, 
                                 pop_size      = "location_population",
                                 duration_stay = "length_of_stay",
                                 num_cases     = "num_cases_time_window",
                                 first_date    = "first_date_cases",
                                 last_date     = "last_date_cases"
)

#' The second argument of `estimate_risk_spread()` is `location_code` which
#' is a character string denoting the infectious location code. We also
#' need to specify the incubation and infectious period distributions.
#' Specifically, we need to provide functions with a single argument `n`
#' that generate `n` random incubation and infectious periods. In this
#' example, we assume that the incubation period $T_E$ is log-normally
#' distributed with mean 4.6 days and variance 2.7 days, and that the
#' infectious period $T_I$ is normally distributed with mean 4.5 days and
#' variance 0.6 days. We can define functions `incubation()` and
#' `infectious()` as

incubation <- function(n) {
  rlnorm(n, 1.46, 0.35)
}

infectious <- function(n) {
  rnorm(n, 4.5, 1.5/1.96)
}

#' Once we have constructed the objects needed to call `estimate_risk_spread()`
#' we can execute the function and obtain the estimated mean number of cases
#' spread to each country and the 95% confidence intervals. The code to
#' calculate the cases spread from Espirito Santo is the following:

set.seed(2018-07-25)
res <- estimate_risk_spread(Brazil_epiflows, 
                            location_code = "Espirito Santo",
                            r_incubation = incubation,
                            r_infectious = infectious,
                            n_sim = 1e5
)

#' The results returned by `estimate_risk_spread()` are stored in the `res`
#' object. This is a data frame with the columns `mean_cases` indicating
#' the mean number of cases spread to each location, and `lower_limit_95CI`
#' and `upper_limit_95CI` indicating the lower and upper limits of 95%
#' confidence intervals. The result object is shown below.

res

#' We can plot the result as follows
#+ plotresults, fig.width = 7, fig.height = 4, out.width = "150mm"

library("ggplot2")
res$location <- rownames(res)
ggplot(res, aes(x = mean_cases, y = location)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower_limit_95CI, xmax = upper_limit_95CI), height = .25) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  ggtitle("Yellow Fever Spread from Espirito Santo, Brazil") +
  xlab("Number of cases") +
  xlim(c(0, NA))

#' Note that if `return_all_simulations = TRUE`, the resulting object `res` will
#' be a data frame with all simulations.

res <- estimate_risk_spread(Brazil_epiflows, 
                            location_code = "Espirito Santo",
                            r_incubation = incubation,
                            r_infectious = infectious,
                            n_sim = 1e5,
                            return_all_simulations = TRUE
)
head(res)

#' Using `res`, we can calculate the mean and 95% confidence interval as follows.

meancases <- colMeans(res, na.rm = TRUE)
quant     <- t(apply(res, 2, stats::quantile, c(.025, .975), na.rm = TRUE))
data.frame(mean_cases = meancases, 
           lower_limit_95CI = quant[, 1], 
           upper_limit_95CI = quant[, 2]
)

#' Visualize population flows
#' --------------------------
#' 
#' ### Flows displayed on an interactive map
#' 
#' We can visualize population flows on an interactive map using `plot()`
#' with the parameter . For this option to work, the `epiflows` object
#' needs to include the longitude and latitude of the locations in decimal
#' degree format. If coordinates are known, they can be added to the object
#' using the function from the package. For example, if longitude and
#' latitude are in data frame `YF_coordinates`, they can be added as
#' follows.

data("YF_coordinates")
head(YF_coordinates)
Brazil_epiflows <- add_coordinates(Brazil_epiflows,
                                   coordinates = YF_coordinates[, -1])

#' If coordinates are unknown, we may resort to one of the freely available
#' tools for geocoding. For example, we can use the `geocode()` function
#' from the package `ggmap` This function finds the latitude and
#' longitude of a given location using either the Data Science Toolkit
#' <http://www.datasciencetoolkit.org/about> or Google Maps
#' <http://code.google.com/apis/maps/documentation/geocoding/>. We can also
#' use `add_coordinates()` which uses to find the coordinates and directly
#' add them to the object as follows.

Brazil_epiflows <- add_coordinates(Brazil_epiflows, overwrite = TRUE)

#' Once we have assigned coordinates to the `epiflows` object, we can use
#' with to visualize the population flows between locations in an
#' interactive map.
#+ flowsmap, fig.width = 7, fig.height = 7, dpi = 600
plot(Brazil_epiflows, type = "map")

#' 
#' ### Flows displayed as a network
#+ flowsnetwork, fig.width = 7, fig.height = 7, dpi = 600

plot(Brazil_epiflows, type = "network")

#' ### Flows displayed as a grid between origins and destinations
#+ flowsgrid, fig.width = 7, fig.height = 6, out.width = "150mm", dpi = 600

plot(Brazil_epiflows, type = "grid") +
  theme(text = element_text(size = 12, family = "Helvetica"))