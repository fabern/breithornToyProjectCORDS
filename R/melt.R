#' @title calculate_melt()
#'
#' @description calculate the melting rate of the glacier (m/day)
#' @param temperature average temperature
#' @param melt_factor a melting factor (m/day/degree).
#' @examples calculate_melt(temperature = 5, melt_factor = 0.1)
#' @export
calculate_melt <- function( temperature, melt_factor ) {

  if (melt_factor < 0) {
    stop("melt_factor cannot be negative. Please check your input parameters.")
  }

  if (temperature >= 0) {

    out <- melt_factor * temperature

  } else {

    out <- 0

  }

  return( out )
}


#' @title calculate_accumulation()
#'
#' @description Calculate accumulation at a point
#'
#' @param temperature Numeric. The temperature at the point of interest (in Celsius).
#' @param precipitation Numeric. The precipitation amount (in mm or other units).
#' @param threshold_temperature Numeric. The threshold temperature below which it snows (in Celsius).
#'
#' @return Numeric. The accumulation at the given point.
#' @examples calculate_accumulation(temperature = -2, precipitation = 10, threshold_temperature = 0)
#' @export
calculate_accumulation <- function(temperature, precipitation, threshold_temperature) {

  if (temperature <= threshold_temperature) {

    return(precipitation)

  } else {

    return(0)

  }
}

#' @title calculate_lapsed_temperature()
#'
#' @description Calculate lapsed temperature
#'
#' @param elevation Numeric. The elevation at the point of interest (in meters).
#' @param elevation_station Numeric. The elevation of the weather station (in meters).
#' @param temperature_station Numeric. The temperature at the weather station (in Celsius).
#' @param lapse_rate Numeric. The lapse rate (in Celsius per meter).
#'
#' @return Numeric. The lapsed temperature at the given elevation.
#' @examples
#' calculate_lapsed_temperature(elevation = 1500, elevation_station = 1000,
#' temperature_station = 10, lapse_rate = -0.0065)
#' @export
calculate_lapsed_temperature <- function(elevation, elevation_station, temperature_station, lapse_rate) {
  delta_z = elevation - elevation_station

  temperature = lapse_rate * delta_z + temperature_station

  return(temperature)
}


#' @title calculate_precip()
#'
#' @description generate synthetic precipitation data (m/day)
#' @param t time in days since beginning of simulation (currently deactivated)
#' @examples calculate_precip(t = 5)
#' @export
calculate_precip <- function( t ) {

  out <- 8e-3 # m/day

  return( out )
}



#' @title net_balance_fn()
#'
#' @description Integrate the balance rate over time for given temperature and
#'              precipitation arrays to get the "net balance".
#'
#' @param dt The time step.
#' @param Ts Array of temperatures.
#' @param Ps Array of precipitations.
#' @param melt_factor The factor to compute melt amount.
#' @param T_threshold The temperature threshold for accumulation.
#' @return The net balance at a point.
net_balance_fn <- function(dt, Ts, Ps, melt_factor, T_threshold) {
  stopifnot(length(Ts) == length(Ps))
  total <- 0.0
  for (i in seq_along(Ts)) {
    T <- Ts[i]
    P <- Ps[i]
    balance_rate <- -calculate_melt(T, melt_factor) + calculate_accumulation(T, P, T_threshold)
    total <- total + balance_rate * dt
  }
  return(total)
}

#' @title glacier_net_balance_fn()
#'
#' @description Calculate the glacier net balance and the net balance at each point.
#'
#' @param zs Array of elevations (with the weather station as datum).
#' @param dt The time step.
#' @param Ts Array of temperatures.
#' @param Ps Array of precipitations.
#' @param melt_factor The factor to compute melt amount.
#' @param T_threshold The temperature threshold for accumulation.
#' @param lapse_rate The lapse rate (temperature change per unit elevation change).
#' @return A list containing the glacier net balance [m] and net balance at all points [m].
glacier_net_balance_fn <- function(zs, dt, Ts, Ps, melt_factor, T_threshold, lapse_rate) {
  glacier_net_balance <- 0.0
  net_balance <- numeric(length(zs))
  for (i in seq_along(zs)) {
    z <- zs[i]
    TT <- sapply(Ts, calculate_lapsed_temperature, z = z, lapse_rate = lapse_rate)
    net_balance[i] <- net_balance_fn(dt, TT, Ps, melt_factor, T_threshold)
    glacier_net_balance <- glacier_net_balance + net_balance[i]
  }
  return(list(glacier_net_balance = glacier_net_balance / length(zs), net_balance = net_balance))
}
