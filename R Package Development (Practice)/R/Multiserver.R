#' @param Arrivals A numeric vector of customer arrival times.
#' @param ServiceTimes A numeric vector of service times for each customer, same length as Arrivals.
#' @param NumServers The number of servers, default is 1.
#'
#' @return A tibble with columns: Arrivals, ServiceBegins, ChosenServer, ServiceEnds.
#' @examples
#' \dontrun{
#' set.seed(2048)
#' arrival_time <- cumsum(rexp(100, 1/60))
#' service_time <- rexp(length(arrival_time), 1/150) + 20
#' Multiserver(arrival_time, service_time, NumServers = 1)
#' }
#' @export
Multiserver <- function(Arrivals, ServiceTimes, NumServers = 1) {
  if (any(Arrivals <= 0 | ServiceTimes <= 0) || NumServers <= 0)
    stop("Arrivals, ServiceTimes, and NumServers must be positive")
  if (length(Arrivals) != length(ServiceTimes))
    stop("Arrivals and ServiceTimes must have same length")

  NumCust <- length(Arrivals)
  AvailableFrom <- rep(0, NumServers)
  ChosenServer <- ServiceBegins <- ServiceEnds <- rep(0, NumCust)

  for (i in seq_along(Arrivals)) {
    avail <- min(AvailableFrom)
    ChosenServer[i] <- which.min(AvailableFrom)
    ServiceBegins[i] <- max(avail, Arrivals[i])
    ServiceEnds[i] <- ServiceBegins[i] + ServiceTimes[i]
    AvailableFrom[ChosenServer[i]] <- ServiceEnds[i]
  }

  tibble::tibble(
    Arrivals = Arrivals,
    ServiceBegins = ServiceBegins,
    ChosenServer = ChosenServer,
    ServiceEnds = ServiceEnds
  )
}
