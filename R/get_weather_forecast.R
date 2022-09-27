#' Weather forecast API
#' 
#' Weather forecast data for five large cities in Sweden. Could be used with a shiny application.
#' 
#' @import httr jsonlite stringi
#' 
#' @param city A string of a city with an uppercase for the first letter
#' 
#' @param date A string of a date in format "YYYY-MM-DD"
#' 
#' @return A \code{data.frame} with time, temperature, humidity, precipitation, cloudcover, wind speed, wind direction and city for every hour.
#' 
#' @examples
#' # Weather forecast for Stockholm today.
#' get_weather_forecast(city="Stockholm", date=Sys.Date()) 
#' 
#' # Weather forecast for Göteborg tomorrow.
#' get_weather_forecast(city="Göteborg", date=Sys.Date()+1)
#' 
#' @references Open-Meteo. Weather Forecast API. \url{https://open-meteo.com/en/docs}
#' 
#' @export

get_weather_forecast <- function(city,
                                 date){
  if(date>Sys.Date()+7|date<Sys.Date()){stop("False date input")}
  if(city=="Link\u00f6ping"){
    latitude <- 58.41
    longitude <- 15.62
  } else if (city=="Stockholm"){
    latitude <- 59.33
    longitude <- 18.07
  } else if (city=="Uppsala"){
    latitude <- 59.86
    longitude <- 17.64
  } else if (city=="Malm\u00f6"){
    latitude <- 55.60
    longitude <- 13.00
  } else if (city=="G\u00f6teborg"){
    latitude <- 57.71
    longitude <- 11.97
  } else {
    stop("City is not supported")
  }
  url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=", latitude, "&longitude=", longitude, "&hourly=temperature_2m,relativehumidity_2m,precipitation,cloudcover,windspeed_10m,winddirection_10m&timezone=auto&start_date=", date, "&end_date=", date) 
  raw_data <- httr::GET(url)
  content <- base::rawToChar(raw_data$content)
  content_JSON <- jsonlite::fromJSON(content, flatten=TRUE)
  data <- as.data.frame(content_JSON$hourly)
  colnames(data) <- c("Time", "Temperature", "Humidity", "Precipitation", "Cloudcover", "Wind_speed","Wind_direction")
  data <- cbind(data, City=city)
  return(data)
}
