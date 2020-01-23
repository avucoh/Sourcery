# Functions that interact with the HIRN Resource Browser API

# apikey should be stored as an environment variable
getResourceSchema <- function(apikey) {
  response <- httr::GET("https://test-resourcebrowser.azurewebsites.net/api/v1/ResourceLoad/GetSchema?",
                        add_headers(apikey = apikey, Accept = "application/json",
                                    `Content-Type` = "application/json"))
}

getAppSchema <- function(apikey) {
  response <- httr::GET("https://test-resourcebrowser.azurewebsites.net/api/v1/ApplicationLoad/GetSchema",
                        add_headers(apikey = apikey, Accept = "application/json",
                                    `Content-Type` = "application/json"))
}

getResourceTestSubmission <- function(apikey) {
  response <- httr::GET("https://test-resourcebrowser.azurewebsites.net/api/v1/ResourceLoad/GetTestSubmission",
                        add_headers(apikey = apikey, Accept = "application/json",
                                    `Content-Type` = "application/json"))
}

# Note: not working
getApplicationTestSubmission <- function(apikey) {
 response <- httr::GET("https://test-resourcebrowser.azurewebsites.net/api/v1/ApplicationLoad/GetTestSubmission",
                       add_headers(apikey = apikey, Accept = "application/json",
                                   `Content-Type` = "application/json"))
}

postValidateResource <- function(apikey, body = NULL) {
  httr::POST("https://test-resourcebrowser.azurewebsites.net/api/v1/ApplicationLoad/Validate",
             add_headers(apikey = apikey, Accept = "application/json",
                         `Content-Type` = "application/json"),
             body = body)
}

postLoadResource <- function(apikey, body = NULL) {
  httr::POST("https://test-resourcebrowser.azurewebsites.net/api/v1/ApplicationLoad/Load",
             add_headers(apikey = apikey, Accept = "application/json",
                         `Content-Type` = "application/json"),
             body = body)
}

getCodedValues <- function(apikey) {
  httr::GET("https://test-resourcebrowser.azurewebsites.net/api/v1/ResourceLoad/GetCodedValues",
            add_headers(apikey = apikey, Accept = "application/json",
                        `Content-Type` = "application/json"))
}
