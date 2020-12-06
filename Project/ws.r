library(jsonlite)
library(httpuv)


launch_server = function(onWSReadyCallback, host = '127.0.0.1', port = 5000) {
  s <- startServer(host = "127.0.0.1", port = 5000,
  list(
    onWSOpen = function(ws) {
      cat('onWSOpen')
      onWSReadyCallback(ws)
      cat("Server connection opened.\n")
      ws$onMessage(function(binary, message) {
        cat("Server received message:", message, "\n")
      })
      ws$onClose(function() {
        cat("Server connection closed.\n")
      })
    }
  )
  )
}