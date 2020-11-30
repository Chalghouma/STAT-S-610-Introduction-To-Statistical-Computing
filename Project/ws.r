# install.packages('jsonlite')
library(jsonlite)
# install.packages('httpuv')
library(httpuv)


s <- startServer(host = "127.0.0.1", port = 5000,
  list(
    onWSOpen = function(ws) {
      # The ws object is a WebSocket object
      cat("Server connection opened.\n")

      ws$onMessage(function(binary, message) {
        cat("Server received message:", message, "\n")
        ws$send(message)
        ws$send(serializeJSON(c(1, 2, 3, 4, 5)))
        ws$send(serializeJSON(list(aString='something',bMatrix=matrix(c(1,2,3,4),nrow = 2))))
        # ws$send(serialize( c(1,2,3,4,5)))
      })
      ws$onClose(function() {
        cat("Server connection closed.\n")
      })
    }
  )
)