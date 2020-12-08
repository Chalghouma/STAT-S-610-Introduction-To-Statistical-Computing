source('Project/source_paths.r')
source(get_web_sockets_server_file_path())
source(get_dynamic_plotter_file_path())
source(get_static_plotter_file_path())


onWSReadyCallback <- function(ws) {
  print('onWSReady')
  onGraphProcessedCallback <- function(year_interval, data) {
    print('onGraphProcessedCallback')
    ws$send(serializeJSON(list(year_interval = year_interval, data = data)))
  }
}

# postProcessedCallback <-  onGraphProcessedCallback
postProcessedCallback <- function(X, Y) { }

# launch_server(onWSReadyCallback)
# plot_static_figure_9(postProcessedCallback)
# plot_figure_10(function(X,Y){})

plot_dynamic_figure_10(postProcessedCallback)