#' A Function to make a barplot
#'
#' This function creates a barplot with  rotated x labels 
#' @param data the data frame holding the data to plot
#' @param column_to_plot is the column name to plot
#' @param labels_vec is a vector of label names to identify each bar
#' @param rot_angle the angle to rotate the x-axis labels 
#' @param pcol is the plottin color
#' @param pmain is the plotting main title
#' @param pylim  is the plotting y limits
#' @keywords correlation analysis among factors
#' @export
#' @examples
#' moose_barplot()
moose_barplot <- function(data, column_to_plot, labels_vec, rot_angle, pcol = "steelblue", pmain = "", pylim = c(0,1)) {
  plt <- barplot(data[[column_to_plot]], col=pcol, xaxt="n", ylim = pylim, main = pmain)
  abline(h = seq(0.1, 1, by = 0.1), lty = 2, col = "grey50" )
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=1.2) 
}

