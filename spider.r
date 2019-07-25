# Library
library(fmsb)
require(tikzDevice)
# Start the tikz device
tikz('phi_plot.tex',
          standAlone = TRUE, # We want a tex file that can be directly compiled to a dvi
               width = 6, height = 6,
               packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts}"))

# Create data: note in High school for Jonathan:
data=as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,10) , rep(0,10) , data)

# The default radar chart proposed by the library:
#radarchart(data)

# Custom the radarChart !
radarchart( data, axistype=1,

           # custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,

           # custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,

           # custom labels
           vlcex=0.8)

dev.off()
# Convert the latex file to a pdf
tools::texi2pdf('phi_plot.tex',quiet=FALSE)
