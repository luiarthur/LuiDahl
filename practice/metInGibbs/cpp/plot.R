out <- read.table("cout.txt",header=F)

pdf("plot.pdf")
  plot(density(out[,1]))
dev.off()  
