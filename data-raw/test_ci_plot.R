p <- ggplot(test, aes(x=Age_stop,y=cum_estimate)) +
  geom_line(size=1) + geom_point(size=3) +
  theme_bw() +
  geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit), data = test2, width = 0.7, size = 0.9) +
  scale_x_continuous(breaks=seq(25,75,5),limits=c(25,75)) +
  scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
  scale_color_brewer(palette='Dark2') +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(colour = 'black', size = 0.5, linetype = 'dashed'),
        panel.border = element_rect(size=1, colour = "black"),
        axis.text.x=element_text(family="Helvetica",size=14), 
        axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5), 
        axis.text.y=element_text(family="Helvetica",size=14),
        axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5), 
        plot.margin = (unit(c(2, 0, 2, 2), "cm")), 
        plot.title = element_text(family="Helvetica",size=14,vjust=2,hjust=0.5),
        legend.text = element_blank()) +
  #legend.text=element_text(family="Helvetica",size=14)) +
  ylab('Cumulative risk of subsequent cancer (%)')