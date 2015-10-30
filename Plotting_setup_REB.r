### MY OWN THEME TO SAVE LINES OF CODE BELOW
theme_boxplot <- function(base_size = 12){
 theme_bw(base_size) %+replace%
   theme(legend.key.size=unit(15,"points"),
         legend.text=element_text(size=I(13)),
         legend.key=element_blank(),
         legend.title=element_blank(),
         legend.position="none",
         plot.margin=unit(c(0.25,2,0.25,2), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
         panel.border=element_rect(colour='black', fill = NA),
         panel.margin=unit(0,"lines"),
         axis.ticks.length=unit(1,"mm"),
         axis.ticks.margin = unit(0, "lines"),
         axis.text=element_text(size=15),
         axis.title.x=element_text(hjust=.55, vjust=-.01, size=17),
         axis.title.y=element_text(size=17, angle=90, hjust=.56, vjust=-.001),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         strip.text.x=element_text(size=14),
         strip.background=element_rect(colour='black', fill='white'))
}  
#########################################
