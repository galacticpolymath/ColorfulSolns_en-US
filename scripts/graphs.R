require(dplyr);require(ggplot2);require(galacticEdTools);require(galacticPubs)

get_base_10_breaks<-function(min_val,max_val,n_breaks){
  log_min<-log10(min_val)
  log_max<-log10(max_val)
  incr<-(log_max-log_min)/n_breaks
  c(min_val,sapply(1:n_breaks,function(i){10^(log_min+i*incr)}))
}

rev_s_curve <- function(x,shape,height,shift,minval) {minval+height*exp(shape*(x-shift))/(minval+exp(shape*(x-shift)))}

x<-get_base_10_breaks(min_val=1,max_val=1e6,n_breaks=100)

#Mauveine
d_synth <- tibble(units=x,price=rev_s_curve(x=x,shape=-.0005,height=9.8e5,shift=1e3,minval=1.50),source_material="Fossil Fuels")
d_tyrian <- tibble(units=x,price=rev_s_curve(x,shape=-.1,height=5e2,shift=20,minval=40),source_material="Murex snails")
d<-rbind(d_synth)#,d_tyrian)
d %>% ggplot(aes(units,price,color=source_material)) +
 #geom_smooth(formula="y~x",method="loess",se = FALSE,span=.5)+
  geom_point()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x,n=6),
                labels = scales::label_comma(accuracy = 1),
                limits=c(1,NA),expand=expansion(c(0.05,0.3)))+
  scale_y_log10(n.breaks=6,labels=scales::label_dollar(),limits=c(0.1,NA),expand=expansion(c(0.02,0.2)))

#Just graph...abandoning trying to actually figure out the formulas :/
ggplot(d)+geom_point(aes(x=units,y=price),color="transparent")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x,n=6),
                labels = scales::label_comma(accuracy = 1),
                limits=c(1,NA),expand=expansion(c(0.05,0.08)))+
  scale_y_log10(n.breaks=7,labels=scales::label_dollar(accuracy=1),limits=c(0.1,NA),expand=expansion(c(0.02,0.09)))+
  theme_galactic(text.cex=1.5)
gpsave("price~units_grid (no x-padding).png")

