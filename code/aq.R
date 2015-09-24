setwd("C:/github/AirQual/data")
lapply(c("dplyr", "data.table", "lubridate", "grid", "ggplot2", "reshape2"), library, character.only = TRUE)
rbindlist(lapply(list.files("Fairbanks", full=T), read.csv))[, c(1,4), with=F] %>% setnames(c("Date", "Val")) %>%
    mutate(Loc="Fbks-Baseline", DateTime=parse_date_time(Date, "%m%d%y"), Date=parse_date_time(Date, "%m%d%y")) -> d
    
read.csv("Fairbanks2/FNSB_AQ_NCore_2015_mean.csv")[,c(3,10)] %>% data.table() %>% setnames(c("Date", "Val")) %>%
    mutate(Loc="Fbks-New", DateTime=parse_date_time(Date, "%m%d%y"), Date=parse_date_time(Date, "%m%d%y")) %>% rbind(d) -> d
    
read.csv("Beijing/Beijin_25particules_USunits.csv")[2:3] %>% data.table() %>% setnames(c("Date", "Val")) %>%
    mutate(Loc="Beijing", DateTime=parse_date_time(Date, "%m%d%y %I:%M p"), Date=parse_date_time(substr(Date, 1,8), "%m%d%y")) %>% rbind(d) %>%
    mutate(Year=year(Date), Month=month(Date, label=T, abbr=T), Mday=mday(Date), Yday=yday(Date)) %>%
    setcolorder(c("Loc", "DateTime", "Date", "Year", "Month", "Mday", "Yday", "Val")) %>% setorder(Loc, DateTime) %>%
    group_by(Loc, Date, Year, Month, Mday, Yday) %>% summarise(Val=mean(Val)) -> d

d2 <- filter(d, Loc!="Beijing" & Year==2015 & as.integer(Month) < 4)
d2 %>% dcast(Date ~ Loc, value.var="Val") %>% data.table() -> d3
lm1 <- lm(d3$"Fbks-Baseline" ~ d3$"Fbks-New" - 1)
d %>% filter(Loc=="Fbks-New") %>% mutate(Loc="Fbks-New-Adjusted", Val=lm1$coefficients[1]*Val) %>% rbind(d) -> d

lm_eqn = function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
      b = format(abs(coef(m)[1]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3));
  if (coef(m)[1] >= 0)  { # was index 2 when intercept present
    eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}

e <- element_blank()
transparent_theme <- theme(axis.title.x=e, axis.title.y=e, axis.text.x=e, axis.text.y=e, axis.ticks=e, panel.grid=e, axis.line=e,
 panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA))
cbpal <- c("#00000075", "#D55E00", "#0072B2", "#CC79A7")

png("../plots/aq.png", height=2800, width=4800, res=300)
vp1 <- viewport(width=0.325, height=0.525, x=0.315, y=0.65)
vp2 <- viewport(width=0.15, height=0.25, x=0.4, y=0.775)
ggplot(data=d, aes(x=Date, y=Val, colour=Loc)) +
    geom_point(size=2) + theme_bw(base_size=16) + scale_colour_manual(name="",values=c(cbpal[1], paste0(cbpal[2:3], 75), cbpal[4])) +
    theme(legend.position="bottom", legend.box="horizontal") + guides(colour=guide_legend(override.aes=list(alpha=1))) + 
    labs(x="", y=expression("Mean daily PM 2.5 ("~mu*g/m^3~")"), title="Fairbanks NSB air quality compared to Beijing, China") +
    geom_line(data=d %>% filter(Loc!="Beijing")) + scale_x_datetime(breaks="1 years") +
    geom_rect(data=data.frame(xmn=as.POSIXct("2015-01-01"), xmx=as.POSIXct("2015-03-31"), ymn=-5, ymx=90),
        aes(x=NULL, y=NULL, xmin=xmn, xmax=xmx, ymin=ymn, ymax=ymx), fill="transparent", colour="black", size=1)

g1 <- ggplot(data=d2, aes(x=Date, y=Val, colour=Loc)) + geom_point(size=3) + geom_smooth(size=1) + geom_line(size=1) +
    theme_bw(base_size=16) + scale_colour_manual(name="",values=cbpal[2:3]) + theme(legend.position="none") + labs(x="", y="", title="")
g2 <- ggplot(data=d3, aes(x=`Fbks-Baseline`, y=`Fbks-New`)) + geom_point(size=3) +
    geom_smooth(method="lm", se=FALSE, color="black", formula=y~x, size=1) +
    geom_text(aes(x=40, y=65, label=lm_eqn(lm1)), parse=TRUE, size=4) +
    transparent_theme + theme(legend.position="none") + labs(x="", y="", title="")
print(g1, vp=vp1)
print(g2, vp=vp2)
dev.off()
