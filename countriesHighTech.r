library(ggplot2)
library(reshape)
library(RColorBrewer)
exports.melt.frame<-read.csv("/Users/sciruela/Documents/countriesHighTech/data.csv")
exports.melt.frame<-melt(exports.melt.frame,id=c("Country.Name","Country.Code"))
colnames(exports.melt.frame)[3]<-"year"
exports.melt.frame$year<-substr(exports.melt.frame$year,2,5)
transform(exports.melt.frame,year=as.numeric(year))
pal2 <- brewer.pal(8,"Set2")
pdf("/Users/sciruela/Documents/countriesHighTech/exports.pdf")
ggplot(exports.melt.frame, aes(x=year, y=value, group=year), na.rm = TRUE) +
  geom_boxplot(na.rm = TRUE) +
  geom_point(data=subset(exports.melt.frame, Country.Name == "Switzerland"),
    aes(x=year, y=value, color="Switzerland")) +
  geom_point(data=subset(exports.melt.frame, Country.Name == "Austria"),
    aes(x=year, y=value, color="Austria")) +
  geom_point(data=subset(exports.melt.frame, Country.Name == "Germany"),
    aes(x=year, y=value, color="Germany")) +
  geom_point(data=subset(exports.melt.frame, Country.Name == "Spain"),
    aes(x=year, y=value, color="Spain")) +
  geom_point(data=subset(exports.melt.frame, Country.Name == "United Kingdom"),
    aes(x=year, y=value, color="United Kingdom")) +
  geom_point(data=subset(exports.melt.frame, Country.Name == "United States"),
    aes(x=year, y=value, color="United States")) +
  
  geom_line(data=subset(exports.melt.frame, Country.Name == "Switzerland"),
    aes(x=year, y=value, group=1, color="Switzerland"), size=1.1) +
  geom_line(data=subset(exports.melt.frame, Country.Name == "Austria"),
    aes(x=year, y=value, group=1, color="Austria"), size=1.1) +
  geom_line(data=subset(exports.melt.frame, Country.Name == "Germany"),
    aes(x=year, y=value, color="Germany", group=1), size=1.1) +
  geom_line(data=subset(exports.melt.frame, Country.Name == "Spain"),
    aes(x=year, y=value, color="Spain", group=1), size=1.1) +
  geom_line(data=subset(exports.melt.frame, Country.Name == "United Kingdom"),
    aes(x=year, y=value, color="United Kingdom", group=1), size=1.1) +
  geom_line(data=subset(exports.melt.frame, Country.Name == "United States"),
    aes(x=year, y=value, color="United States", group=1), size=1.1) +
  
  scale_colour_manual(values=pal2) +
  
  ylab("Exports Which are High Tech") +
  xlab("Year") +
  
  opts(title="Country's Exports which are High Tech",
    legend.title = theme_blank(),
    panel.background = theme_blank(),
    axis.text.x=theme_text(angle=45, hjust=1))
dev.off()


investments.melt.frame<-read.csv("/Users/sciruela/Documents/countriesHighTech/data2.csv")
investments.melt.frame<-melt(investments.melt.frame,id=c("Country.Name","Country.Code"))
colnames(investments.melt.frame)[3]<-"year"
investments.melt.frame$year<-substr(investments.melt.frame$year,2,5)
transform(investments.melt.frame,year=as.numeric(year))
pal2 <- brewer.pal(8,"Set2")
pdf("/Users/sciruela/Documents/countriesHighTech/investments.pdf")
ggplot(investments.melt.frame, aes(x=year, y=value, group=year), na.rm = TRUE) +
  geom_boxplot(na.rm = TRUE) +
  geom_point(data=subset(investments.melt.frame, Country.Name == "Switzerland"),
    aes(x=year, y=value, color="Switzerland")) +
  geom_point(data=subset(investments.melt.frame, Country.Name == "Austria"),
    aes(x=year, y=value, color="Austria")) +
  geom_point(data=subset(investments.melt.frame, Country.Name == "Germany"),
    aes(x=year, y=value, color="Germany")) +
  geom_point(data=subset(investments.melt.frame, Country.Name == "Spain"),
    aes(x=year, y=value, color="Spain")) +
  geom_point(data=subset(investments.melt.frame, Country.Name == "United Kingdom"),
    aes(x=year, y=value, color="United Kingdom")) +
  geom_point(data=subset(investments.melt.frame, Country.Name == "United States"),
    aes(x=year, y=value, color="United States")) +
  
  geom_line(data=subset(investments.melt.frame, Country.Name == "Switzerland"),
    aes(x=year, y=value, group=1, color="Switzerland"), size=1.1) +
  geom_line(data=subset(investments.melt.frame, Country.Name == "Austria"),
    aes(x=year, y=value, group=1, color="Austria"), size=1.1) +
  geom_line(data=subset(investments.melt.frame, Country.Name == "Germany"),
    aes(x=year, y=value, color="Germany", group=1), size=1.1) +
  geom_line(data=subset(investments.melt.frame, Country.Name == "Spain"),
    aes(x=year, y=value, color="Spain", group=1), size=1.1) +
  geom_line(data=subset(investments.melt.frame, Country.Name == "United Kingdom"),
    aes(x=year, y=value, color="United Kingdom", group=1), size=1.1) +
  geom_line(data=subset(investments.melt.frame, Country.Name == "United States"),
    aes(x=year, y=value, color="United States", group=1), size=1.1) +
  
  scale_colour_manual(values=pal2) +
  
  ylab("Foreign Investments") +
  xlab("Year") +
  
  opts(title="Foreign Investments by Country",
    legend.title = theme_blank(),
    panel.background = theme_blank(),
    axis.text.x=theme_text(angle=45, hjust=1))
dev.off()



unemployment.melt.frame<-read.csv("/Users/sciruela/Documents/countriesHighTech/data3.csv")
unemployment.melt.frame<-melt(unemployment.melt.frame,id=c("Country.Name","Country.Code"))
colnames(unemployment.melt.frame)[3]<-"year"
unemployment.melt.frame$year<-substr(unemployment.melt.frame$year,2,5)
transform(unemployment.melt.frame,year=as.numeric(year))
pal2 <- brewer.pal(8,"Set2")
pdf("/Users/sciruela/Documents/countriesHighTech/unemployment.pdf")
ggplot(unemployment.melt.frame, aes(x=year, y=value, group=year), na.rm = TRUE) +
  geom_boxplot(na.rm = TRUE) +
  geom_point(data=subset(unemployment.melt.frame, Country.Name == "Switzerland"),
    aes(x=year, y=value, color="Switzerland")) +
  geom_point(data=subset(unemployment.melt.frame, Country.Name == "Austria"),
    aes(x=year, y=value, color="Austria")) +
  geom_point(data=subset(unemployment.melt.frame, Country.Name == "Germany"),
    aes(x=year, y=value, color="Germany")) +
  geom_point(data=subset(unemployment.melt.frame, Country.Name == "Spain"),
    aes(x=year, y=value, color="Spain")) +
  geom_point(data=subset(unemployment.melt.frame, Country.Name == "United Kingdom"),
    aes(x=year, y=value, color="United Kingdom")) +
  geom_point(data=subset(unemployment.melt.frame, Country.Name == "United States"),
    aes(x=year, y=value, color="United States")) +
  
  geom_line(data=subset(unemployment.melt.frame, Country.Name == "Switzerland"),
    aes(x=year, y=value, group=1, color="Switzerland"), size=1.1) +
  geom_line(data=subset(unemployment.melt.frame, Country.Name == "Austria"),
    aes(x=year, y=value, group=1, color="Austria"), size=1.1) +
  geom_line(data=subset(unemployment.melt.frame, Country.Name == "Germany"),
    aes(x=year, y=value, color="Germany", group=1), size=1.1) +
  geom_line(data=subset(unemployment.melt.frame, Country.Name == "Spain"),
    aes(x=year, y=value, color="Spain", group=1), size=1.1) +
  geom_line(data=subset(unemployment.melt.frame, Country.Name == "United Kingdom"),
    aes(x=year, y=value, color="United Kingdom", group=1), size=1.1) +
  geom_line(data=subset(unemployment.melt.frame, Country.Name == "United States"),
    aes(x=year, y=value, color="United States", group=1), size=1.1) +
  
  scale_colour_manual(values=pal2) +
  
  ylab("% Total Unemployment") +
  xlab("Year") +
  
  opts(title="% Total unemployment by Country",
    legend.title = theme_blank(),
    panel.background = theme_blank(),
    axis.text.x=theme_text(angle=45, hjust=1))
dev.off()

