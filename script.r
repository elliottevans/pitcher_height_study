setwd("~/pitcher_height_study")
pitching<-read.csv("lahman_2015/Pitching.csv",header=TRUE)
salaries<-read.csv("lahman_2015/Salaries.csv",header=TRUE)
master<-read.csv("lahman_2015/Master.csv",header=TRUE)
pitching_2014_war<-read.csv("pitching_2014_war.csv",header=TRUE)
pitching_2013_war<-read.csv("pitching_2013_war.csv",header=TRUE)
pitching_2012_war<-read.csv("pitching_2012_war.csv",header=TRUE)
pitching_2011_war<-read.csv("pitching_2011_war.csv",header=TRUE)
pitching_2010_war<-read.csv("pitching_2010_war.csv",header=TRUE)

fip_constant_2010<-3.079
fip_constant_2011<-3.025
fip_constant_2012<-3.095
fip_constant_2013<-3.048
fip_constant_2014<-3.132

#We examine performance and salary data for 2014 pitchers

#Firstly, we format the data
pitching_2010_war$Name<-strsplit(as.character(pitching_2010_war$Name),"*",fixed=TRUE)
pitching_2011_war$Name<-strsplit(as.character(pitching_2011_war$Name),"*",fixed=TRUE)
pitching_2012_war$Name<-strsplit(as.character(pitching_2012_war$Name),"*",fixed=TRUE)
pitching_2013_war$Name<-strsplit(as.character(pitching_2013_war$Name),"*",fixed=TRUE)
pitching_2014_war$Name<-strsplit(as.character(pitching_2014_war$Name),"*",fixed=TRUE)
pitching_2010_war<-cbind(pitching_2010_war,rep(2010,dim(pitching_2010_war)[1]))
pitching_2011_war<-cbind(pitching_2011_war,rep(2011,dim(pitching_2011_war)[1]))
pitching_2012_war<-cbind(pitching_2012_war,rep(2012,dim(pitching_2012_war)[1]))
pitching_2013_war<-cbind(pitching_2013_war,rep(2013,dim(pitching_2013_war)[1]))
pitching_2014_war<-cbind(pitching_2014_war,rep(2014,dim(pitching_2014_war)[1]))
names(pitching_2010_war)[25]<-"yearID"
names(pitching_2011_war)[25]<-"yearID"
names(pitching_2012_war)[25]<-"yearID"
names(pitching_2013_war)[25]<-"yearID"
names(pitching_2014_war)[25]<-"yearID"

pitching_war<-rbind(pitching_2010_war,pitching_2011_war,pitching_2012_war,pitching_2013_war,pitching_2014_war)


pitchers<-merge(pitching,master,by="playerID")
pitchers<-pitchers[pitchers$yearID>=2010,]

#HERE WE ARE USING 2010-2014 pitching seasons

short_pitchers<-cbind((pitchers[pitchers$height<=71,])[,c("playerID","nameFirst","nameLast","ERA","SO","BB","HR","height","yearID","HBP")],
                      (pitchers[pitchers$height<=71,])[,c("nameFirst","nameLast","IPouts","ERA","SO","BB","HR","height","yearID","HBP")]$IPouts/3)
names(short_pitchers)[11]<-"IP"
tall_pitchers<-cbind((pitchers[pitchers$height>71,])[,c("playerID","nameFirst","nameLast","ERA","SO","BB","HR","height","yearID","HBP")],
                     (pitchers[pitchers$height>71,])[,c("nameFirst","nameLast","IPouts","ERA","SO","BB","HR","height","yearID","HBP")]$IPouts/3)
names(tall_pitchers)[11]<-"IP"

#Calclate rate stats
short_pitchers$SO<-9*short_pitchers$SO/short_pitchers$IP
names(short_pitchers)[5]<-"SO_per_nine"
short_pitchers$BB<-9*short_pitchers$BB/short_pitchers$IP
names(short_pitchers)[6]<-"BB_per_nine"
short_pitchers$HR<-9*short_pitchers$HR/short_pitchers$IP
names(short_pitchers)[7]<-"HR_per_nine"

tall_pitchers$SO<-9*tall_pitchers$SO/tall_pitchers$IP
names(tall_pitchers)[5]<-"SO_per_nine"
tall_pitchers$BB<-9*tall_pitchers$BB/tall_pitchers$IP
names(tall_pitchers)[6]<-"BB_per_nine"
tall_pitchers$HR<-9*tall_pitchers$HR/tall_pitchers$IP
names(tall_pitchers)[7]<-"HR_per_nine"

short_pitchers<-merge(short_pitchers,salaries,by=c("playerID","yearID"))
tall_pitchers<-merge(tall_pitchers,salaries,by=c("playerID","yearID"))

short_pitchers<-cbind(short_pitchers,paste(short_pitchers$nameFirst,short_pitchers$nameLast,sep=" "))
tall_pitchers<-cbind(tall_pitchers,paste(tall_pitchers$nameFirst,tall_pitchers$nameLast,sep=" "))

names(short_pitchers)[15]<-"Name"
names(tall_pitchers)[15]<-"Name"

short_pitchers<-merge(short_pitchers,pitching_war,by=c("Name","yearID"))
tall_pitchers<-merge(tall_pitchers,pitching_war,by=c("Name","yearID"))

short_fips<-c()
for(i in 1:dim(short_pitchers)[1]){
  if(short_pitchers$yearID==2010){
    short_fips<-append(short_fips,13*short_pitchers$HR_per_nine[i]/9 +
                         3*short_pitchers$BB_per_nine[i]/9 + 3*short_pitchers$HBP[i]/short_pitchers$IP.x[i] -
                         2*short_pitchers$SO_per_nine[i]/9 + fip_constant_2010)  
  }
  if(short_pitchers$yearID==2011){
    short_fips<-append(short_fips,13*short_pitchers$HR_per_nine[i]/9 +
                         3*short_pitchers$BB_per_nine[i]/9 + 3*short_pitchers$HBP[i]/short_pitchers$IP.x[i] -
                         2*short_pitchers$SO_per_nine[i]/9 + fip_constant_2011)  
  }
  if(short_pitchers$yearID==2012){
    short_fips<-append(short_fips,13*short_pitchers$HR_per_nine[i]/9 +
                         3*short_pitchers$BB_per_nine[i]/9 + 3*short_pitchers$HBP[i]/short_pitchers$IP.x[i] -
                         2*short_pitchers$SO_per_nine[i]/9 + fip_constant_2012)  
  }
  if(short_pitchers$yearID==2013){
    short_fips<-append(short_fips,13*short_pitchers$HR_per_nine[i]/9 +
                   3*short_pitchers$BB_per_nine[i]/9 + 3*short_pitchers$HBP[i]/short_pitchers$IP.x[i] -
                   2*short_pitchers$SO_per_nine[i]/9 + fip_constant_2013)  
  }
  if(short_pitchers$yearID==2014){
    short_fips<-append(short_fips,13*short_pitchers$HR_per_nine[i]/9 +
                   3*short_pitchers$BB_per_nine[i]/9 + 3*short_pitchers$HBP[i]/short_pitchers$IP.x[i] -
                   2*short_pitchers$SO_per_nine[i]/9 + fip_constant_2014)  
  }  
}

tall_fips<-c()
for(i in 1:dim(tall_pitchers)[1]){
  if(tall_pitchers$yearID==2010){
    tall_fips<-append(tall_fips,13*tall_pitchers$HR_per_nine[i]/9 +
                        3*tall_pitchers$BB_per_nine[i]/9 + 3*tall_pitchers$HBP[i]/tall_pitchers$IP.x[i] -
                        2*tall_pitchers$SO_per_nine[i]/9 + fip_constant_2010)  
  }
  if(tall_pitchers$yearID==2011){
    tall_fips<-append(tall_fips,13*tall_pitchers$HR_per_nine[i]/9 +
                        3*tall_pitchers$BB_per_nine[i]/9 + 3*tall_pitchers$HBP[i]/tall_pitchers$IP.x[i] -
                        2*tall_pitchers$SO_per_nine[i]/9 + fip_constant_2011)  
  }
  if(tall_pitchers$yearID==2012){
    tall_fips<-append(tall_fips,13*tall_pitchers$HR_per_nine[i]/9 +
                        3*tall_pitchers$BB_per_nine[i]/9 + 3*tall_pitchers$HBP[i]/tall_pitchers$IP.x[i] -
                        2*tall_pitchers$SO_per_nine[i]/9 + fip_constant_2012)  
  }
  if(tall_pitchers$yearID==2013){
    tall_fips<-append(tall_fips,13*tall_pitchers$HR_per_nine[i]/9 +
                   3*tall_pitchers$BB_per_nine[i]/9 + 3*tall_pitchers$HBP[i]/tall_pitchers$IP.x[i] -
                   2*tall_pitchers$SO_per_nine[i]/9 + fip_constant_2013)  
  }
  if(tall_pitchers$yearID==2014){
    tall_fips<-append(tall_fips,13*tall_pitchers$HR_per_nine[i]/9 +
                   3*tall_pitchers$BB_per_nine[i]/9 + 3*tall_pitchers$HBP[i]/tall_pitchers$IP.x[i] -
                   2*tall_pitchers$SO_per_nine[i]/9 + fip_constant_2014)  
  }  
}

short_pitchers<-cbind(short_pitchers,short_fips)
names(short_pitchers)[39]<-"FIP"

tall_pitchers<-cbind(tall_pitchers,tall_fips)
names(tall_pitchers)[39]<-"FIP"

short_pitchers<-short_pitchers[,c("Name","yearID","Age","height","teamID","G","GS","IP.x","SO_per_nine",
                                  "BB_per_nine","HR_per_nine","FIP","WAR","ERA","salary","Acquired")]
tall_pitchers<-tall_pitchers[,c("Name","yearID","Age","height","teamID","G","GS","IP.x","SO_per_nine",
                                  "BB_per_nine","HR_per_nine","FIP","WAR","ERA","salary","Acquired")]

short_relief_pitchers<-short_pitchers[short_pitchers$IP.x<90,]
short_relief_pitchers<-short_relief_pitchers[as.numeric(as.character(short_relief_pitchers$GS)) <=5,]
tall_relief_pitchers<-tall_pitchers[tall_pitchers$IP.x<90,]
tall_relief_pitchers<-tall_relief_pitchers[as.numeric(as.character(tall_relief_pitchers$GS)) <=5,]

short_starting_pitchers<-short_pitchers[short_pitchers$IP.x>=150,]
tall_starting_pitchers<-tall_pitchers[tall_pitchers$IP.x>=150,]

png("pics/value_reliever_plot.png", width=6, height=4, units="in", res=200)
getOption("scipen")
opt <- options("scipen" = 20)
getOption("scipen")
plot(tall_relief_pitchers[tall_relief_pitchers$Acquired=="Free Agency",]$salary,
     as.numeric(as.character(tall_relief_pitchers[tall_relief_pitchers$Acquired=="Free Agency",]$WAR)),
     xlab="Salary in Free Agency",ylab="WAR",pch=19)
points(short_relief_pitchers[short_relief_pitchers$Acquired=="Free Agency",]$salary,
       as.numeric(as.character(short_relief_pitchers[short_relief_pitchers$Acquired=="Free Agency",]$WAR)),
       col=2,pch=3)
legend(12350000,-.2,c("short","tall"),col=c(2,1),pch=c(3,19))
grid()


options(opt)


dev.off()


#this is the WAR value of a tall pitcher per $10million that he is being paid in free agency
value_reliever_tall<-mean(as.numeric(as.character(tall_relief_pitchers[tall_relief_pitchers$Acquired=="Free Agency",]$WAR)))/
  mean(tall_relief_pitchers[tall_relief_pitchers$Acquired=="Free Agency",]$salary) * 10000000

#this is the WAR value of a short pitcher per $10million that he is being paid in free agency
value_reliever_short<-mean(as.numeric(as.character(short_relief_pitchers[short_relief_pitchers$Acquired=="Free Agency",]$WAR)))/
  mean(short_relief_pitchers[short_relief_pitchers$Acquired=="Free Agency",]$salary) * 10000000
