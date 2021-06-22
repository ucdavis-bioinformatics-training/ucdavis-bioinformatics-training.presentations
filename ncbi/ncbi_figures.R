##### GENBANK
tb <- read.table("genbank_data.txt",header=T,as.is=T)

tb$Date2 = as.Date(paste("1-",tolower(tb$Date),sep=""),"%d-%b-%y")

pdf("GenBank.SRA.data.pdf",width=10,height=5,pointsize=10)
par(mfrow=c(1,2))
plot(x=tb$Date2,y=tb$GenBank.bases,log="y", ylim=c(10^6,10^12), type='l', col=c("blue"), yaxt="n",xaxt = "n",ylab="Bases",xlab="Year", main="GenBank")
aty <- axTicks(2)
lines(x=tb$Date2,y=tb$WGS.bases,type='l',col=c("red"))

axis.Date(side = 1,tb$Date2, format = "%Y",cex.axis=0.8,tcl=-1.0)
labels <- sapply(log10(aty),function(i)
  as.expression(bquote(10^ .(i))))
axis(2,at=aty,labels=labels)
#grid(nx=7, ny=3,lty=1)
legend("bottomright",c("GenBank","WGS"),pch=21,pt.bg="white",lty=1,col=c("blue","red"))
#dev.off()

sra <- read.table("sra_stat.csv",header=T,as.is=T,sep=",")
sra$date2 <- as.Date(sra$date,"%m/%d/%Y")

pdf("sra.data.pdf",width=10,height=5,pointsize=10)

sra <- read.table("sra_stat.csv",header=T,as.is=T,sep=",")
sra$date2 <- as.Date(sra$date,"%m/%d/%Y")

########## SRA Figure
pdf("SRA.data.pdf",width=6,height=5,pointsize=10)

plot(x=sra$date2,y=sra$bases,log="y", type='l', col=c("blue"), yaxt="n",xaxt = "n",ylab="",xlab="Year", main= "Growth of Sequence Read Archive over time")
aty <- axTicks(2)
lines(x=sra$date2,y=sra$open_access_bases,type='l',col=c("red"))

axis.Date(side = 1, sra$date2, format = "%Y",cex.axis=0.8,tcl=-1.0)
labels <- sapply(log10(aty),function(i)
  as.expression(bquote(10^ .(i)))
)
axis(2,at=aty,labels=labels)
legend("bottomright",c("Bases", "Open access bases"),pch=21,pt.bg="white",lty=1,col=c("blue","red"))

dev.off()


################ Gold DB
gold <- read.table("goldData.txt",sep="\t",header=T,as.is=T, quote="",comment.char="")


##### Sequencing Costs
costs <- read.table("Sequencing_Cost_Data_Table_Aug2020.txt",sep="\t",header=T,as.is=T)

costs$Date2 = as.Date(paste("1-",tolower(costs$Date),sep=""),"%d-%b-%y")

options(scipen=20)
pdf("costs.genome.pdf",width=10,height=5,pointsize=10)
par(mfrow=c(1,2))
plot(x=costs$Date2,y=as.numeric(gsub("\\$|,","",costs$Cost.per.Mb)),log="y", type='l', col=c("blue"), yaxt="n",xaxt = "n",ylab="Dollars",xlab="Year",main="Cost per Megabase of Sequence")
aty <- axTicks(2)

axis.Date(side = 1, costs$Date2, format = "%Y",cex.axis=0.8,tcl=-1.0)
labels = paste("$",aty,sep="")
axis(2,at=aty,labels=labels,las=2)
#grid(lty=1)

plot(x=costs$Date2,y=as.numeric(gsub("\\$|,","",costs$Cost.per.Genome)),log="y", type='l', col=c("blue"), yaxt="n",xaxt = "n",ylab="",xlab="Year",main="Cost per Human Sized Genome @ 30x (Illumina)")
aty <- axTicks(2)

axis.Date(side = 1, costs$Date2, format = "%Y",cex.axis=0.8,tcl=-1.0)
labels = paste("$",aty,sep="")
axis(2,at=aty,labels=labels,las=2,cex=0.5)
#grid(lty=1)
dev.off()


