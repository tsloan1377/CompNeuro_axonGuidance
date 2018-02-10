## Script to red xls files outputted from Netlogo's behaviourspace tool

# Clear all
rm(list=ls());

setwd("~/My Dropbox/Work-home/Computational neuroscience/Project")
fileName1 = "cue1_2E_1I_2RR_4xCue_1000reps_csv.csv"
fileName2 = "cue2_2E_1I_2RR_4xCue_1000reps_csv.csv"
path <- "C:/Users/sloant/Documents/My Dropbox/Work-home/Computational neuroscience/Project" # At home
path=paste(path,fileName1, sep = "/")
# setwd(path)

# Method 1 - fails because can't find perl
# library(gdata)                   # load gdata package 
# mydata = read.xls(fileName1)  # read from first sheet 

# Method 2 - Error: Your InputStream was neither an OLE2 stream, nor an OOXML stream
# library(XLConnect)               # load XLConnect package 
# wk = loadWorkbook(path) 
# df = readWorksheet(wk, sheet="Sheet1") 

# res <- read.xls(path, sheetName="Sheet1")

data1 =read.csv(fileName1,sep = ",",stringsAsFactors=FALSE) # Use "," for long spreadsheets outputted from Word

data2 =read.csv(fileName2,sep = ",",stringsAsFactors=FALSE)
# data3 =read.csv(fileName3,sep = "\t")


# test=as.numeric(data1[17:117,5])
numLevelsVar1=3
numLevelsVar2=1
nIts=100
numReps=1000
rowOffset=16
frontOffset=0
backOffset=1
gcfcOffset=2
ratioOffset=3
setWidth=4
temp=rep(0,numReps) # Size needs to be consistent with number of repetitions

someData <- rep(0, nIts*numLevelsVar1*numLevelsVar2);
data1array <- array(someData, c(nIts, numLevelsVar1, numLevelsVar2));  
data2array <- array(someData, c(nIts, numLevelsVar1, numLevelsVar2));  
data1sd <- array(someData, c(nIts, numLevelsVar1, numLevelsVar2));  
data2sd <- array(someData, c(nIts, numLevelsVar1, numLevelsVar2));  
# -------------------------
## Read data
# -------------------------
for(j in 1:numLevelsVar2){  
  # Read ratio data from data1
  currCol=2+setWidth*numReps*numLevelsVar1*(j-1)
  for(n in 1:numLevelsVar1){  # This seems to be working
    for(m in 1:nIts){
      for(p in 1:numReps){
        temp[p] = as.numeric(data1[m+rowOffset,currCol+ratioOffset+setWidth*(p-1)])
      }
#       print(temp)
      data1array[m,n,j]=mean(temp)
      data1sd[m,n,j]=sd(temp)
    }
    currCol=currCol+numReps*setWidth
  }
  
  # Read ratio data from data2
  currCol=2+setWidth*numReps*numLevelsVar1*(j-1)
  for(n in 1:numLevelsVar1){  # This seems to be working
    for(m in 1:nIts){
      for(p in 1:numReps){
        temp[p] = as.numeric(data2[m+rowOffset,currCol+ratioOffset+setWidth*(p-1)])
      }
#       print(temp)
      data2array[m,n,j]=mean(temp)
      data2sd[m,n,j]=sd(temp)
    }
    currCol=currCol+numReps*setWidth
  }
}



# --------------------------
## Plots
# --------------------------


attach(mtcars)
par(mfrow=c(numLevelsVar2,numLevelsVar1))

i=0
for(j in 1:numLevelsVar2){
  for(k in 1:numLevelsVar1){
  i++
  print(i)
  plot(data1array[,k,j], type="l",lwd=2, col = "blue", xlim=c(0, 100), ylim=c(-1, 1.5))
  lines(data2array[,k,j],type="l",lwd=2, col = "red")
  abline(h=0, lty=2)
  str = "title"
  title(main=str) 
  }
}

summary1=matrix(0,100,3)
summary1[,1]=data1array[,1,1]
summary1[,2]=data1array[,2,1]
summary1[,3]=data1array[,3,1]
summary2=matrix(0,100,3)
summary2[,1]=data2array[,1,1]
summary2[,2]=data2array[,2,1]
summary2[,3]=data2array[,3,1]

# Format output for prism
meanStdN_1=matrix(0,100,3)
meanStdN_1[,1]=data1array[,3,1] # change number to look at different FC positions
meanStdN_1[,2]=data1sd[,3,1]
meanStdN_1[,3]=numReps
meanStdN_2=matrix(0,100,3)
meanStdN_2[,1]=data2array[,3,1]
meanStdN_2[,2]=data2sd[,3,1]
meanStdN_2[,3]=numReps