# R script to simulate axon guidance

# Clear all
rm(list=ls());

# Define variables
axonsPerExplant = 5000;
aveStepSize = 1;
gradOrientation = 3*pi/2 # For some reason (in calculations later), this is the only value that works
k=-0.5 # Compared to 0.5 determined empirically by Mortimer et al.
concentration = 1; 
dTheta = pi / 30;
steepness = 1;
steepnessRange=10^seq(-4,0,len=30)
# concRange=seq(0,1,0.05)
concRange=10^seq(-4,0,len=20)
nIts=25;
segmentLength = 25;

# ------------------------------------------------------
# Define the initial conditions and setup the matrices
# ------------------------------------------------------


# Determine initial positions for all neurons
initialX=0;
initialY=0;

# Create an array with the positions
posX_1=matrix(0,axonsPerExplant,nIts)
posY_1=matrix(0,axonsPerExplant,nIts)
posX_1[,1]=initialX;
posY_1[,1]=initialY;

posX_2=matrix(0,axonsPerExplant,nIts)
posY_2=matrix(0,axonsPerExplant,nIts)
posX_2[,1]=initialX;
posY_2[,1]=initialY;

posX_3=matrix(0,axonsPerExplant,nIts)
posY_3=matrix(0,axonsPerExplant,nIts)
posX_3[,1]=initialX;
posY_3[,1]=initialY;

# posX_4=matrix(0,axonsPerExplant,nIts)
# posY_4=matrix(0,axonsPerExplant,nIts)
# posX_4[,1]=initialX;
# posY_4[,1]=initialY;

# Create an array with a random angular position
currHeading_1=matrix(0,axonsPerExplant,nIts)
currHeading_1[,1]=runif(axonsPerExplant);
currHeading_1[,1]=currHeading_1[,1] * 2 * pi; # Angles in radians

currHeading_2=matrix(0,axonsPerExplant,nIts)
currHeading_2[,1]=runif(axonsPerExplant);
currHeading_2[,1]=currHeading_2[,1] * 2 * pi; # Angles in radians

currHeading_3=matrix(0,axonsPerExplant,nIts)
currHeading_3[,1]=runif(axonsPerExplant);
currHeading_3[,1]=currHeading_3[,1] * 2 * pi; # Angles in radians

#currHeading_4=matrix(0,axonsPerExplant,nIts)
#currHeading_4[,1]=runif(axonsPerExplant);
#currHeading_4[,1]=currHeading_4[,1] * 2 * pi; # Angles in radians

# -------------------------------------- 
# Loop across multiple steepnesses (or concentrations)
# --------------------------------------

 steepnessSummary=matrix(0,length(steepnessRange),7) # Made explicitly for prism graphs
#steepnessSummary=matrix(0,length(concRange),7)      # Modified to loop for concentrations

 for(s in 1:length(steepnessRange)){
#for(s in 1:length(concRange)){
  
   steepness = steepnessRange[s]
#  concentration = concRange[s]
  # -------------------------------------------
  # Perform calculations using relevant models
  # -------------------------------------------
  
  sampleFrom = rep(NaN, axonsPerExplant)
  for(i in 1:(nIts-1)){
   
    for (n in 1:axonsPerExplant){
      
      # Calculation for turning model (Mortimer et al 2010)
      
          # Calculate new positions based on angles
          posX_1[n,i+1]=posX_1[n,i]+aveStepSize*cos(currHeading_1[n,i])
          posY_1[n,i+1]=posY_1[n,i]+aveStepSize*sin(currHeading_1[n,i])
          
          # Calculate SnR for turning model
          turnSNR = steepness*sqrt(concentration/(1+concentration)^3)*sin(currHeading_1[n,i]-gradOrientation)
          sampleDist = rnorm(1000,turnSNR,1)
          sampleFrom <- sampleDist[runif(1)*1000+1] # Choose a random index of sampleDist
          
          # Calculate new heading direction randomly (though biased)
          if(sampleFrom>0){
            currHeading_1[n,i+1]<-currHeading_1[n,i]+dTheta;
          }else if(sampleFrom<0){
            currHeading_1[n,i+1]<-currHeading_1[n,i]-dTheta;
          }
  
          
      # Calculation of growth rate modulation model (Mortimer et al 2010)
      
          # Calculate SnR for turning model
          grmSNR = steepness*sqrt(concentration/(1+concentration)^3)*cos(currHeading_2[n,i]-gradOrientation)
          sampleDist = rnorm(1000,grmSNR,1)
          sampleFrom <- sampleDist[runif(1)*1000+1] # Choose a random index of sampleDist
      
          # Calculate current step size using GRM formula
          currStepSize=aveStepSize*(1+k*(sampleFrom))
      
          # Calculate new positions based on angles
          posX_2[n,i+1]=posX_2[n,i]+currStepSize*cos(currHeading_2[n,i])
          posY_2[n,i+1]=posY_2[n,i]+currStepSize*sin(currHeading_2[n,i])
      
          # Calculate new heading direction entirely randomly
          coinFlip = runif(1);
          if (coinFlip > 0.5){
            currHeading_2[n,i+1]<-currHeading_2[n,i]+dTheta;
          } else if (coinFlip<0.5){
            currHeading_2[n,i+1]<-currHeading_2[n,i]-dTheta;
          }
      
      # Calculation for additive SnR turning model (Adapted from Mortimer et al 2010)
      
          # Calculate new positions based on angles
          posX_3[n,i+1]=posX_3[n,i]+aveStepSize*cos(currHeading_3[n,i])
          posY_3[n,i+1]=posY_3[n,i]+aveStepSize*sin(currHeading_3[n,i])
          
          # Calculate SnR for turning model
          turnSNR = steepness*sqrt(concentration/(1+concentration)^3)*sin(currHeading_3[n,i]-gradOrientation)
          turnSNR = turnSNR*2  # Most simple combination of gradients, since we assume equivalent gradient parameters
          sampleDist = rnorm(1000,turnSNR,1)
          sampleFrom <- sampleDist[runif(1)*1000+1] # Choose a random index of sampleDist
          
          # Calculate new heading direction randomly (though biased)
          if(sampleFrom>0){
            currHeading_3[n,i+1]<-currHeading_3[n,i]+dTheta;
          }else if(sampleFrom<0){
            currHeading_3[n,i+1]<-currHeading_3[n,i]-dTheta;
          }
      
    } # End loop for all axons
   
  } # End of iterations (time)
  
  
  
  # ------------------------------
  ## Draw axons
  # ------------------------------
  
  # attach(mtcars)
  # par(mfrow=c(2,2))
  #   Plot for model 1 (turning)
  #   plot(0,0,xlim=c(min(posX_1),max(posX_1)),ylim=c(min(posY_1),max(posY_1)),main="Axon turning")
  #   
  # 
  #   for (j in 1:axonsPerExplant){
  #     # Set current axon color
  #     
  #     for (k in 1:(nIts-1)){
  #       segments(posX_1[j,k],posY_1[j,k], posX_1[j,k+1], posY_1[j,k+1], col= 'black')
  #       
  #     }
  #     
  #   }
  # 
  #   Plot for model 2 (GRM)
  #   plot(0,0,xlim=c(min(posX_2),max(posX_2)),ylim=c(min(posY_2),max(posY_2)),main="Growth Rate Modulation")
  #   for (j in 1:axonsPerExplant){
  #    # Set current axon color
  #     
  #    for (k in 1:(nIts-1)){
  #      segments(posX_2[j,k],posY_2[j,k], posX_2[j,k+1], posY_2[j,k+1])
  #       
  #    }
  #     
  #  }
  #   Plot for model 3 (Additive turning)
  #  plot(0,0,xlim=c(min(posX_3),max(posX_3)),ylim=c(min(posY_3),max(posY_3)),main="Additive turning")
  #   for (j in 1:axonsPerExplant){
  #    # Set current axon color
  #     
  #    for (k in 1:(nIts-1)){
  #      segments(posX_3[j,k],posY_3[j,k], posX_3[j,k+1], posY_3[j,k+1])
  #       
  #     }
  #     
  #  }
  
  
  # -------------------------------
  # Calculate turning measurements
  # -------------------------------
  
  angles_1=matrix(0,axonsPerExplant,3)
#   angles_2=matrix(0,axonsPerExplant,3)
  angles_3=matrix(0,axonsPerExplant,3)
  
  for (j in 1:axonsPerExplant){
    # ------ Model 1: turning to single cue
    # Calculate the angles as done previously in matlab script
    # Point i
    axonXi=posX_1[j,1]
    axonYi=posY_1[j,1]
    # Point ii
    sumLength=0
    i=1
    dsqrd=0
    while(sumLength<segmentLength){
      if(i<nIts){
        dsqrd=(posX_1[j,i+1]-posX_1[j,i])^2 + (posY_1[j,i+1]-posY_1[j,i])^2
        sumLength=sumLength+sqrt(dsqrd);
      }
      i=i+1
    }
    axonXii=posX_1[j,i]
    axonYii=posY_1[j,i] 
    # Point iii: beginning of tip angle or final segment
    sumLength=0
    k=nIts # May need to include -1
    dsqrd=0
    while(sumLength<segmentLength){
      if (k>0){
        dsqrd=(posX_1[j,k-1]-posX_1[j,k])^2 + (posY_1[j,k-1]-posY_1[j,k])^2
        sumLength=sumLength+sqrt(dsqrd);
      }
      k=k-1
    }
    axonXiii=posX_1[j,k]
    axonYiii=posY_1[j,k]  
    # Point iv: endpoint
    axonXiv=posX_1[j,nIts] # May need -1
    axonYiv=posY_1[j,nIts] # may need -1
    # Unit vector for initial angle (should be robust to nonzero initial position)
    unitAxonXi=axonXii-axonXi
    unitAxonYi=axonYii-axonYi
    unitVectorInitial=c(unitAxonXi, unitAxonYi)
    # Initial (base) angle with respect to gradient direction
    di=sqrt(unitAxonXi^2+unitAxonYi^2)
    gradVectorInitial=c(gradOrientation, di) # Possibly need to replace 0 with gradOrientation and remove - (leftover from image coordinates)
    # Create unit vector for final angle, beginning at (0,0)
    unitAxonXf=axonXiv-axonXiii
    unitAxonYf=axonYiv-axonYiii
    unitVectorFinal=c(unitAxonXf,unitAxonYf)
    # Find final angle with respect to the gradient
    df=sqrt(unitAxonXf^2+unitAxonYf^2)
    gradVectorFinal=c(gradOrientation, df) # Possibly need to replace 0 with gradOrientation and remove - (leftover from image coordinates)
    # Calculate initial, final and turned angles
    dotInitial=acos(sum(gradVectorInitial*unitVectorInitial)/sqrt(sum(gradVectorInitial^2)*sum(unitVectorInitial^2)))
    dotFinal=acos(sum(gradVectorFinal*unitVectorFinal)/sqrt(sum(gradVectorFinal^2)*sum(unitVectorFinal^2)))
    turnedTwd=0
    if(dotInitial>dotFinal){
      turnedTwd=1;
    } else if(dotInitial<dotFinal){
      turnedTwd=-1;
    }
    dotTurned=acos(sum(unitVectorInitial*unitVectorFinal)/sqrt(sum(unitVectorInitial^2)*sum(unitVectorFinal^2)))*turnedTwd 
    
    angles_1[j,1]=dotInitial*180/pi
    angles_1[j,2]=dotFinal*180/pi
    angles_1[j,3]=dotTurned*180/pi
    
    # Test angle calculations
    #segments(axonXi,axonYi,axonXii,axonYii, col= 'blue')
    #segments(axonXiii,axonYiii,axonXiv,axonYiv, col= 'red')
    # ------End of model 1 calculations
  
#     # ------ Model 2: 
#     # Calculate the angles as done previously in matlab script
#     # Point i
#     axonXi=posX_2[j,1]
#     axonYi=posY_2[j,1]
#     # Point ii
#     sumLength=0
#     i=1
#     dsqrd=0
#     while(sumLength<segmentLength){
#       if(i<nIts){
#         dsqrd=(posX_2[j,i+1]-posX_2[j,i])^2 + (posY_2[j,i+1]-posY_2[j,i])^2
#         sumLength=sumLength+sqrt(dsqrd);
#       }
#       i=i+1
#     }
#     axonXii=posX_2[j,i]
#     axonYii=posY_2[j,i] 
#     # Point iii: beginning of tip angle or final segment
#     sumLength=0
#     k=nIts # May need to include -1
#     dsqrd=0
#     while(sumLength<segmentLength){
#       if (k>0){
#         dsqrd=(posX_2[j,k-1]-posX_2[j,k])^2 + (posY_2[j,k-1]-posY_2[j,k])^2
#         sumLength=sumLength+sqrt(dsqrd);
#       }
#       k=k-1
#     }
#     axonXiii=posX_2[j,k]
#     axonYiii=posY_2[j,k]  
#     # Point iv: endpoint
#     axonXiv=posX_2[j,nIts] # May need -1
#     axonYiv=posY_2[j,nIts] # may need -1
#     # Unit vector for initial angle (should be robust to nonzero initial position)
#     unitAxonXi=axonXii-axonXi
#     unitAxonYi=axonYii-axonYi
#     unitVectorInitial=c(unitAxonXi, unitAxonYi)
#     # Initial (base) angle with respect to gradient direction
#     di=sqrt(unitAxonXi^2+unitAxonYi^2)
#     gradVectorInitial=c(gradOrientation, di) # Possibly need to replace 0 with gradOrientation and remove - (leftover from image coordinates)
#     # Create unit vector for final angle, beginning at (0,0)
#     unitAxonXf=axonXiv-axonXiii
#     unitAxonYf=axonYiv-axonYiii
#     unitVectorFinal=c(unitAxonXf,unitAxonYf)
#     # Find final angle with respect to the gradient
#     df=sqrt(unitAxonXf^2+unitAxonYf^2)
#     gradVectorFinal=c(gradOrientation, df) # Possibly need to replace 0 with gradOrientation and remove - (leftover from image coordinates)
#     # Calculate initial, final and turned angles
#     dotInitial=acos(sum(gradVectorInitial*unitVectorInitial)/sqrt(sum(gradVectorInitial^2)*sum(unitVectorInitial^2)))
#     dotFinal=acos(sum(gradVectorFinal*unitVectorFinal)/sqrt(sum(gradVectorFinal^2)*sum(unitVectorFinal^2)))
#     turnedTwd=0
#     if(dotInitial>dotFinal){
#       turnedTwd=1;
#     } else if(dotInitial<dotFinal){
#       turnedTwd=-1;
#     }
#     dotTurned=acos(sum(unitVectorInitial*unitVectorFinal)/sqrt(sum(unitVectorInitial^2)*sum(unitVectorFinal^2)))*turnedTwd 
#     
#     angles_2[j,1]=dotInitial*180/pi
#     angles_2[j,2]=dotFinal*180/pi
#     angles_2[j,3]=dotTurned*180/pi
#     # ---------- End of model 2
    
    
    
    
    # ------ Model 3: turning to single cue
    # Calculate the angles as done previously in matlab script
    # Point i
    axonXi=posX_3[j,1]
    axonYi=posY_3[j,1]
    # Point ii
    sumLength=0
    i=1
    dsqrd=0
    while(sumLength<segmentLength){
      if(i<nIts){
        dsqrd=(posX_3[j,i+1]-posX_3[j,i])^2 + (posY_3[j,i+1]-posY_3[j,i])^2
        sumLength=sumLength+sqrt(dsqrd);
      }
      i=i+1
    }
    axonXii=posX_3[j,i]
    axonYii=posY_3[j,i] 
    # Point iii: beginning of tip angle or final segment
    sumLength=0
    k=nIts # May need to include -1
    dsqrd=0
    while(sumLength<segmentLength){
      if (k>0){
        dsqrd=(posX_3[j,k-1]-posX_3[j,k])^2 + (posY_3[j,k-1]-posY_3[j,k])^2
        sumLength=sumLength+sqrt(dsqrd);
      }
      k=k-1
    }
    axonXiii=posX_3[j,k]
    axonYiii=posY_3[j,k]  
    # Point iv: endpoint
    axonXiv=posX_3[j,nIts] # May need -1
    axonYiv=posY_3[j,nIts] # may need -1
    # Unit vector for initial angle (should be robust to nonzero initial position)
    unitAxonXi=axonXii-axonXi
    unitAxonYi=axonYii-axonYi
    unitVectorInitial=c(unitAxonXi, unitAxonYi)
    # Initial (base) angle with respect to gradient direction
    di=sqrt(unitAxonXi^2+unitAxonYi^2)
    gradVectorInitial=c(gradOrientation, di) # Possibly need to replace 0 with gradOrientation and remove - (leftover from image coordinates)
    # Create unit vector for final angle, beginning at (0,0)
    unitAxonXf=axonXiv-axonXiii
    unitAxonYf=axonYiv-axonYiii
    unitVectorFinal=c(unitAxonXf,unitAxonYf)
    # Find final angle with respect to the gradient
    df=sqrt(unitAxonXf^2+unitAxonYf^2)
    gradVectorFinal=c(gradOrientation, df) # Possibly need to replace 0 with gradOrientation and remove - (leftover from image coordinates)
    # Calculate initial, final and turned angles
    dotInitial=acos(sum(gradVectorInitial*unitVectorInitial)/sqrt(sum(gradVectorInitial^2)*sum(unitVectorInitial^2)))
    dotFinal=acos(sum(gradVectorFinal*unitVectorFinal)/sqrt(sum(gradVectorFinal^2)*sum(unitVectorFinal^2)))
    turnedTwd=0
    if(dotInitial>dotFinal){
      turnedTwd=1;
    } else if(dotInitial<dotFinal){
      turnedTwd=-1;
    }
    dotTurned=acos(sum(unitVectorInitial*unitVectorFinal)/sqrt(sum(unitVectorInitial^2)*sum(unitVectorFinal^2)))*turnedTwd 
    
    angles_3[j,1]=dotInitial*180/pi
    angles_3[j,2]=dotFinal*180/pi
    angles_3[j,3]=dotTurned*180/pi
    # ---------- End of model 3
  } # End looping for all axons
  
  # print(steepness)
  # print(mean(angles_1[,3]))
  # print(sd(angles_1[,3]))
  
  # --------------------------
  # Summary calculations across steepnesses
  # ----------------------------
   steepnessSummary[s,1]=steepnessRange[s]
#  steepnessSummary[s,1]=concRange[s]
  steepnessSummary[s,2]=mean(angles_1[,3]) # Model 1
  steepnessSummary[s,3]=sd(angles_1[,3])
  steepnessSummary[s,4]=axonsPerExplant;
  
#   steepnessSummary[s,5]=mean(angles_2[,3]) # Model 2
#   steepnessSummary[s,6]=sd(angles_2[,3])
#   steepnessSummary[s,7]=axonsPerExplant  
  
  steepnessSummary[s,5]=mean(angles_3[,3]) # Model 1
  steepnessSummary[s,6]=sd(angles_3[,3])
  steepnessSummary[s,7]=axonsPerExplant;
} # End of loop for steepnesses

save.image("~/My Dropbox/Work-home/Computational neuroscience/Project/workspace-ConcRange.RData")