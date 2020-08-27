confidence<-function(p, n){
  ## p is proportion
  ## n is number of entries
  q = 1 - p
  res = (0.90 * sqrt((p * q) / n))
  res
}

############################################################################################################
#                                                 DEGREES                                                  #
############################################################################################################
degreeData<-c()
#Bachelors Degree - 1#
BD<- 42
for (num in 1:BD){
  degreeData<-rbind(degreeData, 1)
}
#Masters Degree - 2#
MD<- 17
for (num in 1:MD){
  degreeData<-rbind(degreeData, 2)
}
#N/A - 3#
ND<- 27
for (num in 1:ND){
  degreeData<-rbind(degreeData, 3)
}
#Other - 4#
OD<- 12
for (num in 1:OD){
  degreeData<-rbind(degreeData, 4)
}
#High School/GED - 5#
HS<-12
for (num in 1:HS){
  degreeData<-rbind(degreeData, 5)
}

# creating a pie chart using the percentages of the degrees #
#slices <- c(BD, MD , ND ,OD , HS)
#lbls <- c("BACHELORS 38.18%", "MASTERS 15.45%", "N/A 24.55%", "OTHER 10.91%", "HS/GED 10.91%")
#pie(slices, labels = lbls, main="Pie Chart of Degrees")

# creating a barplot using the frequencies of the degrees #
perDegrees <- BD + MD + ND + OD + HS
BDPer <- BD/perDegrees
MDPer <- MD/perDegrees
ODPer <- OD/perDegrees 
NAPer <- ND/perDegrees 
HSPer <- HS/perDegrees 
slices7<- c(ODPer, HSPer, MDPer, NAPer,  BDPer )
labelsD<-c("Other","High School/GED",   "Master's" , "N/A", "Bachelor's")
# creating a barplot using the frequencies of the degrees #
#Other HS/GED Master N/A Bachelor
ve<-barplot(slices7, main="Degrees for Statisticians", col=c("paleturquoise3"), ylim = c(0, 0.50), xlab = "Degree Types", ylab = "Relative Frequency", names.arg=labelsD)
text(ve, 0, round(slices7, 3),cex=1,pos=3)
segments(ve, 
         slices7 + confidence(slices7, 110), # upper bound
         ve,
         slices7 - confidence(slices7, 110), # lower bound 
         lwd = 1.5)
arrows(ve, slices7 + confidence(slices7, 110), # upper bound 
       ve,
       slices7 - confidence(slices7, 110), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
cat("Total", perDegrees, " ", BDPer," ", MDPer, " ", NAPer, " ", ODPer," ", HSPer, "\n\n")

############################################################################################################
#                                           SKILLS (PROFESSIONAL)                                          #
############################################################################################################
profSkills<-c()
#Microsoft Office and General Technology - 1#
MSOTech<- 35
for (num in 1:MSOTech){
  profSkills<-rbind(profSkills, 1)
}
#Math Skills - 2#
Math<- 7
for (num in 1:Math){
  profSkills<-rbind(profSkills, 2)
}
#Programming Skills - 3#
Prog<- 28
for (num in 1:Prog){
  profSkills<-rbind(profSkills, 3)
}
#Statistics and Data Analytics (Prior Experience) - 4#
SD<- 41
for (num in 1:SD){
  profSkills<-rbind(profSkills, 4)
}
#Other - Experience, Certifications etc - 5#
OSPro<- 10
for (num in 1:OSPro){
  profSkills<-rbind(profSkills, 5)
}


# creating a pie chart using the frequencies of the skills #
perProSkill <- MSOTech + Math + Prog + SD + OSPro
MSPer <- MSOTech/perProSkill
MathPer <- Math/perProSkill
ProgPer <- Prog/perProSkill
SDPer <- SD/perProSkill
OSProPer <- OSPro/perProSkill
cat("Total", perProSkill, " ", MSPer," ", MathPer, " ", ProgPer," ", SDPer, " ", OSProPer, "\n\n")
# Math Other Programming MS Stats
slices1 <- c(MathPer,OSProPer , ProgPer ,MSPer, SDPer )
labelsPPS<-c("Mathematics", "Other", "Programming", "Microsoft Office", "Stats and Data Analytics")
le<-barplot(slices1, main="Professional Skills for Statisticians",col=c("paleturquoise3"),ylim = c(0, 0.50), xlab = "Skills", ylab = "Relative Frequency", names.arg=labelsPPS)
text(le, 0, round(slices1, 3),cex=1,pos=3)
segments(le, 
         slices1 + confidence(slices1, 110), # upper bound
         le,
         slices1 - confidence(slices1, 110), # lower bound 
         lwd = 1.5)
arrows(le, slices1 + confidence(slices1, 110), # upper bound 
       le,
       slices1 - confidence(slices1, 110), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

############################################################################################################
#                                           SKILLS (PERSONAL)                                              #
############################################################################################################
persSkills<-c()
#Team Work - 1#
GW<- 20
for (num in 1:GW){
  persSkills<-rbind(persSkills, 1)
}
#Self-Motivated/Independent - 3#
SM<- 18
for (num in 1:SM){
  persSkills<-rbind(persSkills, 3)
}
#Good Communication (Oral & Written) - 2#
GCom<- (44+12)
for (num in 1:GCom){
  persSkills<-rbind(persSkills, 2)
}
#Other- Critical Thinking, Organization, Problem Solving etc - 5#
OSPer<- 19
for (num in 1:OSPer){
  persSkills<-rbind(persSkills, 5)
}

# creating a pie chart using the frequencies of the skills #
perPerSkill <- GW + GCom + SM + OSPer
GWPer <- GW/perPerSkill
GCPer <- GCom/perPerSkill
SMPer <- SM/perPerSkill
OSPerPer <- OSPer/perPerSkill

cat("Relative Frequencies: ", GWPer, " " , GCPer," ", SMPer, " ", OSPerPer)
orderedVector4 <- c(SMPer, OSPerPer, GWPer , GCPer)
labelsPS <-c("Self Motivated",  "Critical Thinking", "Teamwork", "Good Communication") 
te<-barplot(orderedVector4, main="Personal Skills for Statisticians",col=c("paleturquoise3"), ylim = c(0, 0.60), xlab = "Skill", ylab = "Relative Frequency", names.arg = labelsPS)
text(te, 0, round(orderedVector4, 3),cex=1,pos=3)
segments(te, 
         orderedVector4 + confidence(orderedVector4, 110), # upper bound
         te,
         orderedVector4 - confidence(orderedVector4, 110), # lower bound 
         lwd = 1.5)
arrows(te, orderedVector4 + confidence(orderedVector4, 110), # upper bound 
       te,
       orderedVector4 - confidence(orderedVector4, 110), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)


############################################################################################################
#                                             YEARS EXPERIENCE                                             #
############################################################################################################
years<-c()
#N/A - 1#
NY<- 66
for (num in 1:NY){
  years<-rbind(years, 1)
}
#0 - 6 Months - 2#
Zsix<-9
for (num in 1:Zsix){
  years<-rbind(years, 2)
}
#7 Months - 1 Year - 3#
one<-15
for (num in 1:one){
  years<-rbind(years, 3)
}
#1 Year and 1 Month - 2 Years - 4#
two<- 9
for (num in 1:two){
  years<-rbind(years, 4)
}
#2 Years and 1 Month - 3 Years - 5#
three<- 10
for (num in 1:three){
  years<-rbind(years, 5)
}
#More than 3 Years - 6#
four<- 14
for (num in 1:four){
  years<-rbind(years, 6)
}

# creating a barplot using the frequencies of the years experience #
perYears <- NY + Zsix + one + two + three + four
NYPer <- NY/perYears 
ZPer <- Zsix/perYears 
ONPer <- one/perYears 
TWPer <- two/perYears 
THRPer <- three/perYears 
FOURPer <- four/perYears 
#cat("Total", perYears, " ", NYPer, " " , ZPer," ", ONPer, " ", TWPer, " ", THRPer, " ", FOURPer , "\n\n")
#slices3 <- c(NYPer, ZPer, ONPer, TWPer, THRPer, FOURPer)
#lbls3 <- c("N/A 53.66%", "0 - 6 Months Year 7.32%", " 7 months - 1 Year 12.20%", "1 Year+ - 2 Years 7.32%", "2 Years+ - 3 Years 8.13%", "3 Years+ 11.38%")
#pie(slices3, labels = lbls3, main="Pie Chart of Years")

cat("Relative Frequencies: ", NYPer, " ",ZPer, " ",ONPer , " ", TWPer, " ", THRPer, " ", FOURPer)

orderedVector1 <- c( ZPer, TWPer, THRPer, FOURPer, ONPer, NYPer)
lbls7<-c("0 - 0.5", "0.6 - 1", "1 - 2", "2 - 3", "3+", "N/A")
ce<-barplot(orderedVector1, main="Years Experience",col=c("paleturquoise3"), ylim = c(0, 0.60), xlab = "Years", ylab = "Relative Frequency", names.arg=lbls7)
text(ce, 0, round(orderedVector1, 3),cex=1,pos=3)
segments(ce, 
         orderedVector1 + confidence(orderedVector1, 110), # upper bound
         ce,
         orderedVector1 - confidence(orderedVector1, 110), # lower bound 
         lwd = 1.5)
arrows(ce, orderedVector1 + confidence(orderedVector1, 110), # upper bound 
       ce,
       orderedVector1 - confidence(orderedVector1, 110), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# creating a piechart using the frequencies of the years experience #
perYears1 <- Zsix + one + two + three + four
ZPer1 <- Zsix/perYears1 
ONPer1 <- one/perYears1 
TWPer1 <- two/perYears1 
THRPer1 <- three/perYears1 
FOURPer1 <- four/perYears1 
cat("\nRELATIVE FREQ. WITH OUT N/A", perYears1, " ", ZPer1," ", ONPer1, " ", TWPer1, " ", THRPer1, " ", FOURPer1 , "\n\n")
orderedVector2 <- c(ZPer1, TWPer1, THRPer1,  FOURPer1, ONPer1)
lbls8<-c("0 - 0.5 years", "0.6 - 1 years", "1 - 2 years", "2 - 3 years", "3+ years")
ye<-barplot(orderedVector2, main="Years Experience w/o N/A", 
            xlab = "Years", ylab = "Relative Frequency", col=c("paleturquoise3"), ylim = c(0, 0.35),names.arg=lbls8)
text(ye, 0, round(orderedVector2, 3),cex=1,pos=3)
segments(ye, 
         orderedVector2 + confidence(orderedVector2, 110), # upper bound
         ye,
         orderedVector2 - confidence(orderedVector2, 110), # lower bound 
         lwd = 1.5)
arrows(ye, orderedVector2 + confidence(orderedVector2, 110), # upper bound 
       ye,
       orderedVector2 - confidence(orderedVector2, 110), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

############################################################################################################
#                                                 DEGREE TYPES                                             #
############################################################################################################
degTypes<-c()
#Math Degree - 1#
MDeg<- 4
for (num in 1:MDeg){
  degTypes<-rbind(degTypes, 1)
}
#Computer Science Degree - 2#
CSDeg<- 2
for (num in 1:CSDeg){
  degTypes<-rbind(degTypes, 2)
}
#Business Degree - 3#
BUDeg<- 5
for (num in 1:BUDeg){
  degTypes<-rbind(degTypes, 3)
}
#Social Science Degree - 4#
SoSDeg<- 5
for (num in 1:SoSDeg){
  degTypes<-rbind(degTypes, 4)
}
#Statistics Degree - 5#
StaDeg<- 10
for (num in 1:StaDeg){
  degTypes<-rbind(degTypes, 5)
}
#Other Stem Degree - 7#
SteDeg<- 12
for (num in 1:SteDeg){
  degTypes<-rbind(degTypes, 7)
}
#Other Degree - 8#
OtDeg<- 1
for (num in 1:OtDeg){
  degTypes<-rbind(degTypes, 8)
}
# computing the percentages for each category #
perTypes <-  MDeg + CSDeg+ BUDeg+ SoSDeg+ StaDeg+ SteDeg+ OtDeg
MATPer <- MDeg/perTypes * 100
COMPPer <- CSDeg/perTypes * 100
BUSPer <- BUDeg/perTypes * 100
SOSPer <- SoSDeg/perTypes * 100
STATPer <- StaDeg/perTypes * 100
STEMPer <- SteDeg/perTypes * 100
OTHERPer <- OtDeg/perTypes * 100
cat("Total", perTypes, " ", MATPer, " " , COMPPer," ", BUSPer, " ", SOSPer, " ", STATPer, " ", STEMPer , " ", OTHERPer, "\n\n")

# creating a barplot using the frequencies of the degree types #
MATPer1 <- MDeg/perTypes
COMPPer1 <- CSDeg/perTypes
BUSPer1 <- BUDeg/perTypes
SOSPer1 <- SoSDeg/perTypes
STATPer1 <- StaDeg/perTypes
STEMPer1 <- SteDeg/perTypes
OTHERPer1 <- OtDeg/perTypes
cat("Relative Frequencies: ", MATPer1, " ",COMPPer1, " ", BUSPer1, " ", SOSPer1, " ", STATPer1, " ", STEMPer1, " ", OTHERPer1 )
orderedVector <- c(OTHERPer1, COMPPer1 , MATPer1 ,  BUSPer1 , SOSPer1 , STATPer1 , STEMPer1  )
labelsDT <-c("Other", "Computer Science","Mathematics","Business","Social Science","Statistics","STEM") 

bp <- barplot(main="Prefered Degree Type", xlab = "Degree Type", 
               ylab = "Relative Frequency", ylim = c(0, 0.40), names.arg = labelsDT,
              col=c("paleturquoise3"), orderedVector)
text(bp, 0, round(orderedVector, 3),cex=1,pos=3)
segments(bp, 
         orderedVector + confidence(orderedVector, 110), # upper bound
         bp,
         orderedVector - confidence(orderedVector, 110), # lower bound 
         lwd = 1.5)
arrows(bp, orderedVector + confidence(orderedVector, 110), # upper bound 
       bp,
       orderedVector - confidence(orderedVector, 110), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

############################################################################################################
#                                     COMMUNICATION SKILLS BREAKDOWN                                       #
############################################################################################################
comSkills<-c()
#Verbal - 1#
VEB<- 8
for (num in 1:VEB){
  comSkills<-rbind(comSkills, 1)
}
#Written - 2#
WE<- 10
for (num in 1:WE){
  comSkills<-rbind(comSkills, 2)
}
#Bilingual - 3#
BE<- 5
for (num in 1:BE){
  comSkills<-rbind(comSkills, 3)
}
#Storytelling - 4#
SET<- 13
for (num in 1:SET){
  comSkills<-rbind(comSkills, 4)
}
#Interpersonal - 5#
INTP<-20
for (num in 1:INTP){
  comSkills<-rbind(comSkills, 5)
}
#Both Written and Verbal - 6#
BWAV<-31
for (num in 1:BWAV){
  comSkills<-rbind(comSkills, 6)
}

# creating a barplot using the frequencies of the degrees #
CBD<- BWAV + INTP + SET + BE + WE + VEB
BWAVPer <- BWAV/CBD
INTPPer <- INTP/CBD
SETPer <- SET/CBD 
BEPer <- BE/CBD 
WEPer <- WE/CBD 
VEBPer <- VEB/CBD 
slices9<- c(BEPer,VEBPer, WEPer, SETPer ,INTPPer,BWAVPer)
labelsCOM<-c( "Bilingual", "Verbal", "Written", "Storytelling" ,"Interpersonal", "Both W & V")
# creating a barplot using the frequencies of the degrees #
COM<-barplot(slices9, main="Communication Skills Breakdown",ylim = c(0, 0.50), xlab = "Communication Skill", 
            ylab = "Relative Frequency", col=c("paleturquoise3"), names.arg=labelsCOM)
text(COM, 0, round(slices9, 3),cex=1,pos=3)
segments(COM, 
         slices9 + confidence(slices9, CBD), # upper bound
         COM,
         slices9 - confidence(slices9, CBD), # lower bound 
         lwd = 1.5)
arrows(COM, slices9 + confidence(slices9, CBD), # upper bound 
       COM,
       slices9 - confidence(slices9, CBD), # lower bound
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
cat("Bilingual")
cat("\n", BEPer, " ", VEBPer, " " ,WEPer, " ", SETPer, " " ,INTPPer, " ",BWAVPer, "\n\n")
print(BEPer+VEBPer+ WEPer+SETPer+INTPPer+BWAVPer)