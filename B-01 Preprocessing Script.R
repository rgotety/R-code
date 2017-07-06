#Installing the packages to read the xlsx file
install.packages("openxlsx")
library(openxlsx)


#Reading the input the variables
df=read.xlsx("C:/Users/User/Desktop/CAPSTONE/backup excels/B-01 California.xlsx",sheet = 1,detectDates = TRUE)


#Removing the redundant variables
df$App.Number.s<- NULL
df$Admit.Type.Adj.Descr<- NULL
df$Census.App.Campus.Descr<- NULL
df$Census.App.College.Short.Descr<- NULL
df$Census.App.Division.Adj.Descr<- NULL
df$Census.App.Dept.Adj.Descr<- NULL
df$Census.App.Degree.Descr<- NULL
df$Census.App.Academic.Plan.Descr<- NULL
df$Gender.Descr<- NULL
df$Minority.Status.Descr<- NULL
df$MKT<- NULL
df$High.School.Descr<- NULL
df$High.School.State.Code<- NULL
df$Last.Transfer.Inst.Descr<- NULL
df$Student.Campus.Description<- NULL
df$Tempe.Enrolled.Flag<- NULL
df$West.Enrolled.Flag<- NULL
df$Polytech.Enrolled.Flag<- NULL
df$Downtown.Enrolled.Flag<- NULL
df$Admit.Reason.Code<- NULL
df$Rolled.Application<- NULL
df$Conditional.Admit<- NULL
df$Admit<- NULL
df$Enrolled<- NULL
df$High.School.Rank.Percent<-NULL
df$High.School.Graduation.Year<-NULL
df$Sat.Combined.Exam.Score<-NULL
df$Sat.Verbal.Exam.Score<-NULL
df$Sat.Quantitative.Exam.Score<-NULL
df$Act.Composite.Exam.Score<-NULL
df$Atp.Code <- NULL
df$Application.Received<-NULL
df$Application.Complete<-NULL
df$High.School.Gpa <- NULL

#Imputing the missing values
df$Cumulative.Transfer.Gpa[df$Admit.Type.Adj == "TRN" & df$Admitted.Flag == "Y" & is.na(df$Cumulative.Transfer.Gpa)] <- 2.92
df$Cumulative.Transfer.Gpa[df$Admit.Type.Adj == "TRN" & df$Admitted.Flag == "N" & is.na(df$Cumulative.Transfer.Gpa)] <- 2.1
df$Cumulative.Transfer.Gpa[df$Admit.Type.Adj == "FTF" & is.na(df$Cumulative.Transfer.Gpa)] <- 0
na.omit(df$High.School.Abor.Gpa[df$Admit.Type.Adj == "FTF"])
na.omit(df$Sat.Combined.Exam.Score.Adj[df$Admit.Type.Adj == "FTF"])
df$High.School.Abor.Gpa[df$Admit.Type.Adj == "FTF" & df$Admitted.Flag == "Y" & is.na(df$High.School.Abor.Gpa)] <- 3.15
df$Sat.Combined.Exam.Score.Adj[df$Admit.Type.Adj == "FTF" & df$Admitted.Flag == "Y" & is.na(df$Sat.Combined.Exam.Score.Adj)] <- 1073
df$High.School.Abor.Gpa[df$Admit.Type.Adj == "FTF" & df$Admitted.Flag == "N" & is.na(df$High.School.Abor.Gpa)] <- 2.49
df$Sat.Combined.Exam.Score.Adj[df$Admit.Type.Adj == "FTF" & df$Admitted.Flag == "N" & is.na(df$Sat.Combined.Exam.Score.Adj)] <- 880
df$High.School.Abor.Gpa[df$Admit.Type.Adj == "TRN" & is.na(df$High.School.Abor.Gpa)] <- 0
df$Sat.Combined.Exam.Score.Adj[df$Admit.Type.Adj == "TRN" & is.na(df$Sat.Combined.Exam.Score.Adj)] <- 0
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa <= 2.5 & df$Sat.Combined.Exam.Score.Adj <= 835] <- "< 86"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.5 & df$High.School.Abor.Gpa <= 2.75 & df$Sat.Combined.Exam.Score.Adj > 835 & df$Sat.Combined.Exam.Score.Adj <= 920 ] <- "86 - 93"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.75 & df$High.School.Abor.Gpa <= 2.95 & df$Sat.Combined.Exam.Score.Adj > 920 & df$Sat.Combined.Exam.Score.Adj <= 1000 ] <- "94 - 102"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.95 & df$High.School.Abor.Gpa <= 3.10 & df$Sat.Combined.Exam.Score.Adj > 1000 & df$Sat.Combined.Exam.Score.Adj <= 1075 ] <- "103 - 107"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.10 & df$High.School.Abor.Gpa <= 3.25 & df$Sat.Combined.Exam.Score.Adj > 1075 & df$Sat.Combined.Exam.Score.Adj <= 1100 ] <- "108 - 110"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.25 & df$High.School.Abor.Gpa <= 3.4 & df$Sat.Combined.Exam.Score.Adj > 1100 & df$Sat.Combined.Exam.Score.Adj <= 1200 ] <- "111 - 120"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.4 & df$High.School.Abor.Gpa <= 3.6 & df$Sat.Combined.Exam.Score.Adj > 1200 & df$Sat.Combined.Exam.Score.Adj <= 1300 ] <- "121 - 128"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.6 & df$High.School.Abor.Gpa <= 4 & df$Sat.Combined.Exam.Score.Adj > 1300 & df$Sat.Combined.Exam.Score.Adj <= 1400 ] <- "129 - 146"
df$Calculated.Index.Group[df$Admit.Type.Adj == "TRN" & is.na(df$Calculated.Index.Group)] <- "No Index"
df$Cumulative.Transfer.Gpa[df$Admit.Type.Adj == "FTF" & df$Cumulative.Transfer.Gpa > 0] <- 0
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 1.7 & df$High.School.Abor.Gpa <= 3.0 & df$Sat.Combined.Exam.Score.Adj == 880 ] <- "86 - 93"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa >= 2.0 & df$High.School.Abor.Gpa <= 3.10 & df$Sat.Combined.Exam.Score.Adj <= 1073 & df$Calculated.Index.Group == "No Index" ] <- "103 - 107"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.10 & df$High.School.Abor.Gpa <= 4.0 & df$Sat.Combined.Exam.Score.Adj == 1073 ] <- "108 - 110"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.0 & df$High.School.Abor.Gpa <= 4.0 & df$Sat.Combined.Exam.Score.Adj > 1000 & df$Calculated.Index.Group == "No Index" ] <- "103 - 107"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.0 & df$High.School.Abor.Gpa <= 3.10 & df$Sat.Combined.Exam.Score.Adj > 1300 & df$Calculated.Index.Group == "No Index" ] <- "111 - 120"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 3.0 & df$High.School.Abor.Gpa <= 4.0 & df$Sat.Combined.Exam.Score.Adj <= 1000 & df$Calculated.Index.Group == "No Index" ] <- "94 - 102"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.0 & df$High.School.Abor.Gpa <= 2.5 & df$Sat.Combined.Exam.Score.Adj <= 1200 & df$Calculated.Index.Group == "No Index" ] <- "94 - 102"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.5 & df$High.School.Abor.Gpa <= 3.0 & df$Sat.Combined.Exam.Score.Adj <= 1200 & df$Calculated.Index.Group == "No Index" ] <- "103 - 107"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.5 & df$High.School.Abor.Gpa <= 3.0 & df$Sat.Combined.Exam.Score.Adj <= 1300 & df$Calculated.Index.Group == "No Index" ] <- "108 - 110"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa > 2.0 & df$High.School.Abor.Gpa <= 2.5 & df$Sat.Combined.Exam.Score.Adj <= 1300 & df$Calculated.Index.Group == "No Index" ] <- "103 - 107"
df$Calculated.Index.Group[df$Admit.Type.Adj == "FTF" & df$High.School.Abor.Gpa <= 1.9 & df$Sat.Combined.Exam.Score.Adj <= 900 & df$Calculated.Index.Group == "No Index" ] <- "< 86"

# addition of two new variables
df$decision_time <- as.Date(df$Admit.Date, origin="1899-12-30") - as.Date(df$Application.Date, origin="1899-12-30") 
df$decision_time_deposit <- as.Date(df$Deposit.Date, origin = "1899-12-30") - as.Date(df$Admit.Date, origin = "1899-12-30")
df$decision_time_deposit[is.na(df$decision_time_deposit)] <- 0
df$decision_time[is.na(df$decision_time)] <- 0

# Rephrasing certain values
df$Term.Code[df$Term.Code == "2107"] <- "Fall2010"
df$Term.Code[df$Term.Code == "2117"] <- "Fall2011"
df$Term.Code[df$Term.Code == "2127"] <- "Fall2012"
df$Term.Code[df$Term.Code == "2137"] <- "Fall2013"
df$Term.Code[df$Term.Code == "2147"] <- "Fall2014"
df$Term.Code[df$Term.Code == "2157"] <- "Fall2015"
df$FASFA.STATUS[df$FASFA.STATUS == "-"] <- "Not Applied"
df$Group.of.Fed.Efc[is.na(df$Group.of.Fed.Efc)] <- "Not Eligible"
df$Pell.Eligibility[is.na(df$Pell.Eligibility)] <- "Not Eligible"
df$FASFA.STATUS[df$Group.of.Fed.Efc == "Not Eligible"] <- "Not Eligible"
df$FASFA.DATE[df$FASFA.STATUS == "Not Eligible"] <- "Not Eligible"
df$Census.App.Division.Adj.Code[is.na(df$Census.App.Division.Adj.Code)] <- "No Division"

# Converting to binary values
df$admitted[df$Admitted.Flag == "Y"] <- 1
df$admitted[df$Admitted.Flag == "N"] <- 0
df$enrolled[df$Asu.Enrolled.Flag == "Y"] <- 1
df$enrolled[df$Asu.Enrolled.Flag == "N"] <- 0
df$completed[df$Completed.Application.Flag == "Y"] <- 1
df$completed[df$Completed.Application.Flag == "N"] <- 0
df$Enrollment.Deposit[is.na(df$Enrollment.Deposit)] <- 0

# Exporting the file
write.csv(df,file = "C:/Users/User/Desktop/CAPSTONE/cleaned_data.csv")

