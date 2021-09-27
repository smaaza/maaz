#Assignment NOD Q4

#Q1
#i)
p = 0.68
dbinom(3, 4, p)
#ii
theft4= dbinom(0:4, 4, p)
plot(theft4)
bar.plot1=barplot(theft4, main="PROBABILITY OF THEFTS DUE TO DRUGS",
                 names.arg = 0:4,ylim=c(0,0.50), col=c("blue","green","red","brown","gold2"), 
                 xlab="NO. OF THEFTS", ylab="PROBABILITY", las=1)
text(x=bar_plot, y=theft4, label=theft4, pos = 3, cex=1)

#iii
p.s =0.4
sum(dbinom(4:9, 14, p.s))
#iv
survive = dbinom(4:9, 14, p.s)
bar.plot2=barplot(survive, main= "PROBABILITY OF POSSIBLE SURVIVORS", 
                  xlab = "NO. OF SURVIVORS" , ylab="PROBABILITY", las=1, 
                  col="brown", ylim=c(0,0.30), names=c(4:9))
#V

lambda=sum((4:9)*survive)/sum(survive)
lambda
# lambda=5.938223
poisson.s=dpois(4:9, lambda)
poisson.s
combine=rbind(survive,poisson.s)
dimnames(combine)=list(c("Binomial","Poisson"), c(4:9))

barplot(combine, main="COMPARISON OF POISSON & BINOMIAL DISTRIBUTION", 
        xlab="NO. OF SURVIVORS", ylab="PROBABILITY OF SURVIVAL", las=1, beside=TRUE, 
        ylim=c(0,0.30), col=c("grey","blue"), legend.text=c("Binomial","Poisson"))
#____________x____________x__________________x_______________

#Q2i

Q2 = read.csv("question_2.csv")
attach(Q2)
aftermed=table(Q2$medicationType, Q2$bloodPressure_after)
summary(aftermed)
aggregate(bloodPressure_after~medicationType, Q2, mean)

boxplot(bloodPressure_after~medicationType, data = Q2, xlab="BP READINGS", 
        ylab = "MEDICATION TYPE", main="BLOOD PRESSURE AFTER MEDICATION", 
        font.lab=3, horizontal = TRUE, notch=T,
        col=c("lightgrey","darkgrey"), boxwex=0.5, whisklty=3, pch=16, 
        outcol=c("lightgrey","darkgrey"))

#iii 

#HO=Mean of bloodPressure_after = Mean of bloodPressure_before
#H1= Mean of bloodPressure_after > Mean of bloodPressure_before

# Paired t test

t.test(Q2$bloodPressure_after, Q2$bloodPressure_before, 
       paired = TRUE, alternative = "greater")



before_drug=subset(Q2$bloodPressure_before, Q2$medicationType=="drug")
after_drug=subset(Q2$bloodPressure_after, Q2$medicationType=="drug")
before_placebo=subset(Q2$bloodPressure_before, Q2$medicationType=="placebo")
after_placebo=subset(Q2$bloodPressure_after, Q2$medicationType=="placebo")

boxplot(before_placebo, after_placebo, before_drug, after_drug,
        names=c("Before(P)","After(P)","Before(D)","After(D)"), horizontal = T, las=1, 
        main="COMPARISON OF BP BEFORE & AFTER TAKING MEDICATION",
        xlab="BP", col = c("blue2","blue2","darkgreen", "darkgreen"),
        legend.text = c("Drug","Drug","Placebo","Placebo"))

# iv)
# for 90% CI

t.test(Q2$bloodPressure_after, Q2$bloodPressure_before, 
       paired = TRUE, alternative = "greater", conf.level = .90)
#________________x_________________x______________x

#Q3i

Regular=c(182,213,203)
Irregular=c(154,138,110)
attendence_pattern=rbind(Regular, Irregular)
colnames(attendence_pattern)=c("Protestants","Catholic","Jewish")
attendence_pattern

barplot(attendence_pattern, beside = T, legend.text = c("Regular","Irregular"), ylim=c(0,300),
        xlab="RELIGION", ylab="ATTENDENCE", las=1, 
        main="ATTENDENCE PATTERN", col=c("lightblue","lightpink"))

#ii

#H0: There is no relationship between religion and attendance pattern of its members
#H1: There is significant relationship between religion and attendance pattern

#iii
dimnames(attendence_pattern)

chisq.test(attendence_pattern, correct = FALSE)
# p-value is very small. Hence H0 is rejected. this also suggests that there is significant relationship between religion and attendance pattern.

n=sum(attendence_pattern)
a.count=rowSums(attendence_pattern)
r.prop=colSums(attendence_pattern)/n
e=outer(a.count,r.prop)

attend.chisq=sum((attendence_pattern-e)^2/e)
attendance=c("Regular","Irregular")
religion=c("Protestant", "Catholic", "Jewish")
a.prop=a.count/n

#simulation
x=replicate(2000,{
        #sampling attendance and religion
        sam.p=sample(attendance,size =n,replace = TRUE,prob = a.prop)
        sam.r=sample(religion,size =n,replace = TRUE,prob = r.prop)
        #tabulated result
        t=table(sam.p,sam.r)
        #calculating expected values
        row=rowSums(t)
        col=colSums(t)/sum(t)
        exp=outer(row,col)
        #chi-sq test using simulation
        sum((t-exp)^2/exp)
})
sum(x>attend.chisq)/2000
chisq.test(attendence_pattern, simulate.p.value = TRUE, B=2000)
#__________x_____________x____________________x____________

#Q4i

PIMA = read.csv("PIMA.csv")
attach(PIMA)
plot(diastolic~age,data = PIMA,col=c("darkgrey","blue"),
     xlab="AGE",ylab = "DIASTOLIC BP", pch=16, las=1,
     main="RELATIONSHIP B/W AGE OF PATIENT & DIASTOLIC BP")
plot(diastolic~age,data = PIMA,col=c("maroon","darkblue"),
     xlab="AGE",ylab = "DIASTOLIC BP", pch=16, las=1,
     main="RELATIONSHIP B/W AGE & DIASTOLIC BP")
fit=lm(diastolic~age, data = PIMA)
print(fit)


abline(fit, col="black", lwd=2)

#ii

cor(PIMA$age, PIMA$diastolic, method = "p")
# Pearsons's Correlation coefficient= 0.2774099

#iii

fit=lm(diastolic~age, data = PIMA)

#bp of a 39 year old women

predict(fit, newdata = data.frame(age=39)) 

#iv
predict(fit, newdata = data.frame(age=39), level = .90)
confint(fit, age=39, level = 0.90)
