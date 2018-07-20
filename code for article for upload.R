
#-------------------------------------------------------------------------
# jags must be installed from:http://mcmc-jags.sourceforge.net/
# The following packages must be installed:
# install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("gplots")
# install.packages("R2jags")
# install.packages("survey")
# install_github( "ajdamico/lodown" , dependencies = TRUE )

library(devtools)
library(lodown)

# Download PNS data using the lodown package
pns_cat <- get_catalog( "pns" ,output_dir = file.path( path.expand( "~" ) , "PNS" ) )
pns_cat <- lodown( "pns" , pns_cat )

PNS_data <- readRDS( file.path( path.expand( "~" ) , "PNS" , 
                                "2013 all questionnaire survey.rds" ) )

# FUNCTION TO SWITCH 1,2 CHARACTER VARIABLES TO 0,1
sw <- function(x) {
  return(as.numeric(x)*-1+2)
}

# INITIAL DATASET--------------------------------------
library(dplyr) ; library(magrittr)
ds <- dplyr::mutate(PNS_data,
                    child = as.integer(c008 < 16),
                    upa = upa_pns,
                    urban = sw(v0026),
                    type.area = as.factor(v0031),
                    teen = as.integer(c008 %in% c(16,17)),
                    income = rowSums(cbind(e01602,e01802),na.rm=T),
                    hid = paste0(upa_pns,v0006_pns),
                    pid = paste0(hid,c00301),
                    socprog = vdf00102,
                    female = as.integer(c006=="feminino"),
                    age = c008,
                    type = c004,
                    children = child + teen,
                    def.int  = as.numeric(g001)*-1 +2,
                    def.phy  = as.numeric(g006)*-1 +2,
                    def.aud  = as.numeric(g014)*-1 +2,
                    def.vis  = as.numeric(g021)*-1 +2,
                    def.any  = as.integer((def.int + def.phy + def.aud + def.vis)>0),
                    hh.pci.all   = ave(income,hid,FUN=mean),
                    hh.child = ave(child,hid,FUN=sum),
                    hh.children = ave(children,hid,FUN=sum),
                    hh.teen  = ave(teen,hid,FUN=sum),
                    hh.income = ave(income,hid,FUN=sum),
                    bf.exp   = pmin(5,hh.child)*32 + pmin(2,hh.teen)*38,
                    state = v0001,
                    race = raca,
                    with.partner = sw(c010),
                    breast.milk = sw(l01701),
                    can.read = sw(d001),
                    attend.school = sw(d002),
                    school.grade = d006,
                    education = as.factor(vdd004),
                    school.before = sw(d008),
                    worked.ft = sw(e001),
                    worked.pt = sw(e003),
                    work.hours = e017,
                    work.sector = e014,
                    pension = f00102,
                    med.coverage = sw(i001),
                    srh = as.integer(as.numeric(j001)<3),
                    stopped.activity = sw(j002),
                    illness = sw(j007),
                    last.med.visit = j011,
                    hospitalized = sw(j037),
                    med.visits.12mths = j012,
                    last.den.visit = j013,
                    violence.unk = sw(o025),
                    violence.kn = sw(o037),
                    violence = as.integer((violence.unk + violence.kn)>0),
                    accident = sw(o009),
                    weight = as.numeric(p00101),
                    exercise = sw(p034),
                    alcohol = ifelse(p027=="3",1,ifelse(p027 %in% c("2","1"),0,NA)),
                    smoke = ifelse(p050=="3",0,ifelse(p050 %in% c("2","1"),1,NA)),
                    hbp = ifelse(q002=="1",1,ifelse(q002 %in% c("2","3"),0,NA)),
                    diabetes = ifelse(q030=="1",1,ifelse(q030 %in% c("2","3"),0,NA)),
                    hchol = sw(q060),
                    cardiac = sw(q063),
                    chronic = sw(q128),
                    asthma = sw(q074),
                    arthritis = sw(q079),
                    depression = sw(q092),
                    mental.illness = sw(q110),
                    cancer = sw(q120),
                    cancer.type=q121,
                    birth.control=sw(r034),
                    height=w00203,
                    house.type=ifelse(a001=="2",0,ifelse(a001 %in% c("1","3"),1,NA)),
                    floor=ifelse(a004 %in% as.character(c(1,7,2,3)),1,ifelse(a004 %in% as.character(4:6),0,NA)),
                    running.water = sw(a007),
                    bathroom = as.numeric(as.numeric(a014)>0),
                    fridge = sw(a01803),
                    dvd = sw(a01805),
                    car = as.numeric(as.numeric(a020)>0),
                    un.saude=ifelse(b001 %in% c("2","3"),0,1),
                    hh.people=as.numeric(v0022),
                    sweight=v00291.y,
                    salad = ifelse(p007==0,0,p007*as.numeric(p008)),
                    vegetables = ifelse(p009==0,0,p009*as.numeric(p010)),
                    redmeat = ifelse(p011==0,0,p011*as.numeric(p012)),
                    chicken = ifelse(p013==0,0,p013*as.numeric(p014)),
                    fish = p015,
                    juice = ifelse(p016==0,0,p016*as.numeric(p017)),
                    fruit = ifelse(p018==0,0,p018*as.numeric(p019)),
                    pop = ifelse(p020==0,0,p020*as.numeric(p021)),
                    sweets = p025,
                    junk.food=as.integer(p026>0),
                    fr.veg = salad+vegetables+juice+fruit,
                    meat = redmeat + chicken + fish,
                    sweet.pop = pop + sweets,
                    BMI = weight/(height/100)^2)

# SELECTING AND ORDERING VARIABLES ----------------------------
ds %<>% dplyr::select(region,state,hid,pid,type,female,age,everything())


# RECODING VARIABLES ------------------------------------------
ds$race %<>% dplyr::recode("Preta" = "black",
                           "Branca" = "white",
                           "Parda" = "mixed",
                           "Indígena" = "indigenous",
                           "amarela" = "asian")

ds$work.sector %<>% dplyr::recode("1" = "domestic",
                                  "2" = "military/police",
                                  "3" = "private",
                                  "4" = "public",
                                  "5" = "employer",
                                  "6" = "self-employed",
                                  "7" = "unpaid-self/employer",
                                  "8" = "unpaid-employee")

ds$type %<>% dplyr::recode("01" = "head",
                           "02" = "partnerd",
                           "03" = "partners",
                           "04" = "child",
                           "05" = "child.head",
                           "06" = "child.step",
                           "07" = "child.in.law",
                           "08" = "parent",
                           "09" = "parent.in.law",
                           "10" = "grandchild",
                           "11" = "greatgrandchild",
                           "12" = "sibling",
                           "13" = "grandparent",
                           "14" = "otherfamily",
                           "15" = "member.nosharing",
                           "16" = "member.sharing",
                           "17" = "pensioner",
                           "18" = "maid",
                           "19" = "maid.family")


# CHANGE NAs TO 0 in socprog
ds$socprog[is.na(ds$socprog)] <- 0
ds$pension[is.na(ds$pension)] <- 0

# POSSIBLE BF VALUES
bf <- expand.grid(0:1,0:5,0:2)[-1,] ; names(bf) <- c("level","child","teen")
bf$bf <- bf$level*70 + bf$child*32 + bf$teen*38

# Unique values of BF payments
bfu <- c("32, 38, 50, 64, 70, 72, 74, 76, 78, 80, 81, 82, 84, 86, 88, 90, 91, 92, 94, 
  96, 98, 100, 102, 103, 104, 106, 108, 110, 112, 114, 116, 117, 118, 119, 120, 
  122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 145, 146, 148, 
  149, 150, 152, 154, 156, 157, 158, 160, 162, 164, 166, 168, 170, 172, 174, 
  176, 177, 178, 180, 182, 184, 186, 188, 189, 190, 192, 194, 196, 198, 200, 
  202, 204, 206, 208, 209, 210, 212, 214, 216, 218, 220, 222, 224, 226, 228, 
  230, 232, 234, 236, 238, 240, 242, 244, 246, 248, 250, 252, 254, 256, 258, 
  260, 261, 262, 264, 266, 268, 270, 272, 274, 276, 278, 280, 281, 282, 284, 
  286, 288, 290, 292, 294, 296, 298, 300, 302, 304, 306, 308, 310, 312, 314, 
  316, 318, 320, 322, 324, 326, 328, 330, 332, 334, 336, 338, 340, 342, 344, 
  346, 348, 350, 352, 354, 356, 358, 360, 362, 364, 366, 368, 370, 372, 374, 
  376, 378, 380, 382, 384, 386, 388, 390, 392, 394, 396, 398, 400")

ds$bf <- ifelse((ds$socprog %in% bfu) & ds$hh.pci.all<1000 ,1,0)
ds$hh.bf <- ave(ds$bf,ds$hid,FUN=max)

# GROUP LEVEL VARIABLES
state <- read.csv("statedata.csv")

ds$region2 <- as.numeric(as.factor(ds$region))
ds$state2 <- as.numeric(as.factor(ds$state))
region <- read.csv("regiondata.csv")
names(region)[-1] <- paste0("region.",names(region)[-1])

ds %<>% join(state,by="state")
ds %<>% join(region,by="region2")

# SCALING
ds$age.c <- scale(ds$age)
ds$hh.pci.c <- scale(ds$hh.pci.all)
ds$hh.people.c <- scale(ds$hh.people)
ds$IMR.c <- scale(ds$IMR)
ds$urban.c <- scale(ds$urban)
ds$no.running.water.c <- scale(ds$no.running.water)
ds$income.other.c <- scale(ds$income.other)
ds$school.lt9.c <- scale(ds$school.lt9)

ds$education2 <- revalue(ds$education, c("1"="low", "2"="med","3"="med","4"="med","5"="high","6"="high","7"="high"))
ds$education2[ds$education2 %in% 2] <- "low"
ds$education2[ds$education2 %in% 3:5] <- "med"
ds$education2[ds$education2 %in% 6:8] <- "high"
ds$education2 <- as.factor(ds$education2)
ds$education2 <- droplevels(ds$education2)

revalue(data$sex, c("M"="1", "F"="2"))

ds$race %<>% dplyr::recode_factor(white="white",black="black",mixed="mixed",.default="mixed")
ds <- ds[!is.na(ds$race),]
ds$subgr <- substr(ds$v0024,1,5)
ds$med.visit.lastyear <- ifelse(is.na(ds$med.visits.12mths),0,1)
pes_sel <- subset( ds , m001 == "1" & !is.na(race))
ds <- pes_sel



library(survey)
# pre-stratified design
pes_sel_des <- svydesign(
  id = ~ upa_pns ,
  strata = ~ v0024 ,
  data = pes_sel ,
  weights = ~ pre_pes_long ,
  nest = TRUE
)

# post-stratified design
pes_sel_des$variables$education2 <- droplevels(pes_sel_des$variables$education2)
post_pop <- unique( pes_sel[ c( 'v00293.y' , 'v00292.y' ) ] )
names( post_pop ) <- c( "v00293.y" , "Freq" )
pes_sel_des_pos <- postStratify( pes_sel_des , ~v00293.y , post_pop )
pes_sel_des_pos$variables$education2 <- droplevels(pes_sel_des_pos$variables$education2)


library(R2jags)
library(gplots)

main.analysis <- function(y,w=NULL,group,ss) {
  
  if (y %in% c("weight","height","srh","BMI")) {
    fam <<- "gaussian"
  } else if (y %in% c("fr.veg","meat","sweet.pop")) {
    fam <<- "quasipoisson"
  } else {
    fam <<- "quasibinomial"
  }
  
  # FOR SUBSETTING DATA
  if (is.null(ss)) {
    survey.object <- pes_sel_des_pos
  } else {
    survey.object <- subset(pes_sel_des_pos,hh.pci.all<=ss)
  }
  
  if (ss==0 & !is.null(w)) {
    w <- w[w!="hh.pci.all"]
  }
  
  # MAKE FORMULAS
  if (is.null(w)) {
    form.g <- paste0(y," ~ 1")
  } else {
    form.g <- paste0(y," ~ ",paste0(w,collapse=" + "))
    
  }
  
  # ESTIMATE MODELS
  g <- suppressWarnings(svyglm(form.g,design=survey.object,family=fam))
  svypm <- suppressWarnings(svypredmeans(g, ~interaction(hh.bf,get(group))))
  svypm.m <- suppressWarnings(svypredmeans(g, ~hh.bf))
  m.con <- svycontrast(svypm.m, quote(`1`-`0`))
  
  group.names <- as.character(unique(survey.object$variables[,group]))
  
  s1 <- expand.grid(1:0,group.names)
  s2 <- paste0(s1[,1],".",as.character(s1[,2]))
  
  mle <- as.data.frame(do.call(rbind,lapply(group.names,FUN=function(x) {
    if (x=="Mato Grosso") {
      con <- s2[grepl("Mato Grosso$",s2)]
    } else {
      con <- s2[grepl(x,s2)]
    }
    qu <- paste0("`",con[1],"`-`",con[2],"`")
    qu <- as.quoted(qu)
    est. <- svycontrast(svypm,qu)
    est. <- c(est.,sqrt(attributes(est.)$var))
    return(est.)
  }))) ; names(mle) <- c("est","se") ; row.names(mle) <- group.names
  
  mle$li <- mle$est - 1.96*mle$se
  mle$ui <- mle$est + 1.96*mle$se
  
  # SHRINKAGE-------------------------------------------------------
  
  # DATA FOR JAGS
  est <- mle$est
  V <- mle$se^2
  Nstud <- length(est)
  
  data.jags <- list(y=est,V=V,Nstud=Nstud)
  modelstring = "
  
  model 
  { 
  for (i in 1:Nstud)
  {
  P[i] <- 1/V[i]
  y[i] ~ dnorm(delta[i], P[i]) #study effects
  delta[i] ~ dnorm(mu, prec)    #random level effects between studies
  }   
  mu ~ dnorm(0, 1.0E-5)
  
  tau~dunif(0,10)
  tau.sq<-tau*tau
  prec<-1/(tau.sq)
  
  }
  "
  
  writeLines(modelstring, con="jags/pooling.model.txt")
  
  # RUN JAGS
  q <- jags(data=data.jags,model.file = "jags/pooling.model.txt", parameters.to.save = c("mu","delta","tau.sq"),n.chains = 3,n.iter = 5000,n.burnin = 1000)
  
  
  # PREPARE FOR PLOTTING
  theta <- q$BUGSoutput$summary[grepl("delta",row.names(q$BUGSoutput$summary)),c("mean","2.5%","97.5%")]
  row.names(theta) <- group.names
  theta <- as.data.frame(theta) ; names(theta) <- c("est","li","ui")
  
  oamean <- q$BUGSoutput$summary[grepl("mu",row.names(q$BUGSoutput$summary)),c("mean","2.5%","97.5%")]
  
  # PLOTTING RESULTS
  
  ## LABELS
  plot.name <- ifelse(is.null(ss),paste0(group,"-",y," (",paste0(w,collapse=", "),")"),paste0(ss,"-",group,"-",y," (",paste0(w,collapse=", "),")"))
  
  labs <- data.frame(y=c("med.coverage","hospitalized","med.visit.lastyear","un.saude","accidents","violence.kn","violence.unk","height","weight","BMI","srh","smoke","alcohol","exercise","running.water","fr.veg","meat","sweet.pop","junk.food","hbp","diabetes","hchol","cardiac","asthma","arthritis","depression","mental.illness","cancer","def.any"),
                     labs=c("Medical coverage","Hospitalized","Medical visits in last 12 months","Community health worker","Accidents","Violence (known)","Violence (unknown)","Height (cm)","Weight (kg)","BMI","Self-rated health","Smoking","Alcohol","Exercise","Running water","Fruits and vegetables (servings)","Meat (servings)","Sweets and pop (servings)","Junk food","High blood pressure","Diabetes","High cholesterol","Cardiac problem","Asthma","Arthritis","Depression","Mental illness","Cancer","Handicap"))
  
  
  
  ## PLOT
  png(paste0("graphs/",plot.name,".png"))
  num.groups <- length(group.names)
  shift <- ifelse(group=="UF",0.1,0.05)
  par(mar=c(9,4,4,2)+0.1)
  plot(1,1,xlim=c(1-shift,num.groups+shift),ylim=c(min(mle$li),max(mle$ui)),type="n",xlab="",ylab=labs$labs[labs$y==y],xaxt="n",main=NULL)
  rect(-1,oamean[2],100,oamean[3],col = "lightgray",border = F)
  abline(h=oamean[1],lty=2,col="darkgray")
  abline(h=0,col="black",lty=1)
  plotCI((1:num.groups)-shift,mle$est,ui=mle$ui,li=mle$li,pch=16,sfrac=0.000,add=T)                        ## MLE
  plotCI((1:num.groups)+shift,theta$est,ui=theta$ui,li=theta$li,col="red",pch=16,sfrac=0.000,add=T)      ## Empirical Bayes
  legend("bottomleft",legend = c("MLE","Empirical Bayes"),col=c("black","red"),lty = 1,bty="n")
  axis(1, at=1:num.groups, labels=group.names,las=2)
  dev.off()
  
  svypm2 <- as.data.frame(cbind(svypm,diag(sqrt((attributes(svypm)$var))))) ; names(svypm2) <- c("est","se")
  svypm2$li <- svypm2$est - 1.96*svypm2$se
  svypm2$ui <- svypm2$est + 1.96*svypm2$se
  p0 <- svypm2[grepl("0.",row.names(svypm2)),]
  p1 <- svypm2[grepl("1.",row.names(svypm2)),]
  
  
  
  num.groups <- length(group.names)
  shift <- ifelse(group=="UF",0.1,0.05)
  png(paste0("graphs/",plot.name,"predicted_probabilities.png"))
  par(mar=c(9,4,4,2)+0.1)
  plot(1,1,xlim=c(1-shift,num.groups+shift),ylim=c(min(svypm2$li),max(svypm2$ui)),type="n",xlab="",ylab="Prevalence",xaxt="n")
  plotCI((1:num.groups)-shift,p0$est,ui=p0$ui,li=p0$li,pch=16,sfrac=0.000,add=T)                        ## MLE
  plotCI((1:num.groups)+shift,p1$est,ui=p1$ui,li=p1$li,col="red",pch=16,sfrac=0.000,add=T)      ## Empirical Bayes
  legend("topleft",legend = c("No Bolsa Familia","Bolsa Familia"),col=c("black","red"),lty = 1,bty="n")
  axis(1, at=1:num.groups, labels=group.names,las=2)     
  dev.off()
  
  return(list(mle=mle,m.con=m.con,eb=theta,overall=oamean,tausq=q$BUGSoutput$summary[grepl("tau.sq",row.names(q$BUGSoutput$summary)),c("mean","2.5%","97.5%")]))
  
}

# Number of cores
ncores <- 1


# RUN the function main.analysis over all combinations of outcomes, subsets, 
# control variables and state/region
outcomes <- c("med.coverage","hospitalized","med.visit.lastyear","un.saude",
              "height","weight","BMI","srh",
              "smoke","alcohol","exercise","fr.veg","meat","sweet.pop","junk.food",
              "hbp","diabetes","hchol","cardiac","asthma","arthritis","depression","mental.illness","cancer","def.any")
subsets <- list(0,500)
w.list <- list(c("female","hh.people","urban","age","education2","race","hh.pci.all"),NULL)
groups <- c("UF","region")

out. <- mclapply(outcomes,FUN=function(y1) {
  res. <- lapply(w.list,FUN=function(w1) {
    ss. <- lapply(subsets,function(ss1) {
      
      print(c(y1,w1,ss1))
      result. <- tryCatch(main.analysis(y=y1,w=w1,group="UF",ss=ss1), error=function(e) NULL)
      return(result.)
      
    }) ; names(ss.) <- c("zero","five")
    return(ss.)
  }) ; names(res.) <- c("adjusted","unadjusted")
  return(res.)
},mc.cores = ncores) ; names(out.) <- outcomes
