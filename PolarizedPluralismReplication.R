#################################################################################
####### REPLICATION MATERIALS FOR TABLES, FIGURES, AND IN-TEXT STATISTICS #######
############################# POLARIZED PLURALISM ###############################
####################### CROSSON, FURNAS, & LORENZ 2020 ##########################
#################################################################################


rm(list=ls())
library(tidyverse)


###### LOAD DATA #####

crp <- read_csv("Data/CRP_Categories.csv")
nominate <- read_csv("Data/HSall_members.csv") %>% filter(congress >108)
groups_1 <- read_csv("Data/group_fullchains_final.csv")

## NOTE: All code used to prepare data for estimation, along with some descriptive statistics listed in the text,
## can be found in data_cleaning_and_descriptives_rep.R



###### FIGURE 1 #####

gun_test <- filter(groups_1, orgname ==  "National Rifle Association" |orgname ==  "Gun Owners of America") %>% dplyr::select( orgname, estimate) %>% unique()

doctors_test <- filter(groups_1, orgname ==  "American College of Physicians" |orgname ==  "Association of American Physicians and Surgeons") %>% dplyr::select( orgname, estimate) %>% unique()

lawyers_test <- filter(groups_1, orgname ==  "American Center for Law and Justice" |orgname ==  "American Bar Association" | orgname == "American Association for Justice") %>% dplyr::select( orgname, estimate) %>% unique()

enviro_test <- filter(groups_1, orgname ==  "Greenpeace" |orgname ==  "Ducks Unlimited" ) %>% dplyr::select( orgname, estimate) %>% unique()

labor_test <- filter(groups_1, orgname ==  "International Brotherhood of Teamsters" |orgname ==  "American Federation of State, County and Municipal Employees") %>% dplyr::select( orgname, estimate) %>% unique()

jewish_test <- filter(groups_1, orgname ==  "American Israel Public Affairs Committee" |orgname ==  "American Jewish Committee") %>% dplyr::select( orgname, estimate) %>% unique()

womens_test <- filter(groups_1, orgname ==  "Independent Women's Forum" |orgname ==  "Feminist Majority Foundation") %>% dplyr::select( orgname, estimate) %>% unique()

#business_test$type <- "Business Association"
gun_test$type <- "Gun Rights"
doctors_test$type <- "Medical Association"
lawyers_test$type <- "Lawyers Association"
enviro_test$type <- "Environmental Organization"
labor_test$type <- "Labor Union"
jewish_test$type <- "Pro-Israel"
womens_test$type <- "Women's Issues"

left_predicts <-c("Greenpeace", "American Association for Justice", "American College of Physicians", "National Rifle Association", "Feminist Majority Foundation", "U.S. Chamber of Commerce", "American Federation of State, County and Municipal Employees", "American Jewish Committee")
test_orgs <- bind_rows(jewish_test, womens_test, gun_test, doctors_test, lawyers_test, enviro_test, labor_test)
test_orgs$predict_left <- 0
test_orgs$predict_left[test_orgs$orgname %in% left_predicts] <- 1

library(ggrepel)

testorg_plot <- ggplot(test_orgs, aes(x=type, y=estimate, group=orgname, fill=as.factor(predict_left))) +theme_classic() + 
  geom_violin(width=1,position= position_dodge(width = .5),draw_quantiles = .5, linetype = 1, aes(y=estimate, x = type, fill=as.factor(predict_left))) + 
  stat_summary(fun.y=median, geom="point", size=1.5, position=position_dodge(width = .5), color="black") + 
  coord_flip() + scale_fill_manual(values = c("#137B80", "#8E6C8A")) + theme(legend.position="none")


ggsave("testorg_plot.pdf", testorg_plot, device="pdf") 
 
#text labels added to the plot manually


##### FIGURE 2 #######

chambers <- c("California Chamber of Commerce", "National Black Chamber of Commerce", "National Gay & Lesbian Chamber of Commerce", "U.S. Chamber of Commerce", "U.S. Hispanic Chamber of Commerce", "U.S. Women's Chamber of Commerce")

chambers_dat <- filter(groups_1, orgname %in% chambers)
chambers_dat$orgname <- fct_reorder(chambers_dat$orgname, chambers_dat$estimate, .fun=median)

cham_plot <- ggplot(chambers_dat, aes(x=reorder(orgname, estimate, median), y=estimate)) +theme_classic() + 
  geom_violin(width=1,position= position_dodge(width = .5),draw_quantiles = .5, linetype = 1, aes(y=estimate, x = orgname), fill="grey") +
  coord_flip() + xlab("") +  scale_x_discrete(limits = c("U.S. Women's Chamber of Commerce", "National Gay & Lesbian Chamber of Commerce", "U.S. Hispanic Chamber of Commerce", "California Chamber of Commerce", "National Black Chamber of Commerce", "U.S. Chamber of Commerce")) +
  stat_summary(fun.y=median, geom="point", size=3, position=position_dodge(width = .5), color="black")

ggsave("chambers.pdf", cham_plot, device="pdf")



##### FIGURE 3 ######

(IGvMOV_dens <- ggplot(groups_1, aes(x=estimate, fill=as.factor(respondent_type))) +geom_density(alpha=.5, color=NA) +theme_classic() + scale_fill_manual(values = c("#E3BA22", "#684664")) )
ggsave("IGvMOV_dens.pdf", IGvMOV_dens, device="pdf")



##### FIGURE 4 #######

leg <- groups_1 %>% filter(respondent_type == "Legislator")
nominate$icpsr <- as.character(nominate$icpsr)
leg <- leg %>% left_join(unique(dplyr::select(nominate, icpsr, party_code)), by=c("orgname" = "icpsr"))

D_med <- median(filter(leg, party_code == 100)$estimate)

R_med <- median(filter(leg, party_code == 200)$estimate)

library(ggridges)
library(forcats)
cat_keeps <- c("Corporations", "Health", "Education", "Occupational Associations","Trade and Other Business Associations", "Socail Welfare or Poor", "Public Interest", "Identity Groups", "Unions")
(SVB <- ggplot(filter(groups_1, respondent_type == "Organization", usecode %in% cat_keeps), aes( x=estimate, y=forcats::fct_reorder(usecode, estimate, .fun = median))) +theme_classic(base_size = 16)  + 
    geom_density_ridges() +xlim(-4,4)  +
    ylab("") + xlab("IGscore")  + geom_vline(xintercept=D_med, linetype=6) +  geom_vline(xintercept=R_med,linetype=6)
)

ggsave("SVB_joyplot.pdf", SVB, device="pdf") 



##### SECTOR SPECIFIC FIGURE DATA PREP ######

group_types <- read_csv("Data/Group_Types.csv")

grouptype_counts <- group_by(group_types, Catname) %>% summarise(num_groups = length(unique(orgname))) %>% arrange(desc(num_groups))
three_groups <- unique(filter(grouptype_counts, num_groups >2)$Catname)

IGscore_groups_catname <- groups_1 %>% dplyr::filter(Catname %in% three_groups)
IGscores_sector <- filter(IGscore_groups, Sector != "Ideology/Single-Issue" & Sector != "Party Cmte" & Sector != "Unknown" & Sector != "Other" & Sector != "Non-contribution") %>% dplyr::select(orgname,estimate, Sector) %>% unique()

IGscores_singleissue <- filter(IGscore_groups_catname, Sector == "Ideology/Single-Issue") %>% dplyr::select(orgname,estimate, Catname) %>% unique()
IGscores_Other <- filter(IGscore_groups_catname, Sector == "Other" ) %>% dplyr::select(orgname,estimate, Catname) %>% unique()
IGscores_Labor<- filter(IGscore_groups_catname, Sector == "Labor" ) %>% dplyr::select(orgname,estimate, Catname) %>% unique()
IGscores_Health <- filter(IGscore_groups_catname, Sector == "Health")  %>% dplyr::select(orgname,estimate, Catname) %>% unique()


###### FIGURE 5 #####

sec_dens <- ggplot(IGscores_sector, aes(x=estimate, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Sector) +theme_classic() +ylab("Scaled Density") 
ggsave("sec_dens.pdf", sec_dens, device="pdf")


###### PERCENT CLOSER TO DEMS BY SVB GROUP, INDUSTRY (INCL. TABLE 2) ######

groups_1$closer_d <- ifelse(abs(groups_1$estimate - -0.8077483) < abs(groups_1$estimate - 1.0087249), 1, 0)

# Percentage of iterations closer to dem than republican
length(subset(groups_1, closer_d == 1 & respondent_type == "Organization")$orgname)/length(subset(groups_1, respondent_type == "Organization")$orgname)


# Locations of modes (CODE TAKEN FROM https://stackoverflow.com/a/39855416)
require(dplyr)
require(tidyr)
get.modes2 <- function(x,bw,signifi) {  
  den <- density(x, kernel=c("gaussian"),bw=bw)
  den.s <- smooth.spline(den$x, den$y, all.knots=TRUE, spar=0.1)
  s.1 <- predict(den.s, den.s$x, deriv=1)
  s.0 <- predict(den.s, den.s$x, deriv=0)
  den.sign <- sign(s.1$y)
  a<-c(1,1+which(diff(den.sign)!=0))
  b<-rle(den.sign)$values
  df<-data.frame(a,b)
  df = df[which(df$b %in% -1),]
  modes<-s.1$x[df$a]
  density<-s.0$y[df$a]
  df2<-data.frame(modes,density)
  df2$sig<-signif(df2$density,signifi)
  df2<-df2[with(df2, order(-sig)), ] 
  print(df2)
  df<-as.data.frame(df2 %>% 
                      mutate(m = min_rank(desc(sig)) ) %>% #, count = sum(n)) %>% 
                      group_by(m) %>% 
                      summarize(a = paste(format(round(modes,2),nsmall=2), collapse = ',')) %>%
                      spread(m, a, sep = ''))
  colnames(df)<-paste0("m",1:length(colnames(df)))
  print(df)
}

modes_orgs<-get.modes2(subset(groups_1, respondent_type == "Organization")$estimate,bw.nrd0(subset(groups_1, respondent_type == "Organization")$estimate),2)
modes_rs<-get.modes2(subset(full, party_code == 200)$estimate,bw.nrd(subset(full, party_code == 200)$estimate),2)


## CLOSER TO DEMS, BY INDUSTRY (table 2)

groups_1$business <- ifelse(groups_1$usecode == "Trade and Other Business Associations" | groups_1$usecode == "Corporations" | groups_1$usecode == "Occupational Associations", 1, 0)
closer_d_industries <- NA
for(i in unique(groups_1$Sector.Long)){
  share <-length(subset(groups_1, closer_d == 1 & business == 1 & Sector.Long == i)$orgname)/length(subset(groups_1, Sector.Long == i)$orgname)
  industry <- i
  closer_d_industries <- rbind(closer_d_industries, c(industry, share))
}
closer_d_industries


## CLOSER TO DEMS BY SVB BUSINESS CATEGORY

modes_trades <-get.modes2(subset(groups_1, usecode == "Trade and Other Business Associations")$estimate,bw.nrd0(subset(groups_1, usecode == "Trade and Other Business Associations")$estimate),2)
# 0.615
modes_corporations <-get.modes2(subset(groups_1, usecode == "Corporations")$estimate,bw.nrd0(subset(groups_1, usecode == "Corporations")$estimate),2)
# 0.380
modes_occupational<-get.modes2(subset(groups_1, usecode == "Occupational Associations")$estimate,bw.nrd0(subset(groups_1, usecode == "Occupational Associations")$estimate),2)
# 0.353

length(subset(groups_1, closer_d == 1 & usecode == "Trade and Other Business Associations")$orgname)/length(subset(groups_1, usecode == "Trade and Other Business Associations")$orgname)
length(subset(groups_1, closer_d == 1 & usecode == "Corporations")$orgname)/length(subset(groups_1, usecode == "Corporations")$orgname)
length(subset(groups_1, closer_d == 1 & usecode == "Occupational Associations")$orgname)/length(subset(groups_1, usecode == "Occupational Associations")$orgname)



##### FIGURE 6 #####

si_dens <- ggplot(IGscores_singleissue, aes(x=estimate, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density")  + coord_cartesian(xlim = c(-3.8317, 2.5902)) 
ggsave("si_dens.pdf", si_dens, device="pdf")




##########################################
###### INTENSITY/WEIGHTED ANALYSIS #######
##########################################


IGestimates_long <- read_csv("Data/ScoreswithWeights2.4.2019.csv")
IGestimates_long <- as_tibble(IGestimates_long)
IGestimates_long$weight[is.na(IGestimates_long$weight)] <- 0


#### GET MODES FUNCTION ##### 

library(spatstat)

###### POSITION-TAKING ACTIVITY ######

position_activity<- filter(IGestimates_long, weight_type == "pos_weight"  | weight_type == "log_pos_weight" | weight_type == "unweighted")
position_activity$weight_type[position_activity$weight_type == "pos_weight"] <- "Number of Positions"
position_activity$weight_type[position_activity$weight_type == "unweighted"] <- "Unweighted"
position_activity$weight_type[position_activity$weight_type == "log_pos_weight"] <- "Logged Number of Positions"

position_activity$weight_type <- fct_relevel(position_activity$weight_type, "Unweighted", "Number of Positions", "Logged Number of Positions")

###### FIGURE 7 #######
pa <- ggplot(position_activity, aes(x=estimate, y = ..scaled.., weight = weight)) +geom_density(fill = "#666666", alpha=.7) + theme_classic() +
  facet_wrap(~weight_type, ncol=1) +xlab("IGScore") + ylab("Scaled Density") +xlim(-4,4) +geom_vline( xintercept = D_med, linetype = "dashed") +
  geom_vline( xintercept = R_med, linetype = "dashed")
ggsave( "position_activity.pdf",pa, device = "pdf", width = 5.5, height = 3, units = "in")


#Position-Taking Mode
positiontaking <- filter(IGestimates_long, weight_type == "pos_weight")
positiontakingmode <-get.modes2(positiontaking$estimate, bw.nrd0(positiontaking$estimate),2, weight = positiontaking$weight)
positiontakingmode

#Log Position-Taking Mode
logpositiontaking <- filter(IGestimates_long, weight_type == "log_pos_weight")
logpositiontakingmode <-get.modes2(logpositiontaking$estimate, bw.nrd0(logpositiontaking$estimate),2, weight = logpositiontaking$weight)
logpositiontakingmode

#Position taking percent closer to democrats
library(Hmisc)
e<- wtd.Ecdf(positiontaking$estimate, weights=positiontaking$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
ptd <- e$ecdf[which.min(abs(e$x - psplit))]

#Log position taking percent closer to democrats
e<- wtd.Ecdf(logpositiontaking$estimate, weights=logpositiontaking$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
lpdd <- e$ecdf[which.min(abs(e$x - psplit))]

##### CF activity

CF_activity<- filter(IGestimates_long, weight_type == "PAC_weight"  | weight_type == "log_PAC_weight" | weight_type == "unweighted" | weight_type == "hasPAC")
CF_activity$weight_type[CF_activity$weight_type == "PAC_weight"] <- "PAC Contributions"
CF_activity$weight_type[CF_activity$weight_type == "log_PAC_weight"] <- "Logged PAC Contributions"
CF_activity$weight_type[CF_activity$weight_type == "unweighted"] <- "Unweighted"
CF_activity$weight_type[CF_activity$weight_type == "hasPAC"] <- "Has a PAC"


CF_activity$weight_type <- fct_relevel(CF_activity$weight_type, "Unweighted", "Has a PAC", "PAC Contributions", "Logged PAC Contributions")

###### FIGURE 8 #######
pa_g <- ggplot(CF_activity, aes(x=estimate, y = ..scaled.., weight = weight/sum(weight))) +geom_density(fill = "#666666", alpha=.7) + theme_classic() +
  facet_wrap(~weight_type, ncol=1) +xlab("IGScore") + ylab("Scaled Density") +xlim(-4,4) +geom_vline( xintercept = D_med, linetype = "dashed") +
  geom_vline( xintercept = R_med, linetype = "dashed")
ggsave( "CF_activity.pdf",pa_g, device = "pdf", width = 5.5, height = 3.5, units = "in")


#CF spending modes
CF_spending <- filter(IGestimates_long, weight_type == "PAC_weight")
CF_spendingmode <-get.modes2(CF_spending$estimate, bw.nrd0(CF_spending$estimate),2, weight = CF_spending$weight)
CF_spendingmode

#CF Spending Stats
CF_spending_zero <- CF_spending %>% filter(weight == 0)
num_noncontrib_orgs <- length(unique(CF_spending_zero$orgname))
CF_spending_nonzero <- CF_spending %>% filter(weight != 0)

##number of contributing organizations
num_contrib_orgs <- length(unique(CF_spending_nonzero$orgname))

##share of contributing organizations
num_contrib_orgs/(num_contrib_orgs + num_noncontrib_orgs)



#CF logspending mode
logCF_spending <- filter(IGestimates_long, weight_type == "log_PAC_weight")
logCF_spendingmode <-get.modes2(logCF_spending$estimate, bw.nrd0(logCF_spending$estimate),2, weight = logCF_spending$weight)
logCF_spendingmode

e<- wtd.Ecdf(CF_spending$estimate, weights=CF_spending$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
cfd <- e$ecdf[which.min(abs(e$x - psplit))]

e<- wtd.Ecdf(logCF_spending$estimate, weights=logCF_spending$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
lcfd<- e$ecdf[which.min(abs(e$x - psplit))]

e<- wtd.Ecdf(CF_spending_zero$estimate, weights=rep(1, nrow(CF_spending_zero)), normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
ncfd <- e$ecdf[which.min(abs(e$x - psplit))]
#Percent of non contributing organizations closer to republicans
1-ncfd

e<- wtd.Ecdf(CF_spending_nonzero$estimate, weights=rep(1, nrow(CF_spending_nonzero)), normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
ycfd <- e$ecdf[which.min(abs(e$x - psplit))]

#Percent of contributing organizations closer to republicans
1-ycfd
###### FIGURE 9 ###### 
#SEE FULL CF ANALYSIS BELOW


###### LOBBYING ACTIVITY ######
log_lobbying <- filter(IGestimates_long, weight_type == "lobbys" | weight_type == "log_spending_weight" | weight_type == "spending_weight" | weight_type == "issue_spending_weight" |weight_type == "unweighted")
log_lobbying$weight_type[log_lobbying$weight_type == "log_spending_weight"] <- "Logged Lobbying Spending"
log_lobbying$weight_type[log_lobbying$weight_type == "unweighted"] <- "Unweighted"
log_lobbying$weight_type[log_lobbying$weight_type == "spending_weight"] <- "Lobbying Spending"
log_lobbying$weight_type[log_lobbying$weight_type == "issue_spending_weight"] <- "Lobbying Spending/Issue Portfolio"
log_lobbying$weight_type[log_lobbying$weight_type == "lobbys"] <- "Engages in Lobbying"


log_lobbying$weight_type <- fct_relevel(log_lobbying$weight_type, "Unweighted", "Engages in Lobbying", "Lobbying Spending", "Logged Lobbying Spending","Lobbying Spending/Issue Portfolio" )

####### FIGURE 10
lls <- ggplot(log_lobbying, aes(x=estimate, y = ..scaled.., weight = weight)) +geom_density(fill = "#666666", alpha=.7) + theme_classic() +
  facet_wrap(~weight_type, ncol=1) +xlab("IGScore") + ylab("Scaled Density") +xlim(-4,4) +geom_vline( xintercept = D_med, linetype = "dashed") +
  geom_vline( xintercept = R_med, linetype = "dashed")
ggsave( "logged_lobbying.pdf",lls, device = "pdf", width = 5.5, height = 4, units = "in") 


#Lobbying Modes
lobbying <- filter(IGestimates_long, weight_type == "spending_weight")
lobbyingmode <-get.modes2(lobbying$estimate, bw.nrd0(lobbying$estimate),2, weight = lobbying$weight)
lobbyingmode

#Logged Lobbying Modes
log_lobbying <- filter(IGestimates_long, weight_type == "log_spending_weight")
log_lobbyingmode <-get.modes2(log_lobbying$estimate, bw.nrd0(log_lobbying$estimate),2, weight = log_lobbying$weight)
log_lobbyingmode

#Issue Lobbying Modes
issue_lobbying <- filter(IGestimates_long, weight_type == "issue_spending_weight")
issue_lobbyingmode <-get.modes2(issue_lobbying$estimate, bw.nrd0(issue_lobbying$estimate),2, weight = issue_lobbying$weight)

#Medians
weighted.median(lobbying$estimate, lobbying$weight)
weighted.median(log_lobbying$estimate, log_lobbying$weight)

#Percent closer to Dems
psplit <-  (D_med + R_med)/2

unweighted <- filter(IGestimates_long, weight_type == "unweighted")
e<- wtd.Ecdf(unweighted$estimate, weights=unweighted$weight, normwt=F, na.rm=TRUE)

ud <- e$ecdf[which.min(abs(e$x - psplit))]

e<- wtd.Ecdf(log_lobbying$estimate, weights=log_lobbying$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
lld <- e$ecdf[which.min(abs(e$x - psplit))]

e<- wtd.Ecdf(lobbying$estimate, weights=lobbying$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
ld <- e$ecdf[which.min(abs(e$x - psplit))]

e<- wtd.Ecdf(issue_lobbying$estimate, weights=issue_lobbying$weight, normwt=F, na.rm=TRUE)
which.min(abs(e$x - psplit)) 
ild <- e$ecdf[which.min(abs(e$x - psplit))]

dm_perc <- c(ud, ptd, lpdd, ld, lld, ild, cfd, lcfd)
dm_perc_lab <- c("Unweighted", "Number of Positions", "Logged Number of Positions","Lobbying Spending", "Logged Lobbying Spending", "Lobbying Spending/Issue Portfolio", "PAC Contributions", "Logged PAC Contributions" )

##### Percent closer to democrats for each joint posterior distribution
dmperc_tab <- cbind(dm_perc_lab, dm_perc)
print(xtable(dmperc_tab), include.rownames = F)


###### FULL CF ANALYSIS ######


CF_match2 <- read_csv("Data/CF_match.csv")

matched <- CF_match2 %>% group_by(orgname) %>% summarise(num=n()) %>% filter(num>1)

CF_match2 <- CF_match2 %>% left_join(matched)

#### SUPPLEMENTARY INFORMATION FIGURE 1
(full_dist <- ggplot(CF_match2, aes(x=score, y=..scaled.., group=score_type, fill=score_type)) +geom_density(alpha=.5) +theme_classic() +ylab("Scaled Density"))
ggsave("full_dist.pdf", full_dist, device="pdf")


#### FIGURE 9 #####
(matched_dist <- ggplot(filter(CF_match2, num > 1, !is.na(num)), aes(x=score, y=..scaled.., group=score_type, fill=score_type)) +geom_density(alpha=.5) +theme_classic() +ylab("Scaled Density")
)
ggsave("matched_dist.pdf", matched_dist, device="pdf")



matchorgs <- filter(CF_match2, num > 1, !is.na(num))
library(Matching)

#### Bootstrapped KS test of IGscores and a T-test for footnote 21 #######

ks.boot(CF_spending_nonzero$estimate, CF_spending_zero$estimate, nboots=1000, alternative = c("two.sided","less","greater"),
        print.level=0)

t.test(CF_spending_nonzero$estimate, CF_spending_zero$estimate)

###### Bimodality Score ######
library(boot)
library(e1071) 
kurt_bs <- function(data, indices) {
  d <- as.matrix(data)[indices,] # allows boot to select sample 
  k <- e1071::kurtosis(d, type=2)
  return(k)
} 

skew_bs <- function(data, indices) {
  d <- as.matrix(data)[indices,] # allows boot to select sample 
  s <- skewness(d)
  return(s)
} 

# bootstrapping with 1000 replications 
set.seed(666)

boot_k_IG <- boot(data=filter(CF_match2, score_type == "IGScore")$score, statistic=kurt_bs, 
                  R=1000)

boot_k_CF <- boot(data=filter(CF_match2, score_type == "CFScore.irt")$score, statistic=kurt_bs, 
                  R=1000)

boot_s_IG <- boot(data=filter(CF_match2, score_type == "IGScore")$score, statistic=skew_bs, 
                  R=1000)

boot_s_CF <- boot(data=filter(CF_match2,  score_type == "CFScore.irt")$score, statistic=skew_bs, 
                  R=1000)

BC <- function(k,s,n){
  
  n_corrected <- (3*(n-1)^2)/((n-2)*(n-3))
  b <- (s * s +1) / (k + n_corrected)
  return(b)
}

IG_bc <- BC(boot_k_IG$t, boot_s_IG$t, n = nrow(filter(CF_match2, score_type == "IGScore")))

CF_bc <- BC(boot_k_CF$t, boot_s_CF$t, n = nrow(filter(CF_match2, score_type == "CFScore.irt")))

library(overlapping)

overlap <- overlap(list(IG_bc, CF_bc), nbins = 100)

test <- IG_bc - CF_bc
percentile_test <- ecdf(test)

#share of simulations where the bimodality coefficient is larger for CF scorezs than it is for IG scores
percentile_test(0)


#IGScore Modes
mm <- filter(matchorgs, score_type == "IGScore")$score
IGmodes<-get.modes2(mm, bw.nrd0(mm),2, weight = rep(1, length(mm)))
IGmodes

#CFcore Modes
mm <- filter(matchorgs, score_type == "CFScore.irt")$score
CFmodes<-get.modes2(mm, bw.nrd0(mm),2, weight = rep(1, length(mm)))
CFmodes

library(rjags)
library(BEST)

##### SUPPLEMENTAL INFORMATION FIGURE 10 #####
BESTout <- BESTmcmc(filter(CF_match2, num > 1, !is.na(num), score_type == "IGScore")$score, filter(CF_match2, num > 1, !is.na(num), score_type == "CFScore.irt")$score, parallel=FALSE)
bplot <- plotAll(BESTout)
ggsave("Diff_Means_CFIG.pdf", bplot, device="pdf")


##### SUPPLEMENTAL INFORMATION FIGURE 11 #####

IGCF_scatter.IG <- filter(CF_match2, num > 1, !is.na(num), score_type == "IGScore") %>% dplyr::select(orgname, score) %>% rename(IGscore = score)
IGCF_scatter.CF <- filter(CF_match2, num > 1, !is.na(num), score_type == "CFScore.irt") %>% dplyr::select(orgname, score) %>% rename(CFscore.irt = score)

IGCF_scatter <- full_join(IGCF_scatter.IG, IGCF_scatter.CF )

CFplot <- ggplot(filter(IGCF_scatter, CFscore.irt > -4), aes(x=CFscore.irt, y=IGscore)) +theme_classic() + geom_point() +geom_smooth(method="lm")  +xlab("CFScore") +ylab("IGScore")
ggsave("CFplot.pdf", CFplot, device="pdf")  


#####Spearman Rank Correlation for Page 32, Main Text #####
cor(IGCF_scatter$IGscore, IGCF_scatter$CFscore.irt, method = "spearman")


#####Spearman Rank Correlation for Page 32, Main Text #####
#read in all of the bill positions, cbp, votes etc.
pos_all <- read_csv("Data/all_positions_including114-csv.csv")


#read in roll call votes
rollcalls <- read_csv("Data/HSall_rollcalls.csv")
votes <- read_csv("Data/HSall_votes.csv")

#read in the congressional bill project to get topic codes
cbp <- read.csv("Data/bills93-114 2.csv")
cbp_codes <- read.csv("Data/CBP_MajorCodes.csv")
cbp_codes$Major <- as.character(cbp_codes$Major)
cbp <- filter(cbp, Cong >108)
cbp_select <- cbp %>% dplyr::select(BillID, BillNum, BillType, Chamber, Cong, Major)

dim(pos_all)

unique(cbp$BillType)
unique(pos_all$prefix)

#standardize bill types in CBP to match Maplight ids
pos_all$prefix[pos_all$prefix == "S"] <- "s"
pos_all$prefix[pos_all$prefix == "H"] <- "hr"
pos_all$prefix[pos_all$prefix == "HR"] <- "hres"
pos_all$prefix[pos_all$prefix == "HC"] <- "hconres"
pos_all$prefix[pos_all$prefix == "SR"] <- "sres"
pos_all$prefix[pos_all$prefix == "SJ"] <- "sjres"
pos_all$prefix[pos_all$prefix == "HJ"] <- "hjres"
pos_all$prefix[pos_all$prefix == "SC"] <- "sconres"

cbp_select <- cbp %>% dplyr::select(BillID, BillNum, BillType, Chamber, Cong, IntrDate, Major)

pos_all <- pos_all %>% left_join(cbp_select, c("number" = "BillNum", "prefix" = "BillType", "session" = "Cong"))



pos_all_edges <- pos_all %>% dplyr::select(orgname, BillID, disposition)
pos_all_edges <- pos_all_edges %>% filter(complete.cases(pos_all_edges)) 


rollcalls <- filter(rollcalls, congress > 108)

#& (grepl("passage", vote_desc) | grepl("Passage", vote_question)))
votes <- filter(votes, congress > 108)

votes$disposition <- NA
votes$disposition[votes$cast_code == 1] <- "support"
votes$disposition[votes$cast_code == 6] <- "oppose"

votes <- votes %>% filter(!is.na(disposition))

rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID, date)

last_rolls <- rollcalls %>% group_by(BillID) %>% summarise(lastrolldate = max(date))

rollcalls <- rollcalls %>% left_join(last_rolls)
rollcalls <- rollcalls %>% filter(date == lastrolldate)

rollcalls <- rollcalls %>% filter(BillID %in% unique(pos_all_edges$BillID))

final_rollnum <- rollcalls %>% group_by(BillID) %>% summarise(last_rollnum = max(rollnumber))
rollcalls <- rollcalls %>% left_join(final_rollnum)
rollcalls <- rollcalls %>% filter(rollnumber == last_rollnum)
rollcalls <- rollcalls %>% left_join(votes)





rollcalls_pos <- rollcalls %>% dplyr::select(BillID, icpsr, disposition) %>% rename(orgname = icpsr)
rollcalls_pos$orgname <- as.character(rollcalls_pos$orgname)

combined_pos <- bind_rows(pos_all_edges, rollcalls_pos)

unanimous_votes <- combined_pos %>% group_by(BillID) %>% summarise(num_votes = n(), num_support = sum(disposition == "support"), num_oppose = sum(disposition == "oppose"))
unanimous_votes <- unanimous_votes %>% filter(num_votes == num_support | num_votes == num_oppose)
combined_pos <- combined_pos %>% filter(!(BillID %in% unanimous_votes$BillID))  %>% unique()

g_mat <- matrix(0,nrow = length(unique(combined_pos$orgname)), ncol = length(unique(combined_pos$BillID)), dimnames = list(unique(combined_pos$orgname), unique(combined_pos$BillID)))


for (i in 1:nrow(combined_pos)){
  row_org <- combined_pos$orgname[i]
  col_bill <- combined_pos$BillID[i]
  g_mat[row_org , col_bill] <- 1
}

library(igraph)
est_g <- graph_from_incidence_matrix(g_mat)

V(est_g)$core <- coreness(est_g)
core5 <- induced_subgraph(est_g,V(est_g)$core>4)
#extract ids of the bills and orgs to keep
core_names <- V(core5)$name


pos_all_edges_core <- as_tibble(combined_pos) %>% filter(BillID %in% core_names) %>% filter(orgname %in% core_names)

pos_all_core_dat <- dplyr::select(pos_all_edges_core, BillID, orgname, disposition)

rollcalls <- filter(rollcalls, congress > 108)
votes <- filter(votes, congress > 108)

#create bill id that matches cbp and maplight
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID)

rollcalls <- rollcalls %>% left_join(dplyr::select(cbp_select, BillID, Major))

rollcall_cat <- unique(dplyr::select(rollcalls, BillID, Major))



cbp_select <- unique(cbp_select)
cbp_select_maj <- dplyr::select(cbp_select, BillID, Major) %>% filter(!is.na(Major) & Major != 99 )
cbp_select_maj$Major <- as.character(cbp_select_maj$Major)
cbp_select_maj <- cbp_select_maj %>% left_join(cbp_codes)
cbp_select_maj <- cbp_select_maj %>% dplyr::select(-Major)
cbp_select_maj <- cbp_select_maj %>% filter(!is.na(MajorText))
cbp_select_maj$roll_call <- 0
cbp_select_maj$IG_pos <- 0
cbp_select_maj$fivecore <- 0
cbp_select_maj$roll_call[cbp_select_maj$BillID %in% unique(rollcalls$BillID)] <- 1
cbp_select_maj$IG_pos[cbp_select_maj$BillID %in% unique(pos_all$BillID)] <- 1
cbp_select_maj$fivecore[cbp_select_maj$BillID %in% unique(pos_all_core_dat$BillID)] <- 1



bill_table <- prop.table(table(cbp_select_maj$MajorText))*100
roll_table <- prop.table(table(cbp_select_maj$MajorText[cbp_select_maj$roll_call ==1]))*100
IG_table <- prop.table(table(cbp_select_maj$MajorText[cbp_select_maj$IG_pos ==1]))*100
use_table <- prop.table(table(cbp_select_maj$MajorText[cbp_select_maj$fivecore ==1]))*100

bill_table <- cbind(bill_table,roll_table, IG_table, use_table)

bill_table <- round(bill_table , digits = 2)
xtable(bill_table)


##### PLOTS FOR SUPPLEMENTAL INFORMATION B #########
other_dens <- ggplot(IGscores_Other, aes(x=estimate, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density") + coord_cartesian(xlim = c(-3.8317, 2.5902)) 
ggsave("other_dens.pdf", other_dens, device="pdf")

labor_dens <- ggplot(IGscores_Labor, aes(x=estimate, y =..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density")  + coord_cartesian(xlim = c(-3.8317, 2.5902)) 

ggsave("labor_dens.pdf", labor_dens, device="pdf")

health_dens <- ggplot(IGscores_Health, aes(x=estimate, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density")  + coord_cartesian(xlim = c(-3.8317, 2.5902)) 
ggsave("health_dens.pdf", health_dens, device="pdf")


##### SUPPLEMENTAL INFORMATION C: IG-ONLY SCORES

## CORE-5 SCORE ANALYSIS ##

igonly_core5 <- read.csv("IG_only_scores226.csv")
allscores_core5 <- read.csv("static_scores_withints321.csv")

analysis_core5 <- merge(igonly_core5, allscores_core5, by.x = "orgname", by.y = "orgname_new", all.x = T)

# Raw correlation:
cor(analysis_core5$scores, analysis_core5$mean_score_new, method = "spearman")

# Sign switchers
analysis_core5$switch <- ifelse((analysis_core5$scores < 0 & analysis_core5$mean_score_new > 0) |(analysis_core5$scores > 0 & analysis_core5$mean_score_new < 0), 1,0)
sum(analysis_core5$switch) 

## DISTRIBUTIONS ##

#Figure 4:
p <- ggplot(data = analysis_core5, aes(scale(scores))) # scores is IGonly/blue
p + geom_density(fill = "blue", alpha = .5) + geom_density(aes(scale(mean_score_new)), fill = "yellow", alpha = .5) +
  theme_classic(base_family = "AvantGarde") +
  xlab("Score Location") +
  theme(plot.title = element_text(face="bold", size = 20),legend.title = element_blank()) 

## COMPARISONS TO THE 10-CORE SCORES

igonly <- read.csv("IGonly_core10_9162019.csv")
allscores <- read.csv("IGscores_core10_9162019.csv")

analysis <- merge(allscores, igonly, by = "orgname")

# Raw correlation:
cor(analysis$scores.x, analysis$scores.y, method = "spearman") # compared to 0.870 for core 5

# Sign switchers:
analysis$switch <- ifelse((analysis$scores.x < 0 & analysis$scores.y > 0) |(analysis$scores.x > 0 & analysis$scores.y < 0), 1,0)
sum(analysis$switch) # Just 24

# Average discrepancy:
analysis$discrep <- abs(analysis$scores.x - analysis$scores.y)
mean(analysis$discrep) # 0.28
median(analysis$discrep) # 0.20


###### SUPPLEMENTAL INFORMATION E, Comparison to Nominate  #######

nominate <- read_csv("Data/HS_IGScoresNOMINATE4.24.csv" )

nominate_long <- filter(nominate) %>% dplyr::select(icpsr, party_code, IGScore, dim1_std, congress) %>% filter(congress <=114)
nominate_long <- nominate_long %>% gather("score_type","score", -c(icpsr, party_code, congress))
#most conservative democrat
max(filter(nominate_long, party_code == 100, score_type == "IGScore")$score, na.rm = T)

#most liberal republican
min(filter(nominate_long, party_code == 200, score_type == "IGScore")$score, na.rm = T)

####### SUPPLEMENTARY INFORMATION FIGURE 8 ######
party_congresses <- ggplot(filter(nominate_long, party_code == 100 | party_code == 200), aes(fill=as.factor(party_code), color=as.factor(party_code), x=score)) + geom_freqpoly() + facet_grid(congress~score_type) + theme_classic() + scale_color_manual(values = c("#05426C", "#712422","#708259")) + theme(legend.position="none")

ggsave( "party_congresses.pdf",party_congresses,device = "pdf")


####### SUPPLEMENTARY INFORMATION FIGURE 7 ######
cdf1 <- ecdf(nominate$dim1_std) 
cdf2 <- ecdf(nominate$IGScore) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(nominate$dim1_std, nominate$IGScore, na.rm = T), max(nominate$dim1_std, nominate$IGScore, na.rm = T), length.out=length(nominate$dim1_std)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ggplot(filter(nominate_long, party_code == 100 | party_code == 200), aes(x = score, group = score_type, color = score_type))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 28) +
  theme(legend.position ="top") +
  xlab("Sample") +
  ylab("ECDF") + 
  facet_wrap(~congress) + 
  theme_classic() +
  scale_colour_manual(name="", values = c("#137B80", "#8E6C8A"),labels=c("NOMINATE", "IGScore"))


####### TESTS FOR TABLE 4 #####
ks14 <- ks.test(filter(nominate, congress == 114)$dim1_std, filter(nominate, congress == 114)$IGScore)
ks13 <- ks.test(filter(nominate, congress == 113)$dim1_std, filter(nominate, congress == 113)$IGScore)
ks12 <- ks.test(filter(nominate, congress == 112)$dim1_std, filter(nominate, congress == 112)$IGScore)
ks11 <- ks.test(filter(nominate, congress == 111)$dim1_std, filter(nominate, congress == 111)$IGScore)
ks10 <- ks.test(filter(nominate, congress == 110)$dim1_std, filter(nominate, congress == 110)$IGScore)
ks09 <- ks.test(filter(nominate, congress == 109)$dim1_std, filter(nominate, congress == 109)$IGScore)

Ds <- c(ks14$statistic, ks13$statistic,ks12$statistic,ks11$statistic,ks10$statistic,ks09$statistic)
Ps <- c(ks14$p.value, ks13$p.value,ks12$p.value,ks11$p.value,ks10$p.value,ks09$p.value)

right_dem114 <- max(filter(nominate, congress == 114, party_code == 100)$IGScore)
left_rep114 <- min(filter(nominate, congress == 114, party_code == 200)$IGScore)

right_dem113 <- max(filter(nominate, congress == 113, party_code == 100)$IGScore)
left_rep113 <- min(filter(nominate, congress == 113, party_code == 200)$IGScore)

right_dem112 <- max(filter(nominate, congress == 112, party_code == 100)$IGScore)
left_rep112 <- min(filter(nominate, congress == 112, party_code == 200)$IGScore)

right_dem111 <- max(filter(nominate, congress == 111, party_code == 100)$IGScore, na.rm=T)
left_rep111 <- min(filter(nominate, congress == 111, party_code == 200)$IGScore)

right_dem110 <- max(filter(nominate, congress == 110, party_code == 100)$IGScore, na.rm=T)
left_rep110 <- min(filter(nominate, congress == 110, party_code == 200)$IGScore)

right_dem109 <- max(filter(nominate, congress == 109, party_code == 100)$IGScore, na.rm=T)
left_rep109 <- min(filter(nominate, congress == 109, party_code == 200)$IGScore, na.rm=T)

right_dem114_NOM <- max(filter(nominate, congress == 114, party_code == 100)$dim1)
left_rep114_NOM <- min(filter(nominate, congress == 114, party_code == 200)$dim1)

right_dem113_NOM <- max(filter(nominate, congress == 113, party_code == 100)$dim1)
left_rep113_NOM <- min(filter(nominate, congress == 113, party_code == 200)$dim1)

right_dem112_NOM <- max(filter(nominate, congress == 112, party_code == 100)$dim1)
left_rep112_NOM <- min(filter(nominate, congress == 112, party_code == 200)$dim1)

right_dem111_NOM <- max(filter(nominate, congress == 111, party_code == 100)$dim1, na.rm=T)
left_rep111_NOM <- min(filter(nominate, congress == 111, party_code == 200)$dim1)

right_dem110_NOM <- max(filter(nominate, congress == 110, party_code == 100)$dim1, na.rm=T)
left_rep110_NOM <- min(filter(nominate, congress == 110, party_code == 200)$dim1)

right_dem109_NOM <- max(filter(nominate, congress == 109, party_code == 100)$dim1, na.rm=T)
left_rep109_NOM <- min(filter(nominate, congress == 109, party_code == 200)$dim1, na.rm=T)

MLdiff114 <- left_rep114 - right_dem114 
MLdiff113 <- left_rep113 - right_dem113 
MLdiff112 <- left_rep112 - right_dem112 
MLdiff111 <- left_rep111 - right_dem111 
MLdiff110 <- left_rep110 - right_dem110 
MLdiff109 <- left_rep109 - right_dem109

NOMdiff114 <- left_rep114_NOM - right_dem114_NOM 
NOMdiff113 <- left_rep113_NOM - right_dem113_NOM 
NOMdiff112 <- left_rep112_NOM - right_dem112_NOM
NOMdiff111 <- left_rep111_NOM - right_dem111_NOM
NOMdiff110 <- left_rep110_NOM - right_dem110_NOM 
NOMdiff109 <- left_rep109_NOM - right_dem109_NOM

Congresses <- c(114,113,112,111,110,109)
NOMdiffs <- c(NOMdiff114, NOMdiff113, NOMdiff112, NOMdiff111, NOMdiff110, NOMdiff109)
MLdiffs <- c(MLdiff114, MLdiff113, MLdiff112, MLdiff111, MLdiff110, MLdiff109)

######### SUPPLEMENTARY INFORMATION TABLE 4  ##### 
cong_overlap_table <- cbind(Congresses,MLdiffs, NOMdiffs, Ds, Ps)
print(xtable(cong_overlap_table, digits = 3), include.rownames = F)


######## SUPPLEMENTARY INFORMATION FIGURE 6 #########
cong_14scat <- ggplot(filter(nominate, congress == 114), aes(x=dim1, y=IGScore, color = as.factor(party_code))) +geom_point() +geom_smooth(method="lm")+ theme_classic() +facet_wrap(~congress)  +ggtitle("IGScores vs. DW-Nominate 1st dimension for 114th Congress") +xlab("DW-Nominate dim1") + scale_color_manual(values = c("#05426C", "#712422","#708259")) +facet_wrap(~chamber)



ggsave("cong_14scat.pdf", cong_14scat, device="pdf")


####### TESTS FOR SUPPLEMENTARY INFORMATION TABLE 3 ########
cong114 <- cor.test(filter(nominate, congress == 114)$dim1, filter(nominate, congress == 114)$IGScore, method = "spearman")$estimate
cong114dem <-cor.test(filter(nominate, congress == 114)$dim1[filter(nominate, congress == 114)$party_code==100], filter(nominate, congress == 114)$IGScore[filter(nominate, congress == 114)$party_code==100],  method = "spearman")$estimate
cong114rep <-cor.test(filter(nominate, congress == 114)$dim1[filter(nominate, congress == 114)$party_code==200], filter(nominate, congress == 114)$IGScore[filter(nominate, congress == 114)$party_code==200], method = "spearman")$estimate

cong113 <- cor.test(filter(nominate, congress == 113)$dim1, filter(nominate, congress == 113)$IGScore, method = "spearman")$estimate
cong113dem <-cor.test(filter(nominate, congress == 113)$dim1[filter(nominate, congress == 113)$party_code==100], filter(nominate, congress == 113)$IGScore[filter(nominate, congress == 113)$party_code==100],  method = "spearman")$estimate
cong113rep <-cor.test(filter(nominate, congress == 113)$dim1[filter(nominate, congress == 113)$party_code==200], filter(nominate, congress == 113)$IGScore[filter(nominate, congress == 113)$party_code==200], method = "spearman")$estimate

cong112 <- cor.test(filter(nominate, congress == 112)$dim1, filter(nominate, congress == 112)$IGScore, method = "spearman")$estimate
cong112dem <-cor.test(filter(nominate, congress == 112)$dim1[filter(nominate, congress == 112)$party_code==100], filter(nominate, congress == 112)$IGScore[filter(nominate, congress == 112)$party_code==100],  method = "spearman")$estimate
cong112rep <-cor.test(filter(nominate, congress == 112)$dim1[filter(nominate, congress == 112)$party_code==200], filter(nominate, congress == 112)$IGScore[filter(nominate, congress == 112)$party_code==200], method = "spearman")$estimate

cong111 <- cor.test(filter(nominate, congress == 111)$dim1, filter(nominate, congress == 111)$IGScore, method = "spearman")$estimate
cong111dem <-cor.test(filter(nominate, congress == 111)$dim1[filter(nominate, congress == 111)$party_code==100], filter(nominate, congress == 111)$IGScore[filter(nominate, congress == 111)$party_code==100],  method = "spearman")$estimate
cong111rep <-cor.test(filter(nominate, congress == 111)$dim1[filter(nominate, congress == 111)$party_code==200], filter(nominate, congress == 111)$IGScore[filter(nominate, congress == 111)$party_code==200], method = "spearman")$estimate

cong110 <- cor.test(filter(nominate, congress == 110)$dim1, filter(nominate, congress == 110)$IGScore, method = "spearman")$estimate
cong110dem <-cor.test(filter(nominate, congress == 110)$dim1[filter(nominate, congress == 110)$party_code==100], filter(nominate, congress == 110)$IGScore[filter(nominate, congress == 110)$party_code==100],  method = "spearman")$estimate
cong110rep <-cor.test(filter(nominate, congress == 110)$dim1[filter(nominate, congress == 110)$party_code==200], filter(nominate, congress == 110)$IGScore[filter(nominate, congress == 110)$party_code==200], method = "spearman")$estimate

cong109 <- cor.test(filter(nominate, congress == 109)$dim1, filter(nominate, congress == 109)$IGScore, method = "spearman")$estimate
cong109dem <-cor.test(filter(nominate, congress == 109)$dim1[filter(nominate, congress == 109)$party_code==100], filter(nominate, congress == 109)$IGScore[filter(nominate, congress == 109)$party_code==100],  method = "spearman")$estimate
cong109rep <-cor.test(filter(nominate, congress == 109)$dim1[filter(nominate, congress == 109)$party_code==200], filter(nominate, congress == 109)$IGScore[filter(nominate, congress == 109)$party_code==200], method = "spearman")$estimate


####SUPPLEMENTARY INFORMATION TABLE 3########
cong_num <- c(114,113,112,111,110,109)
congs <- c(cong114, cong113, cong112, cong111, cong110, cong109)
cong_dem <- c(cong114dem, cong113dem, cong112dem, cong111dem, cong110dem, cong109dem)
congs_rep <- c(cong114rep, cong113rep, cong112rep, cong111rep, cong110rep, cong109rep)
cong_corrs <- cbind(cong_num,congs, cong_dem, congs_rep)

print(xtable(cong_corrs), include.rownames = F)
#party_medians <- group_by(nominate, congress, party_code, chamber) %>% summarise(med_IGScore = median(IGScore, na.rm=T), ML_25th = quantile(IGScore, .25, na.rm=T), ML_75th = quantile(IGScore, .75, na.rm=T)) 


#### SUPPLEMENTARY INFORMATION G: CORRECT CLASSIFICATION ANALSYSES #####

## READ IN PREDICTIONS (GENERATED USING PREDICT() FUNCTION IN PSCL)

perc_correct <- read.csv("correct_classifications_allchains_rep.csv")

## SUMMARY STATISTICS

mean(perc_correct$mean_perc_correct)
median(perc_correct$mean_perc_correct)

# Figure 12:
plot(perc_correct$scores, perc_correct$mean_perc_correct)


## MERGE TO LEGISLATOR/GROUP IDENTIFIERS 

scores <- read.csv("scores_withlegisindicator.csv")
perc_correct$org_index <- 1:length(unique(final_positions_edgelist$org_index))
all_perc_legis <- merge(scores, perc_correct, by = "org_index")


# Interest groups significantly better classified than legislators
t.test(subset(all_perc_legis, legislator == 1)$mean_perc_correct, subset(all_perc_legis, legislator == 0)$mean_perc_correct)

# Figure 13:
all_perc_legis$legislator_group <- ifelse(all_perc_legis$legislator == 1, "Legislator", "Interest Group")
p <- ggplot(data = all_perc_legis, aes(factor(legislator_group), mean_perc_correct))
p + geom_boxplot() + scale_y_continuous(limits = c(45, 100))+
  theme_few(base_family = "Gill Sans MT") +
  # Add labels for X and Y axes
  xlab("")+
  ylab("Average Correct Classification Rate (%)") +
  # Add the title
  theme(plot.title = element_text(face="bold", size = 18), legend.title = element_blank()) 


## CORRECT CLASSIFICATIONS BY GROUP TYPE

types <- read.csv("Groups_WRS_Crosswalk.csv")
all_perc_types <- merge(all_perc_legis, types, by = "orgname")

# Business versus non-business:
t.test(subset(all_perc_types, WRS_ThreeCat == "Corporations, Business, Trade and Occupational Associations")$mean_perc_correct, subset(all_perc_types, WRS_ThreeCat != "Corporations, Business, Trade and Occupational Associations")$mean_perc_correct)
# no significant difference

# Table 5:
require("doBy")
require(stargazer)
fortable <- summaryBy(mean_perc_correct ~ WRS_full, data = all_perc_types, FUN = mean)
View(fortable) # (entered into LaTeX manually from here)


