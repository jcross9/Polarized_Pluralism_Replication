##############################################################################
######## DATA CLEANING, FORMATTING FOR IGSCORE ESTIMATION ####################
####################### POLARIZED PLURALISM ##################################
##################### CROSSON, FURNAS & LORENZ ###############################
##############################################################################

rm(list=ls())
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

setwd("/Data")

## READ IN THE MAPLIGHT DATA

pos_all <- read_csv("all_positions_including114-csv.csv")

## READ IN THE ROLL CALL DATA, VOTES

rollcalls <- read_csv("HSall_rollcalls.csv")
votes <- read_csv("HSall_votes.csv")

## READ IN BILL-SPECIFIC INFORMATION FROM CONGRESSIONAL BILLS PROJECT

cbp <- read_csv("bills93-114 2.csv")

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

#join to Maplight data
pos_all <- pos_all %>% left_join(cbp_select, c("number" = "BillNum", "prefix" = "BillType", "session" = "Cong"))
pos_all_edges <- pos_all %>% dplyr::select(orgname, BillID, disposition)
pos_all_edges <- pos_all_edges %>% filter(complete.cases(pos_all_edges)) 

## JOIN DATA TOGETHER

rollcalls <- filter(rollcalls, congress > 108)
votes <- filter(votes, congress > 108)

votes$disposition <- NA
votes$disposition[votes$cast_code == 1] <- "support"
votes$disposition[votes$cast_code == 6] <- "oppose"

votes <- votes %>% filter(!is.na(disposition))

rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID, date)

#merge by last position taken, last roll call taken
last_rolls <- rollcalls %>% group_by(BillID) %>% summarise(lastrolldate = max(date))

rollcalls <- rollcalls %>% left_join(last_rolls)
rollcalls <- rollcalls %>% filter(date == lastrolldate)
rollcalls <- rollcalls %>% filter(BillID %in% unique(pos_all_edges$BillID))
final_rollnum <- rollcalls %>% group_by(BillID) %>% summarise(last_rollnum = max(rollnumber))
rollcalls <- rollcalls %>% left_join(final_rollnum)
rollcalls <- rollcalls %>% filter(rollnumber == last_rollnum)

rollcalls <- rollcalls %>% left_join(votes)

#add to MapLight data
rollcalls_pos <- rollcalls %>% dplyr::select(BillID, icpsr, disposition) %>% rename(orgname = icpsr)
rollcalls_pos$orgname <- as.character(rollcalls_pos$orgname)
combined_pos <- bind_rows(pos_all_edges, rollcalls_pos)

## PULL OUT UNANIMOUS VOTES, AHEAD OF K-CORE FILTRATION

unanimous_votes <- combined_pos %>% group_by(BillID) %>% summarise(num_votes = n(), num_support = sum(disposition == "support"), num_oppose = sum(disposition == "oppose"))
unanimous_votes <- unanimous_votes %>% filter(num_votes == num_support | num_votes == num_oppose)
combined_pos <- combined_pos %>% filter(!(BillID %in% unanimous_votes$BillID))  %>% unique()

## K-CORE FILTRATION; ENSURES ADEQUATE "GLUE" FOR ESTIMATION

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
core_names <- data.frame(cbind(core_names, V(core5)$core))
names(core_names)[2] <- "core"

pos_all_edges_core <- as_tibble(combined_pos) %>% filter(BillID %in% core_names$core_names) %>% filter(orgname %in% core_names$core_names)

pos_all_edges_core <- merge(pos_all_edges_core, core_names, by.x = "orgname", by.y = "core_names")
pos_all_core_dat <- dplyr::select(pos_all_edges_core, BillID, orgname, disposition, core)
corelevels <- pos_all_core_dat %>% select(orgname, core) %>% distinct()

## IDENTIFY ANCHOR ORGANIZATIONS FOR MODEL IDENTIFICATION

anchor_orgs <- c("10713" ,"14657","Sierra Club","Americans for Tax Reform")
non_anchors <- filter(pos_all_core_dat, !(orgname %in% anchor_orgs))

#assign incides
anchor_pos <- filter(pos_all_core_dat, orgname %in% anchor_orgs)

non_anchors$org_index <- as.numeric(as.factor(non_anchors$orgname)) + 4
non_anchors$bill_index <- as.numeric(as.factor(non_anchors$BillID))

anchor_pos$org_index[anchor_pos$orgname == "10713" ] <- 1
anchor_pos$org_index[anchor_pos$orgname == "Sierra Club"] <- 2
anchor_pos$org_index[anchor_pos$orgname == "14657"] <- 3
anchor_pos$org_index[anchor_pos$orgname == "Americans for Tax Reform"] <- 4

anchor_pos <- anchor_pos %>% left_join(dplyr::select(non_anchors, BillID, bill_index)) %>% unique()

## BUILD FINAL DYAD LIST

final_positions_edgelist <- bind_rows(dplyr::select(anchor_pos, org_index, orgname, bill_index, disposition), dplyr::select(non_anchors, org_index, orgname, bill_index, disposition))
final_positions_edgelist$vote <- NA
final_positions_edgelist$vote[final_positions_edgelist$disposition == "support"] <- 1
final_positions_edgelist$vote[final_positions_edgelist$disposition == "oppose"] <- 0

final_positions_edgelist <- final_positions_edgelist %>% dplyr::select(-disposition)

## SOME DESCRIPTIVE INFORMATION FROM THE PAPER TEXT

#number of bills in k core
length(unique(final_positions_edgelist$bill_index))

#number of 5-core bills with rollcall votes
length(unique(subset(pos_all_core_dat, BillID %in% rollcalls$BillID)$BillID))

#total number of bills receiving roll call
raw_rolls <- read_csv("HSall_rollcalls.csv")
raw_rolls$BillID <- paste(raw_rolls$congress, raw_rolls$chamber, raw_rolls$bill_number)
length(unique(subset(raw_rolls, congress > 108 & congress < 115)$BillID))

#total number of bills in Maplight data; percentage of all bills
length(unique(pos_all$BillID))
length(unique(pos_all$BillID))/75673

#total number of groups in Maplight data
length(unique(pos_all$MLid))
