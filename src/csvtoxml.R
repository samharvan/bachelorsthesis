library(XML)
library(tidyverse)
library(data.table)
library(htmltidy)
## VOTES CSV to XML
setwd("/home/samuel/bachelorsthesis")
total_df = read.csv("7volebneobdobie.csv")
total_df_rollcalls = read.csv("7volobdobie_rollcalls.csv")
rollcall_id = unique(total_df$vote_id_total)
setwd("/home/samuel/bachelorsthesis/votes")

for (i in rollcall_id) {
  each_rollcall = filter(total_df, vote_id_total == i)
  each_rollcall_info = filter(total_df_rollcalls, vote_id_total == i)
  rollcall_mp_id = each_rollcall$mp_id
  # doc+root element
  doc = newXMLDoc()
  root = newXMLNode("voteinfo", doc = doc)
  
  # FIRST-ORDER ELEMENTS
  votetitle = newXMLNode("votetitle", "", parent = root)
  fulltitle = newXMLNode("fulltitle", each_rollcall_info$rollcall_title, parent = root)
  policyarea = newXMLNode("policyarea", "", parent = root)
  docref = newXMLNode("docref", "", parent = root)
  dossiercommittee = newXMLNode("dossiercommittee", "", parent = root)
  epref = newXMLNode("epref", "", parent = root)
  legalbasis = newXMLNode("legalbasis", "", parent = root)
  reporterinfo = newXMLNode("reporterinfo", "", parent = root)
  date = newXMLNode("date", each_rollcall_info$date, parent = root)
  rollcall_id = newXMLNode("rollcall_id", i, parent = root)
  votes = newXMLNode("votes", "", parent = root)
  
  for (j in rollcall_mp_id) {
    each_rollcall_vote = filter(each_rollcall, mp_id == j)
    if (each_rollcall_vote$vote == "Was not present") {
      next
    } else {
      # SECOND-ORDER ELEMENT
      vte = newXMLNode("vote", "", parent = votes)
      
      # THIRD-ORDER ELEMENT
      newXMLNode("mepid", j, parent = vte)
      newXMLNode("mepinfo", "", parent = vte)
      newXMLNode("mepvote", each_rollcall_vote$vote, parent = vte)
    }
  }
  
  # OUTPUT XML CONTENT TO FILE
  saveXML(doc, file=paste0(i, ".xml"), encoding = "UTF-8")
}


## MEPS CSV to XML
setwd("/home/samuel/bachelorsthesis")
total_df = read.csv("7volobdobie_attributes.csv")
mps = unique(total_df$mp_id)
setwd("/home/samuel/bachelorsthesis/meps")

for (i in mps) {
  each_mp = filter(total_df, mp_id == i)
  # doc+rootelement
  doc = newXMLDoc()
  root = newXMLNode("mepinfo", doc = doc)
  
  # FIRST-ORDER ELEMENTS
  mepid = newXMLNode("mepid", i, parent = root)
  mepname = newXMLNode("mepname",each_mp$last_name, parent = root)
  fullname = newXMLNode("fullname", paste(each_mp$first_name,each_mp$last_name), parent = root)
  country = newXMLNode("country", each_mp$region_resid, parent = root)
  countryinfo = newXMLNode("countryinfo", "", parent = root)
  title = newXMLNode("title", "", parent = root)
  party = newXMLNode("party", "", parent = root)
  mepprofile = newXMLNode("mepprofile", "", parent = root)
  webpage = newXMLNode("webpage", "", parent = root)
  address = newXMLNode("address", "", parent = root)
  mail = newXMLNode("mail", "", parent = root)
  birth = newXMLNode("birth", each_mp$birth, parent = root)
  birthplace = newXMLNode("birthplace", "", parent = root)
  europarlid = newXMLNode("europarlid", "", parent = root)
  membersince = newXMLNode("membersince", "", parent = root)
  membertill = newXMLNode("membertill", "", parent = root)
  group = newXMLNode("group", each_mp$cand_for, parent = root)
  attendance = newXMLNode("attendance", "", parent = root)
  college_title = newXMLNode("college_title", each_mp$title, parent = root)
  residence = newXMLNode("residence", each_mp$place_resid, parent = root)
  ethnicity = newXMLNode("ethnicity", each_mp$natio, parent = root)
  
  # OUTPUT XML CONTENT TO FILE
  saveXML(doc, file=paste0(i, ".xml"), encoding = "UTF-8")
}
