# Load required libraries
library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(plotly)
library(viridis)

# Read a subset of the train data
setwd('/Users/ppj/Documents/CORP/OP_fashionicon')
uid_eid <- readRDS("./data/uid_eid.rds")

# Extract the unique place ids for subsetting in the app graphs
# placeIds <- unique(dataSubset$place_id)
uids <- table(uid_eid$uid)
uidslist <- names(sort(uids, decreasing = TRUE))

