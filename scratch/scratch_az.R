data = readRDS("data/MAStatePatrol.rds")
for (race in unique(data$subject_race)) {
  rr = data$subject_race == race & data$search_conducted
  rr[is.na(rr)] <- FALSE
  cf = data$contraband_found[rr]
  cf[is.na(cf)] <- FALSE
  print(race)
  print(sum(cf)/sum(rr))
}

# for (loc in unique(data$location)) {
#   rr = data$location == loc & data$search_conducted
#   rr[is.na(rr)] <- FALSE
#   cf = data$contraband_found[rr]
#   cf[is.na(cf)] <- FALSE
#   print(loc)
#   print(sum(cf)/sum(rr))
# }
