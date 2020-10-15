# write.csv (source, "Allen_test_source.csv", row.names = F)
# write.csv (target, "Allen_test_target.csv", row.names = F)
# write.csv (weights, "Allen_test_weights.csv", row.names = F)

source_glob <- read.csv ("Allen_test_source.csv")
source_1 <- source_glob[,1:7]
source_2 <- source_glob[,8:14]
source_1[,3] <- (source_1[,3])/1000 + 1
source_2[,3] <- (source_2[,3])/1000 + 1
source_glob <- list (source_1, source_2)
target_glob <- read.csv ("Allen_test_target.csv")
dfa_glob <- read.csv ("Allen_test_weights.csv")

source <- source_glob
target <- target_glob
weights <- dfa_glob
weights <- weights[, colSums(weights != 0) > 0]	
for (i in seq (1, length(source))){		
  source[[i]] <- source[[i]][,c(1,2,which (names (source[[i]]) %in% colnames(weights)))]		
}		
target <- target[,c(1,2,which (names (target) %in% colnames(weights)))]

#lets focus on target #2
uniqueSources <- unique (source_2[,2])
datas <- getSubsetmean(source_2[, -1])
