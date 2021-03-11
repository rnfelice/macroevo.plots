#uses treeio to combine tree topology and bayestraits posterior into single S4 object for plotting.
add_rjpp_to_tree <- function(rjpp_out){
    rjpp_data <- as_tibble(rjpp_out$data)
    timetree <- rjpp_out$meantree
    timetree$edge.length <- rjpp_data$orgBL[-1]
    timetree$root.time <- max(nodeHeights(timetree))
    rjpp_data_nodes <- rjpp_data %>% rename(., node=descNode) %>% mutate(., iters = rjpp_out$niter) %>% mutate(., ppRate = round(nOrgnNRate/iters,2))
    timetree <- treeio::as.treedata(timetree)
    treedata <- treeio::full_join(timetree, y = rjpp_data_nodes, by = "node")
    return(treedata)
}



rttplotter3<-function(rjpp.data,label){
    
    #pprates<-rjpp.output$data$meanRate[-1]/mean(rjpp.output$data$meanRate[-1])
    pprates<-rjpp.data@extraInfo$meanRate
    
    #scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
    #scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
    #scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
    
    H1<-nodeHeights(rjpp.data@phylo)
    timedepth<-round(max(H1))
    rate.at.time<-vector()
    
    
    for (i in 1:timedepth){
        #slice at time of timebin:
        spot<-i
        edges<-which(H1[,2]>spot&H1[,1]<spot)
        rate.at.time[i]<-mean(pprates[edges])
        
    }
    rate.at.time<-rate.at.time[1:timedepth-1]/mean(rate.at.time[1:timedepth-1])
    
    
    
    df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]))
    colnames(df2)[2]<-label
    return(df2)
}

#
rate_summary <- function(rjpp_out, time_tree, lookup.table, group_column){
    #select the species from  the lookup table that are part of the current dataset
    current.clades <- filter(lookup.table, newnames %in% rjpp_out$meantree$tip.label)
    groupvector<-current.clades[which(colnames(lookup.table)==group_column)]
    groupcats<-unique(pull(groupvector))
    #select the sub tree for each group
    tree_subsets <- lapply(1:length(groupcats), function(x) keep.tip(time_tree, current.clades$newnames[which(groupvector==groupcats[x])]))
    #get the time represented by each sub tree
    total_tree_depths <-  lapply(1:length(tree_subsets), function(x) max(phytools::nodeHeights(tree_subsets[[x]]))) %>% unlist()

    #the first column is the rate on the root and our time tree has no root edge so this is removed with the "[-1,]"
    rate_table <- rjpp_out$scalars$rates[-1,]
    #make copy the mean tree x times where x is the number of trees in the posterior distribution
    treelist<-rep(list(rjpp_out$meantree),dim(rate_table)[2])
    #now convert the edge lengths of each of those trees to be the rate scalar for that edge
    #this allows us to keep rates associated with the appropriate edges as we go to the next step
    for (k in 1:length(treelist)){
        treelist[[k]]$edge.length<-rate_table[,k]
    }
    
    resultsA <- paste0('mean_rate_', groupcats) %>% purrr::map_dfc(setNames, object = list(numeric()))
    results_scaledA <- paste0('mean_scaled_rate_', groupcats) %>% purrr::map_dfc(setNames, object = list(numeric()))
    #now subset those trees based on group identity
    for (i in 1:length(treelist)){
        tree_subsets_i <- lapply(1:length(groupcats), function(x) keep.tip(treelist[[i]], current.clades$newnames[which(groupvector==groupcats[x])]))
        #get the mean of each group
        mean_rates_i <- lapply(1:length(tree_subsets), function(x) mean(tree_subsets_i[[x]]$edge.length)) %>% unlist()
        names(mean_rates_i) <- paste0('mean_rate_', groupcats)
        #scale to age of subtree
        mean_rates_i_scaled <- mean_rates_i/total_tree_depths
        names(mean_rates_i_scaled) <- paste0('mean_scaled_rate_', groupcats) 
        resultsA <- bind_rows(resultsA, mean_rates_i)
        results_scaledA <- bind_rows(results_scaledA, mean_rates_i_scaled)
        
    }
    #comnbine into two tables, one for unscaled one for scaled 
    results <- resultsA %>% pivot_longer(cols = starts_with("mean_rate_"),
                                         names_to = "Group",
                                         names_prefix = "mean_rate_",
                                         values_to = "MeanRate")
    results_scaled <- results_scaledA %>% pivot_longer(cols = starts_with("mean_scaled_rate_"),
                                                       names_to = "Group",
                                                       names_prefix = "mean_scaled_rate_",
                                                       values_to = "MeanRate")
    
    return(list(results, results_scaled))
}
