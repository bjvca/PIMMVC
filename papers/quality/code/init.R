path <- getwd()
path_2 <- strsplit(path, "/papers/quality")[[1]]
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

farmers$tot_plot_size <- rowSums(cbind(as.numeric(as.character(farmers$hh.maize.plot.1..q42)),as.numeric(as.character(farmers$hh.maize.plot.2..q42)) ,
as.numeric(as.character(farmers$hh.maize.plot.3..q42)),  
as.numeric(as.character(farmers$hh.maize.plot.4..q42)), 
as.numeric(as.character(farmers$hh.maize.plot.5..q42)),  
as.numeric(as.character(farmers$hh.maize.plot.6..q42))  ), na.rm=T )
farmers$hh.maize.q95[farmers$hh.maize.q95 > 998] <- NA
farmers$tot_prod <- farmers$hh.maize.q95*farmers$hh.maize.q96
farmers$maize_yield <- farmers$tot_prod/farmers$tot_plot_size

farmers$maize_yield_cap <- farmers$maize_yield/farmers$hh.maize.q28

farmers$tot_sold  <- rowSums(cbind(as.numeric(as.character(farmers$hh.maize.transaction.1..q101d)),
as.numeric(as.character(farmers$hh.maize.transaction.2..q101d)),
as.numeric(as.character(farmers$hh.maize.transaction.3..q101d)),
as.numeric(as.character(farmers$hh.maize.transaction.4..q101d)),
as.numeric(as.character(farmers$hh.maize.transaction.5..q101d)),
as.numeric(as.character(farmers$hh.maize.transaction.6..q101d)),
as.numeric(as.character(farmers$hh.maize.transaction.7..q101d))), na.rm=T )*farmers$hh.maize.q96

farmers$tot_sold[farmers$tot_sold > farmers$tot_prod] <- NA

farmers$market_part_rate <- farmers$tot_sold / farmers$tot_prod 

traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

traders$hh.maize.value.q15a[traders$hh.maize.value.q15a + traders$hh.maize.value.q15b + traders$hh.maize.value.q15c + traders$hh.maize.value.q15d + traders$hh.maize.value.q15e >100] <- NA

