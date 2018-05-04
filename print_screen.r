print_screen <- function(l){
	if(length(l) > 0){
		cat(unlist(head(l, n = 1)),sep="")
		cat("\n")
		print_screen(tail(l, n = length(l)-1))
	}
}



#cat(strrep("|                                  |\n", 20))


screen = list(1:20,1:20,1:20,1:20,1:20)
print_screen(screen)
