# constroi a tela como matriz de espacos " "
build_screen <- function(height = 30, width = 40, player){
        screen <- matrix(rep(" ", height*width), ncol=width, nrow=height)
}
 
# imprime matrix m no terminal
print_screen <- function(m){
        if(length(m) > 0){
                cat("|",unlist(head(m, n = 1)),"|",sep="")
                cat("\n")
                print_screen(tail(m, n = (length(m)/ncol(m))-1))
        }
}
 
# loop principal
game_loop <- function(){
        # para limpar o terminal conforme o sistema
        if (.Platform$OS.type != 'windows')
                clear <- function(){ cat("\033c") }
        else
                clear <- function(){ cat("\014") }
 
        # constroi a nave do jogar em ASCII
        #screen = list(list("  ",intToUtf8(9595),"  "),
        #              list(intToUtf8(rep(9606,2)),intToUtf8(9608),intToUtf8(rep(9606,2))))
         
        # loop: limpa terminal, gera nova tela, imprime      
        while(TRUE){
                clear()
                screen <- build_screen()
                print_screen(screen)
                Sys.sleep(0.5)
        }
}

game_loop()
