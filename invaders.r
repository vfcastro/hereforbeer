if (interactive()) {
  if (.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib')
}

# Cria o jogador
MakePlayer <- function(coordx=0, coordy=0, newlife=3, newammo=3){
	return (list(x = coordx, y = coordy, life = newlife, ammo = newammo))
}

# Atualiza o player dependendo do botão pressionado
UpdatePlayer <- function(key, player){
	if(key == "a")
	  return (MakePlayer(player[["x"]]-1, player[["y"]], player[["life"]], player[["ammo"]]))
	if(key == "d")
	  return (MakePlayer(player[["x"]]+1, player[["y"]], player[["life"]], player[["ammo"]]))
	if(key == "w")
	  return (MakePlayer(player[["x"]], player[["y"]]+1, player[["life"]], player[["ammo"]]-1))
	player
}

# Desenha o jogador (em branco)
DrawPlayer <- function(player){
	symbols(player[["x"]], player[["y"]], rectangles = matrix(c(3,1), ncol = 2),
	          inches = FALSE, fg = "white", bg = "white", add = TRUE	)
}

# Apaga o jogador (desenha em preto)
ErasePlayer <- function(player){
	symbols(player[["x"]], player[["y"]], rectangles = matrix(c(3,1), ncol = 2),
	          inches = FALSE, fg = "black", bg = "black", add = TRUE)
}

# Desenha as bordas da tela
DrawBorders <- function(height,width){
	lines(c(0,height,height,0,0), c(0,0,width,width,0), type = "l", lwd = 2, col = "white")
}


game <- function(height=30,width=30){
	status <- NULL


	# Tratamento do teclado
	keypress <- function(key){
		p <- player
		# ESC: termina execucao
		if(key == "\033")
			graphics.off()
		else{
			ErasePlayer(p)
			newplayer <- UpdatePlayer(key,p)
			DrawBorders(height,width)
			DrawPlayer(newplayer)
		}
		return(newplayer)
	}

	# Cria a janela do jogo
	par(mar = rep(1,4), bg = "black")
	plot.new()
	plot.window(xlim = c(0, 30), ylim = c(0, 30))

	# Desenha as bordas
	DrawBorders(height,width)
	
	# Cria o jogador na posicao inicial
	player <- MakePlayer(coordx=15,coordy=0.5)
	
  # Desenha o jogador
	DrawPlayer(player)

	while(TRUE){
		Sys.sleep(.05)
		player <- getGraphicsEvent(prompt = "", onKeybd = keypress)		

	}

}

game()
