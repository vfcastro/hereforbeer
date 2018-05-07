if (interactive()) {
  if (.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib')
}

# Cria o jogador
MakePlayer <- function(x=0, y=0, xsize=3, ysize=1, life=3, ammo=3){
	return (list(x = x, y = y, xsize=xsize, ysize=ysize, life = life, ammo = ammo))
}

# Atualiza o player dependendo do botão pressionado
UpdatePlayer <- function(key, player){
	if(key == "a")
	  return (MakePlayer(x=player[["x"]]-1, y=player[["y"]]))
	if(key == "d")
	  return (MakePlayer(x=player[["x"]]+1, y=player[["y"]]))
	if(key == "w")
	  return (MakePlayer(x=player[["x"]]-1, y=player[["y"]], ammo=player[["ammo"]]-1))
	return(player)
}

# Desenha o jogador (em branco)
DrawPlayer <- function(player){
	symbols(player[["x"]], player[["y"]], rectangles = matrix(c(player[["xsize"]],player[["ysize"]]), ncol = 2),
	          inches = FALSE, fg = "white", bg = "white", add = TRUE	)
	print(player)
}

# Apaga o jogador (desenha em preto)
ErasePlayer <- function(player){
	symbols(player[["x"]], player[["y"]], rectangles = matrix(c(player[["xsize"]],player[["ysize"]]), ncol = 2),
	          inches = FALSE, fg = "black", bg = "black", add = TRUE)
}

# Desenha as bordas da tela
DrawBorders <- function(height,width){
	lines(c(0,height,height,0,0), c(0,0,width,width,0), type = "l", lwd = 2, col = "white")
}

# Testa se o jogador está no limite da tela
TestScreenSpace <- function(player,height,width){
	if(player[["x"]]+player[["xsize"]]/2 > width || player[["x"]]-player[["xsize"]]/2 < 0)
		return(TRUE)
	else return(FALSE)
}


game <- function(height=30,width=30){
	# Tratamento do teclado
	keypress <- function(key){
		p <- player
		# ESC: termina execucao
		if(key == "\033")
			graphics.off()
		# Teclas de movimento em UpdatePlayer
		else{
				newplayer <- UpdatePlayer(key,p)
				if(!TestScreenSpace(newplayer,height,width)){
					ErasePlayer(p)
					DrawBorders(height,width)
					DrawPlayer(newplayer)
					return(newplayer)
				}
		}
		return(p)
	}

	# Cria a janela do jogo
	par(mar = rep(1,4), bg = "black")
	plot.new()
	plot.window(xlim = c(0, width), ylim = c(0, height))

	# Desenha as bordas
	DrawBorders(height,width)
	
	# Cria o jogador na posicao inicial
	player <- MakePlayer(x=15,y=0.5)
	
  # Desenha o jogador
	DrawPlayer(player)

	# Loop principal
	while(TRUE){
		Sys.sleep(.02)
		player <- getGraphicsEvent(prompt = "", onKeybd = keypress)		

	}

}

game()
