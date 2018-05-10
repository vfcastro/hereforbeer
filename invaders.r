if (interactive()) {
  if (.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib')
}

# Cria o jogador
MakePlayer <- function(x=15, y=0.5, xsize=3, ysize=1, life=3, ammo=3, shots=list()){
	return (list(x = x, y = y, xsize=xsize, ysize=ysize, life = life, ammo = ammo, shots=shots))
}

# Atualiza o player dependendo do botão pressionado
UpdatePlayer <- function(key, player){
	if(key == "a")
	  return (MakePlayer(x=player[["x"]]-1, y=player[["y"]], shots=player[["shots"]]))
	if(key == "d")
	  return (MakePlayer(x=player[["x"]]+1, y=player[["y"]], shots=player[["shots"]]))
	if(key == "w")
	  return (MakePlayer(x=player[["x"]], y=player[["y"]], 
											 shots=list(append(player[["shots"]],MakeShot(player[["x"]],player[["y"]]+1)))))
	return(player)
}

# Desenha o jogador (em branco)
DrawPlayer <- function(player){
	symbols(player[["x"]], player[["y"]], rectangles = matrix(c(player[["xsize"]],player[["ysize"]]), ncol = 2),
	          inches = FALSE, fg = "white", bg = "white", add = TRUE	)
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


# Cria um tiro (vamos ter 2 lista, uma pros tiros do player e outra pras naves)
MakeShot <- function(x, y, xsize=1, ysize=1){
  return (list(x = x, y = y, xsize=xsize, ysize=ysize))
}

# Desenha o tiro
DrawShot <- function(shot){
	symbols(shot[["x"]], shot[["y"]], rectangles = matrix(c(shot[["xsize"]],shot[["ysize"]]), ncol = 2),
	          inches = FALSE, fg = "white", bg = "white", add = TRUE	)
}

# Desenha o tiro
EraseShot <- function(shot){
	symbols(shot[["x"]], shot[["y"]], rectangles = matrix(c(shot[["xsize"]],shot[["ysize"]]), ncol = 2),
	          inches = FALSE, fg = "black", bg = "black", add = TRUE	)
}

# Atualiza tiro do jogador
UpdateShot <- function(shot){
	return(MakeShot(x=shot[["x"]], y=shot[["y"]]+1))
}


game <- function(height=30,width=30){
	# Cria a janela do jogo
	par(mar = rep(1,4), bg = "black")
	plot.new()
	plot.window(xlim = c(0, width), ylim = c(0, height))
	# Desenha as bordas
	DrawBorders(height,width)
	
	# Cria o jogador
	player <- MakePlayer(x=15,y=0.5)
	playerShots <- list()
		
  # Desenha o jogador
	DrawPlayer(player)

	# Tratamento do teclado
	keypress <- function(key){
		if(key == "\033")
			graphics.off()
		else
			return(UpdatePlayer(key,player))
			
	}

	# Loop principal
	while(TRUE){
		# Atulizar player conforme eventos do teclado
		newplayer <- getGraphicsEvent(prompt = "", onKeybd = keypress)
		# Se a nova posicao e valida, renderiza nova posicao
		if(!TestScreenSpace(newplayer,height,width)){
			ErasePlayer(player)
			player <- newplayer
			DrawBorders(height,width)
			DrawPlayer(player)
		}		

		# Renderiza tiros do jogador
		lapply(player[["shots"]],DrawShot)

		# Apaga tiros do jogador
		lapply(player[["shots"]],EraseShot)

  	# Atuliza tiros do jogador
		newshots <- lapply(player[["shots"]],UpdateShot)
		player <- MakePlayer(x=player[["x"]], y=player[["y"]], shots=newshots)

		# Renderiza tiros do jogador
		lapply(player[["shots"]],DrawShot)
	


		Sys.sleep(.01)
	}

}

game()
