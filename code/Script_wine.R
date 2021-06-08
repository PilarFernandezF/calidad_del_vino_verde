
#Por defecto coma (,) como separador y punto (.) como separador decimal

#Lectura fichero winequality-red.csv
#Por defecto los campos vienen separados por coma (,) y los decimales por punto(.) como en nuestro fichero. 
#Indicamos con el valor TRUE en header que el fichero viene con encabezado
datos_vino<-read.csv('C:\\PRA2\\winequality-red.csv', header=TRUE)	

#Leemos las primeras filas del fichero
head(datos_vino)

#Comprobamos el tipo de datos que se han leído
sapply(datos_vino, function(x) class(x))

#LIMPIEZA DE LOS DATOS
#Comprobamos si tenemos registros duplicados
datos_duplicados<-datos_vino[duplicated(datos_vino),]
#Contamos las filas duplicadas
nrow(datos_duplicados)

#Contamos las filas que tenemos en el dataframe antes del borrado
nrow(datos_vino)
#Procedemos al borrado de las filas duplicadas
datos_vino<-datos_vino[!duplicated(datos_vino),]
#Contabilizamos las filas que nos quedan en el dataframe
nrow(datos_vino)

#Vamos a comprobar si existen elementos vacíos
sapply(datos_vino, function(x) sum(is.na(x)))

#Vamos a comprobar si existen valores a cero
sapply(datos_vino, function(x) sum(x == 0))

#COMPROBACIÓN DE OUTLIERS
#Outliers de fixed.acidity
boxplot(datos_vino$fixed.acidity)$out

#Outliers de volatile.acid
boxplot(datos_vino$volatile.acid)$out

#Outliers de citric.acid
boxplot(datos_vino$citric.acid)$out

#Outliers de residual.sugar
boxplot(datos_vino$residual.sugar)$out

#Outliers de chlorides
boxplot(datos_vino$chlorides)$out

#Outliers de free.sulfur.dioxide
boxplot(datos_vino$free.sulfur.dioxide)$out

#Outliers de total.sulfur.dioxide
boxplot(datos_vino$total.sulfur.dioxide)$out
#Eliminamos las dos filas que tiene outlieres: 289 y 278
datos_vino<-datos_vino[!(datos_vino$total.sulfur.dioxide == 289),]
datos_vino<-datos_vino[!(datos_vino$total.sulfur.dioxide == 278),]
#Comprobamos que se han eliminado las filas de los outliers
boxplot(datos_vino$total.sulfur.dioxide)$out

#Outliers de density
boxplot(datos_vino$density)$out

#Outliers de pH
boxplot(datos_vino$pH)$out

#Outliers de sulphates
boxplot(datos_vino$sulphates)$out

#Outliers de alcohol
boxplot(datos_vino$alcohol)$out

#Outliers de quality
boxplot(datos_vino$quality)$out

#Exportamos el fichero después de la limpieza de datos
write.csv(datos_vino, 'C:\\PRA2\\winequality-red_clean.csv')

#Calculamos la normalidad
alpha=0.05
col.names =colnames(datos_vino)
for (i in 1:ncol(datos_vino))
{
	if(i==1) cat ("Variables que no siguen una distribución normal:\n")
	if(is.integer(datos_vino[,i]) | is.numeric(datos_vino[,i]))
	{
		p_valor = shapiro.test(datos_vino[,i])$p.value
		if (p_valor<alpha)
		{
			cat(col.names[i])
			#Formato de salida
			if (i <= ncol(datos_vino) - 1) cat(", \n")
		}
	}	
}


#Cálculo de la homogeneidad de la varianza
#Recuperamos los valores que queremos ver la relación
volatile.acid<-datos_vino$volatile.acid
density<-datos_vino$density
pH<-datos_vino$pH
alcohol<-datos_vino$alcohol
quality<-datos_vino$volatile.acid

#Test de Fligner-Killen
#volatile-acid y quality
print(fligner.test(volatile.acid ~ quality, data = datos_vino))
#density y quality
print(fligner.test(density ~ quality, data = datos_vino))
#pH y quality
print(fligner.test(pH ~ quality, data = datos_vino))
#alcohol y quality
print(fligner.test(alcohol ~ quality, data = datos_vino))


#Correlación con el test de Spearman entre cada variable y la calidad
matriz_correlacion<-matrix(nc=2, nr=0)
colnames(matriz_correlacion)<-c("estimate", "p-value")
#Eliminamos la calidad ya que haremos el test con ella
for (i in 1:(ncol(datos_vino) -1))
{
	if(is.integer(datos_vino[,i]) | is.numeric(datos_vino[,i]))
	{
		test_Spearman =  cor.test(datos_vino[,i],
					        datos_vino$quality,
						  method= "spearman",
						  exact = FALSE)

		coeficiente_correlacion = test_Spearman$estimate
		p_valor = test_Spearman$p.value	
		
		#Añadir fila a la matriz a mostrar
		pair = matrix(ncol=2, nrow=1)
		pair[1][1] = coeficiente_correlacion
		pair[2][1] = p_valor
		matriz_correlacion<-rbind(matriz_correlacion, pair)
		rownames(matriz_correlacion)[nrow(matriz_correlacion)]<-colnames(datos_vino)[i]
	}
}
#Imprimimos la matriz con los resultados obtenidos
print(matriz_correlacion)


#Contraste de hipótesis
wilcox.test(densidad ~ quality, data=datos_vino, subset = quality %in% c(5,8))

#Regresión lineal
acidez_volatil = datos_vino$volatile.acid
alcohol = datos_vino$alcohol
pH = datos_vino$pH
densidad = datos_vino$density
acidez_fija = datos_vino$fixed.acidity
acido_citrico = datos_vino$citric.acid
azucar_residual = datos_vino$residual.sugar
sulfatos = datos_vino$sulphates
cloruros = datos_vino$chlorides
sulfitos_libres = datos_vino$free.sulfur.dioxide
sulfitos_totales = datos_vino$total.sulfur.dioxide
calidad = datos_vino$quality

modelo_A <- lm(calidad ~ acidez_volatil + alcohol + pH + densidad, data = datos_vino)

modelo_B <- lm(calidad ~ acidez_fija + acido_citrico + azucar_residual + sulfatos + alcohol, data= datos_vino)

modelo_C <- lm(calidad ~ acidez_volatil + cloruros + sulfitos_libres + sulfitos_totales + densidad + pH, 
	         data = datos_vino)

print(summary(modelo_A)$r.squared)
print(summary(modelo_B)$r.squared)
print(summary(modelo_C)$r.squared)


#Representación gráfica
#Normalidad mediantes histogramas
hist(datos_vino$quality)

hist(datos_vino$volatile.acid)
qqnorm(datos_vino$volatile.acid, main="datos_vino")

