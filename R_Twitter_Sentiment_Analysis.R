#***************************************************************************
#Datamining
#Autor: Inigo Montanchez
#Fecha: 17/9/2018
#Descripcion: Estudio de sentimientos con paquete sentiments en tematicas de tweets.
#Version: 1.0
#***************************************************************************

#Recomendacion para buen funcionamiento: 

#Para el buen funciomiento de la aplicacion se deberian ejecutar los pasos en el orden indicado.

#NO EJECUTAR LAS LINEAS COMENTADAS DE SEPARACION *****
#***************************************************************************



#instalacion de librerias que necesitamos 
#la libreria devtools nos permitira instalar Rstem y sentiment por comandos
#las librerias devtools y sentiments se instalaran a partir de los comandos
#abrir terminal del sistema operativo
#navegar hasta el directorio de los paquetes y ejecutar de forma separada:
#R CMD INSTALL Rstem_0.4-1.tar
#R CMD INSTALL sentiment_0.2.tar
#EN CASO DE DAR ERROR EN UN SISTEMA MAC QUIZA HAYA QUE LANZAR POR CONSOLA EL COMANDO
#xcode-select --install
#ambos archivos a instalar seran descargados de la pagina de CRAN
#se dispondra de ellos dentro de este repositorio.
#https://cran.r-project.org/src/contrib/Archive/Rstem/
#https://cran.r-project.org/src/contrib/Archive/sentiment/

# 1 PASO A EJECUTAR -> INSTALACION DE PAQUETES
install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")
install.packages("sentiment")
install.packages("devtools")
install.packages("ggplot2")


#***************************************************************************


# 2 PASO A EJECUTAR -> CARGAR LIBRERIAS
#cargamos las librerias
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(scales)

#***************************************************************************


# 3 PASO A EJECUTAR -> CARGAR CLAVES DE DESARROLLADOR
#a partir de la cuenta de desarrolladores de twitter se deben asociar los datos de cuenta
#https://apps.twitter.com
#esto  nos permitir?? conectar a twitter como aplicaci??n
consumer_key <- "83NdFjtxGPcMcD55BUpXwle5f"
consumer_secret <- "ty4hHcgQPxvn105TpvqWizP7QxgYjN8DhhnjweWxyVFYdFiCoU"
access_token <- "2191636523-HVKyuKUlATinIq9ZdeT7nAMcBNQKgqxgiM57sRu"
access_token_secret <- "LmBNxCSK8KhMR8qQjj2JNOA4xPQ0o30Tbuk02IIIisyLj"
#se realiza la conexion
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)

#LIMPIAR CONSOLA
cat("\014")  
#***************************************************************************


# 4 PASO A EJECUTAR -> ESPECIFICAR TEMATICA A ANALIZAR.
#se borra la variable primero con RM para evitar errores 
#escribimos una tematica a analizar
bool = exists("TEMATICA")
if(bool == TRUE){
rm(TEMATICA)#SI SALE ERROR NO PASA NADA 
print(paste("SE HA BORRADO CON RM, BOOL= ", bool))
}
if(exists("TEMATICA")==FALSE){

#LIMPIAR CONSOLA
cat("\014") 
  
TEMATICA=readline("ESCRIBA NOMBRE DE LA TEMATICA A ANALIZAR : ")
print(paste("TEMATICA INSERTADA => ", TEMATICA))

}

#***************************************************************************


# 5 PASO A EJECUTAR -> SE RECOGE LA INFORMACION DE TWITTER REFERENTE AL TEMA
#recogemos tweets siendo N el numero de tweets a recoger y LANG el lenguaje
#se puede poner en espanol ES pero funciona con menor exactitud
TWEET <-searchTwitter(TEMATICA, n=2000, lang="es")
TWEET #muestra tweets
TWEET[1:3] # muestra 3 tweets
class(TWEET)
TWEET_texto <- sapply(TWEET, function(x) x$getText()) #recogemos el texto 
typeof(TWEET_texto) #vemos que ahora es de tipo character

#PUEDE TARDAR EN EJECUTAR YA QUE DEBE RECOGER 2000 MENSAJES
#########
#FIN DE LA CONEXCION A TWITTER Y DE LA OPTENCI??N DEL TEXTO
#########


#***************************************************************************

# 6 PASO A EJECUTAR
#########
#DAMOS FORMATO AL TEXTO  HACEMOS USO DE FUNCIONES DEL PACKAGE "TM"
#########
#quitamos signos que suele contener twitter
TWEET_texto = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", TWEET_texto)
#eliminamos nombre de persona que ha twitteado
TWEET_texto = gsub("@\\w+", "", TWEET_texto)
#eliminamos signos de puntuaci??n
TWEET_texto = gsub("[[:punct:]]", "", TWEET_texto)
#eliminamos digitos
TWEET_texto = gsub("[[:digit:]]", "", TWEET_texto)
# eliminamos links a otras paginas y codigo html
TWEET_texto = gsub("http\\w+", "", TWEET_texto)
#eliminamos espacios
TWEET_texto = gsub("[ \t]{2,}", "", TWEET_texto)
TWEET_texto = gsub("^\\s+|\\s+$", "", TWEET_texto)

# ponemos todo a minusculas
minusculas_error = function(x)
{
  # valor vacio para funcion tolower
  y = NA
  minusculas_error = tryCatch(tolower(x), error=function(e) e)
  #si no hay error
  if (!inherits(minusculas_error, "error"))
    y = tolower(x)
  # resultado que devolveremos en TWEET_texto
  return(y)
}
# convertimos a minusculas usando la funci??n minusculas
TWEET_texto = sapply(TWEET_texto, minusculas_error)

# eliminamos valores no disponibles o vacios
TWEET_texto = TWEET_texto[!is.na(TWEET_texto)]
names(TWEET_texto) = NULL


#***************************************************************************

##################EJECUTAR EL RESTO PARA CONCLUIR ANALISIS################
########
# COMENZAMOS EL ANALISIS DE LAS EMOCIONES
########
# clasificamos la emocion a traves de funciones dadas por SENTIMENT
# EJEMPLO DE LA LIBRERIA: classify_emocion(documents,algorithm="bayes",verbose=TRUE)
clasificar_emocion = classify_emotion(TWEET_texto, algorithm="bayes", prior=1.0)
#conseguir un ajuste de las emociones
emocion = clasificar_emocion[,7]
# sustituir valores nulos o desconocidos por texto EMOCION DESCONOCIDA
emocion[is.na(emocion)] = "Emocion_Desconocida"

# clasificacion de polaridad, es decir, emociones negativas, neutras y positivas
clasificar_polaridad = classify_polarity(TWEET_texto, algorithm="bayes")
# ajustamos la polaridad
polarity = clasificar_polaridad[,4]

#############
#CREAMOS LOS GRAFICOS
#############
# creamos un dataframe
dataframe_sentimiento = data.frame(text=TWEET_texto, emocion=emocion,
                                   polarity=polarity, stringsAsFactors=FALSE)

# dataframe reducido
dataframe_sentimiento = within(dataframe_sentimiento,
                               emocion <- factor(emocion, levels=names(sort(table(emocion), decreasing=TRUE))))

#################
#GRAFICO GGPLOT
#################
# grafico de distribucion de emociones
ggplot(dataframe_sentimiento, aes(x=emocion)) +
  ggtitle(paste(TEMATICA))+ 
  theme(plot.title = element_text(hjust = 0.5)) + #AJUSTAR TITULO AL CENTRO
  geom_bar(aes(y=..count.., fill=emocion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emociones", y="numero de tweets")

###############
#grafico de distribucion de polaridad
###############
ggplot(dataframe_sentimiento, aes(x=polarity))+ 
  ggtitle(paste(TEMATICA))+ 
  theme(plot.title = element_text(hjust = 0.5)) + #AJUSTAR TITULO AL CENTRO
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="polaridades", y="numero de tweets") 

##############
#CREACION DE NUBE DE SENTIMIENTOS
##############
# separamos el texto por emociones
emociones = levels(factor(dataframe_sentimiento$emocion))
n_emocion = length(emociones)
emocionesdoc = rep("", n_emocion)
for (i in 1:n_emocion)
{
  tmp = TWEET_texto[emocion == emociones[i]]
  emocionesdoc[i] = paste(tmp, collapse=" ")
}

# eliminamos stopwords ESPECIFICAR SI ES EN INGLES O ESPANOL
emocionesdoc = removeWords(emocionesdoc, stopwords("spanish"))
# creamos un corpus
corpus = Corpus(VectorSource(emocionesdoc))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emociones

# nube de sentimientos
comparison.cloud(tdm, colors = brewer.pal(n_emocion, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

#IMPORTANTE AL EJECUTAR LOS PORCENTAJES SE ELIMINAN LOS ANTERIORES PLOTS
#######
#PORCENTAJES 
#######
# grafico de distribucion de emociones
ggplot(dataframe_sentimiento, aes(x=emocion)) +
  ggtitle(paste(TEMATICA))+ 
  theme(plot.title = element_text(hjust = 0.5)) + #AJUSTAR TITULO AL CENTRO
  geom_bar(aes(y=..count.., fill=emocion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emociones", y="numero de tweets")+ 
  scale_y_continuous(labels = percent_format())
#########
# grafico de distribucion de polaridad
ggplot(dataframe_sentimiento, aes(x=polarity)) +
  ggtitle(paste(TEMATICA))+ 
  theme(plot.title = element_text(hjust = 0.5)) + #AJUSTAR TITULO AL CENTRO
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="polaridades", y="numero de tweets")+ 
  scale_y_continuous(labels = percent_format())
#########


term.freq <- tdm %>% as.matrix() %>% rowSums()
term.freq <- term.freq %>% subset(term.freq>=20)
df <- data.frame(term=names(term.freq), freq=term.freq)
df <- df[order(df$freq,decreasing = TRUE),]
df



ggplot(df, aes(x=reorder(term, freq), y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Freq") + coord_flip() + theme(axis.text=element_text(size=7))

