Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#devtools::install_github("Mikata-Project/ggthemr")

## Desabilitar notación científica.----
options(scipen = 999)
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#devtools::install_github("Mikata-Project/ggthemr")

## Desabilitar notación científica.----
options(scipen = 999)
# Paquetes
library(pacman)
p_load(readxl, tidyverse, dplyr, cowplot, janitor, lmtest, ldatuning,
       sandwich, sjPlot, pander, pscl, haven, gridExtra, ggExtra, ggrepel,
       hexbin, janitor, mosaicData, scales, ggthemes, rtweet,ggraph,
       lubridate, hms, tidytext, wordcloud2, tm, SnowballC, htmlTable, kableExtra,
       magick, magrittr, scales, textdata, syuzhet, visNetwork, stringi, topicmodels,
       ggthemr, viridis, forcats, rvest, writexl, quanteda, zoo)

#Aplicar estilo gráficas (opcional)
ggthemr('pale')

# ----------------------  FUNCIONES (SIEMPRE EJECUTAR)  -------------------- #

# Stop Words

custom_stop_words <- as_tibble(tm::stopwords("es")) %>% 
  bind_rows(as_data_frame(c(
    "si","dijo","así","sólo", "dice", "pues","entonces",
    "ahí","digo","creo","que","en","la","ah","bueno", "bla","tan",
    "te", "iba", "he", "él", "t", "+", "de", "cómo", "su", "andrés manuel", "lópez obrador","manuel lópez",
    "presidente andrés","mil millones","unavailable","media","policy","because","it","violates","learn"
  ))) %>% 
  dplyr::rename(palabra = value)

stopwords <- as.matrix(custom_stop_words)

# ----------------------  OBTENER MAÑANERAS -------------------- #

# Automatizar URL
  ## Fecha
  fecha_inicial <-  as_date('2023/01/29', format="%Y/%m/%d") #Fecha de última mañanera
  #fecha_inicial - 10
  #fecha_final <- str_replace_all(fecha_inicial,"-", "/")

i <- 2000 #Número de última mañanera - 797
df <- data.frame()
df2 <- data.frame()



#Loop para recoger mañaneras
while (i>0) {
  
  tryCatch( { 
    fecha_inicial = fecha_inicial - 1
    i = i-1
    #open connection to url 
    fecha_final <- str_replace_all(fecha_inicial,"-", "/")
    
    #Extraer página
    
    paginaweb <- paste("https://lopezobrador.org.mx/",fecha_final,"/version-estenografica-de-la-conferencia-de-prensa-matutina") 
    paginafinal <- str_replace_all(paginaweb, fixed(" "), "")
    
    html <- session(paginafinal)
    
    #Extraer texto
    
    
    ## Obtener personaje
    ### Como el tag es distino en algunas entradas, entonces será necesario crear dos columnas
    ### que, después de recoger todas las mañaneras, vamos a consolidar en una. 
    personaje <- html %>% 
      html_nodes("p") %>% 
      str_match("<strong>\\s*(.*?)\\s*</strong>")
    
    personaje2 <- html %>% 
      html_nodes("p") %>% 
      str_match("<b>\\s*(.*?)\\s*</b>")
    
    
    ## Llenar los campos de acuerdo al personaje en cada columna
    
    ### Personaje 1
    personaje <- as_tibble(personaje) %>% 
      select(Personaje = V2) %>% 
      ### Además, llenamos los campos vaciós de tal forma que la conversación esté 
      ### bien asignada al personaje correspondiente
      fill(Personaje, .direction = "downup") %>% 
      mutate(id = row_number()) %>% 
      select(id, Personaje)
    
    ### Personaje 2
    personaje2 <- as_tibble(personaje2) %>% 
      select(Personaje2 = V2) %>% 
      fill(Personaje2, .direction = "downup") %>% 
      mutate(id = row_number()) %>% 
      select(id, Personaje2)
    
    ## Obtener texto
    texto <- html %>%
      html_nodes("p") %>% 
      html_text() %>% 
      as_tibble() %>% 
      rename(Texto = value) %>% 
      mutate(id = row_number()) %>% 
      select(id, Texto)
    
    ## Unir tablas y obtener texto y lo asignamos al personaje que le corresponde
    resultado_previo <- personaje %>%
      left_join(texto, by="id") %>%
      left_join(personaje2, by="id") %>%
      mutate(fecha = fecha_final) %>% 
      select(id, Personaje, Personaje2, Texto, fecha)
    
    
    ## Unir todas las tablas de cada mañanera para así tener una base de datos 
    ## con todas las mañaneras
    df2<- rbind(df2,resultado_previo)
    ## Aquí se muestran las fechas en que no se puede extraer la url. Básicamente
    ## corresponde a las fechas en las que no hubo mañanera o no se registró.
  }, error = function(e){
    print(sprintf("No funciona"))
  }
  )
  #Iteración de página final
  print(paginafinal)
  Sys.sleep(0)
}

# ----------------------  LIMPIEZA BASE DE DATOS -------------------- #

# Consolidamos las dos tablas de los personajes y nos deshacemos de la columna
# Personaje 2 que ya no necesitaremos
final <- df2 %>% 
  mutate(Personaje = ifelse(Personaje=="+++++", Personaje2, Personaje)) %>% 
  select(id, Personaje, Texto, fecha)

         
# Guardamos la base de datos         
write_excel_csv(final,"mañaneras.csv")

# Abrimos la base de datos que creamos para no repetir el proceso
mañaneras <- read_csv("mañaneras.csv")

# Vamos a hacer limpieza de datos

#Primero vamos a eliminar caracteres innecesarios:

mañaneras <- mañaneras %>% 
  mutate(Personaje = gsub("<.*?>", "", Personaje)) %>% 
  mutate(Personaje = str_replace_all(Personaje, "[[:punct:]]", " "))

## Dado que hay inconsistencias en los nombres de los personajes, queremos 
## homologarlos y mostrarlos de una forma sencilla. Para ello primero vamos
## a ver los distintos nombres para después simplificar los que nos interesan.

mañaneras %>% 
  group_by(Personaje) %>%
  count(Personaje) %>% 
  view()
  
mañaneras <- mañaneras %>% 
  mutate(Persona = case_when(str_detect(Personaje, "OBRADOR") ~ "AMLO",
         str_detect(Personaje, "EBRARD") ~ "Marcelo Ebrard",
         str_detect(Personaje, "GATELL") ~ "López-Gatell",
         str_detect(Personaje, "VILCHIS") ~ "García Vilchis",
         str_detect(Personaje, "CRESENCIO") ~ "Luis Crescencio (Sec. Defensa Nacional)",
         str_detect(Personaje, "SHEFFIELD") ~ "Ricardo Sheffield (Proc. del consumidor)",
         str_detect(Personaje, "ALCOCER") ~ "Jorge Alcocer (Sec. Salud)",
         str_detect(Personaje, "RICARDO MEJÍA") ~ "Ricardo Mejía (Sub Seguridad Pública)",
         str_detect(Personaje, "ROSA ICELA") ~ "Rosa Icela Rodríguez (Sec Seguridad)",
         str_detect(Personaje, "ZOÉ ROBLEDO") ~ "Zoe Robledo (IMSS)",
         str_detect(Personaje, "RAFAEL OJEDA") ~ "Rafael Ojeda (Sec. Marina)",
         str_detect(Personaje, "BUCIO") ~ "Luis R. Bucio (Comandante Guardia Nacional)",
                             TRUE ~ Personaje) 
         ) %>% 
  #Y arreglamos el orden de las columnas
  select(id, Persona, Texto, fecha)

#Y ya tenemos lista la base de datos con la que vamos a trabajar

# ----------------------  LENTITUD DE AMLO -------------------- #

palabras_por_mañanera <- mañaneras %>% 
  mutate(palabras = sapply(gregexpr("\\S+", Texto), length)) %>% 
  group_by(fecha) %>% 
  summarize(suma = sum(palabras)) %>% 
  ungroup()

palabras_amlo <- mañaneras %>% 
  filter(Persona == "AMLO") %>% 
  mutate(palabras = sapply(gregexpr("\\S+", Texto), length)) %>% 
  group_by(fecha, Persona) %>% 
  summarize(pab_amlo = sum(palabras)) %>% 
  ungroup()
  
  
participacion_amlo <- palabras_por_mañanera %>% 
  left_join(palabras_amlo, by="fecha") %>% 
  mutate(Persona = ifelse(is.na(Persona), "AMLO", Persona )) %>% 
  mutate(pab_amlo = ifelse(is.na(pab_amlo), 0, pab_amlo )) %>% 
  mutate(promedio_AMLO = (pab_amlo/suma)) %>% 
  rename(total_palabras = suma) %>% 
  filter(promedio_AMLO != 0 & promedio_AMLO != 1)


reg <- lm(total_palabras ~ promedio_AMLO, data = participacion_amlo)
cor(participacion_amlo$total_palabras, participacion_amlo$promedio_AMLO)

summary(reg)

palabras_por_mañanera %>% 
  ggplot(aes(fecha, suma))+
  geom_point()+
  geom_smooth(span = 0.8, method="loess", se = FALSE, color="#333333")+
  #scale_x_continuous(labels = scales::percent)+
  labs(title = 'Palabras por mañanera',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "Por día del año",
       y = "Número de palabras totales")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 16, family="Futura LT")
  )

participacion_amlo %>% 
  ggplot(aes(promedio_AMLO, total_palabras))+
  geom_point()+
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent)+
  labs(title = 'Lentitud de AMLO',
       subtitle = 'A mayor participación de AMLO, menos palabras se pronuncian\nen la mañanera',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "Porcentaje participación AMLO",
       y = "Número de palabras totales")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 16, family="Futura LT")
  )
  
# Monopolio AMLO

participacion_amlo %>% 
  ggplot(aes(fecha, promedio_AMLO))+
  geom_point()+
  geom_smooth(span = 0.8, method="loess", se = FALSE, color="#333333")+
  scale_y_continuous(labels = scales::percent)+
  labs(title = '¿Qué tanto acapara AMLO las mañaneras?',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "Fecha",
       y = "Porcentaje participación AMLO")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 16, family="Futura LT")
  )


# ----------------------  ¿QUIENES PARTICIPAN MÁS? -------------------- #

participaciones <- mañaneras %>% 
  filter((!str_detect(Persona, "PREGUNTA|INTERLOCUTOR|VOZ MUJER|VOZ HOMBRE"))) %>% 
  mutate(palabras = sapply(gregexpr("\\S+", Texto), length)) %>% 
  group_by(Persona) %>% 
  summarize(palabras = sum(palabras)) %>% 
  filter(palabras > 30000) %>%
  arrange(desc(palabras))

participaciones %>% 
  ggplot(aes(reorder(Persona, palabras), palabras, label = palabras))+
  geom_col()+
  geom_label()+
  labs(title = '¿Quiénes participan\nmás?',
       subtitle = 'Palabras pronunciadas en mañaneras',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  coord_flip()+ 
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 12, family="Futura LT")
  )

  
participaciones_fecha <- mañaneras %>% 
  filter((!str_detect(Persona, "PREGUNTA|INTERLOCUTOR|VOZ MUJER|VOZ HOMBRE"))) %>% 
  mutate(palabras = sapply(gregexpr("\\S+", Texto), length)) %>% 
  #mutate(mes = month(fecha)) %>% 
  #mutate(año = format(fecha, format="%Y")) %>%
  group_by(mes = floor_date(fecha, "month"), Persona) %>% 
  summarize(palabras = sum(palabras)) %>% 
  #arrange(año, mes) %>% 
  #mutate(mes_año = paste(año, mes, sep = '-')) %>% 
  filter(Persona == "AMLO" | Persona == "López-Gatell" | 
           Persona == "Luis Crescencio (Sec. Defensa Nacional)" | 
           Persona == "Marcelo Ebrard" | Persona == "García Vilchis" |
           Persona == "Zoe Robledo (IMSS)" | 
           Persona == "Rafael Ojeda (Sec. Marina)" |
           Persona == "Luis R. Bucio (Comandante Guardia Nacional)" |
           Persona == "Rosa Icela Rodríguez (Sec Seguridad)" |
           Persona == "Ricardo Mejía (Sub Seguridad Pública)")

glimpse(participaciones_fecha)

participaciones_fecha %>% 
  ggplot(aes(mes, palabras))+
  geom_col()+ 
  geom_smooth(span = 0.8, method="loess", se = FALSE, color="#333333")+
  facet_wrap(~Persona, ncol=2, scales = "free")+
  labs(title = 'Palabras mencionadas por mañanera',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "Promedio palabras por mes",
       y = "")+
  scale_x_date(
    date_breaks = "12 months", 
    labels=date_format("%Y"),
    limits = as.Date(c('2018-12-01','2023-01-29')))+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 12, family="Futura LT")
  )

# ---------------------- Bigramas -------------------- #

filtrar_bigramas <- c("andrés manuel","lópez obrador", "manuel lópez", "presidente andrés",
             "mil millones", "NA NA", "puede ser", "obrador buenos", "dos mil", "todas maneras",
             "cada vez", "hace falta", "año pasado", "puedo decir", "mismo tiempo", "buenos días",
             "muchas gracias", "señor presidente", "tres mil", "mil pesos", "obrador vamos", "puede haber",
             "año próximo", "mil 500", "10 mil","2011 2020", "copyright derechos", "2020 sitio", 
             "derechos reservados", "reservados 2011", "sitio oficial","lópez gatell", "hugo lópez", "gatell ramírez",
             "quiere decir", "ramírez subsecretario", "luis cresencio", "sandoval gonzález","gonzález secretario",
             "tendencia hacia", "cuatro mil", "aquí vemos", "cinco mil", "siete mil", "seis mil", "cada 100",
             "dos millones", "cresencio sandoval", "100 mil", "marcelo ebrard", "ebrard casaubon", 
             "casaubon secretario", "ustedes saben", "primer lugar", "garcía vilchis", "elizabeth garcía", 
             "ana elizabeth", "vilchis buenos")

bigramas <- mañaneras %>%
  filter(Persona == "AMLO") %>%
  dplyr::select(Texto) %>% 
  #mutate(texto = limpiar(cuentas)) %>%
  #select(texto) %>%
  unnest_tokens(input = Texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE) %>% 
  separate(bigrama, c("palabra1", "palabra2"),
           sep = " ") %>% 
  filter(!palabra1 %in% stopwords) %>%
  filter(!palabra2 %in% stopwords) %>% 
  unite(palabra1, palabra2,col="bigrama",sep=" ") %>% 
  filter(!bigrama %in% filtrar_bigramas) %>% 
  group_by(bigrama) %>% 
  count() %>% 
  arrange(desc(n)) %>%   
  head(100)

bigramas

bigramas %>% 
  ggplot(aes(reorder(bigrama,n), n)) +
  geom_col() +
  coord_flip()+
  labs(title = 'Frases de García Vilchis',
       subtitle = 'Que menciona más en las mañaneras',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 16, family="Futura LT")
  )

#------- CORRELACIONES ---------------

discurso

discurso <- mañaneras %>% 
  filter(Persona == "AMLO") %>% 
  dplyr::select(Texto) %>% 
  rowid_to_column("doc_id")



corpus <- VCorpus(VectorSource(df$text))

myCorpus <- Corpus(VectorSource(discurso$Texto))

colapsar <- as.character(unlist(myCorpus, use.names=FALSE))

peyorativo <- c("neoliberalismo", "neoliberal", "conservador", "conservadores", 
                "fifi", "fifis", "mafia del poder", "prian", "adversarios")

medios_adversos <- c("el reforma", "periódico reforma", "diario reforma", "latinus",
                     "televisa","milenio","el universal")

periodistas_adversos <- c("loret","krauze","dresser","brozo","aristegui","herzog","zuckermann",
                          "ferríz","jorge ramos","camín","alazraki", "gómez leyva", "ciro gómez",
                          "lópez dóriga", "castañeda", "vargas llosa", "chumel")

democracia <- c("democracia","elecciones","el ine","libertad de expresión","pluralismo","ciudadanía",
                "manifestación", "libertades")

progresismo <- c("feminismo","mujeres","lgbt","homosexuales","matrimonio igualitario","aborto",
                 "derechos humanos", "feminicidio", "machismo", "homofobia", "equidad de género",
                 "inclusión","empoderamiento", "tolerancia","diversidad")

conservadurismo <- c("autoridad","disciplina","orden","tradición","religión",
                     "iglesia", "valores", "moral", "patriotismo","responsabilidad","pecado")

economía <- c("inversión", "empresas", "mercado", "dinero", "capital", "economía", "empleo", 
              "sector privado", "iniciativa privada", "crecimiento")

sociales <- c("programas sociales", "despensas", "sembrando vida", "construyendo el futuro",
              "adultos mayores", "pensión", "bienestar")

ciencia <- c("ciencia", "científicos", "conacyt", "investigación", "UNAM", "ipn", "innovación")

INE <- c("lorenzo córdova", "woldenberg", "el ine", "instituto nacional electoral", "reforma electoral",
         "consejeros")

feminismo <- c("feminismo", "feminismos", "feminista", "feministas")

resultado <- kwic(colapsar, INE, window = 8)

res_procesado <- as_tibble(resultado) %>%
  dplyr::select(pre,post) %>% 
  unite("Texto", pre:post, sep=" ")

#Por palabra

res_palabra <- 
  res_procesado %>%
  unnest_tokens(input = "Texto", output = "Palabra") %>% 
  filter(!Palabra %in% stopwords)

res_palabra %>%
  group_by(Palabra) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) 

#Por bigrama

filtrar_bigramas <- c("lópez obrador", "andrés manuel", "cada vez", "dos dos", 
                      "manuel lópez", "presidente andrés", "ustedes dos", "dos tres")
filtrar_bigramas <- c("lópez obrador", "andrés manuel", "cada vez", "dos dos", 
                      "manuel lópez", "carmen aristegui", "lópez doriga", 
                      "jesús silva", "don jesús", "denise dresser", "x gonzález",
                      "claudio x", "mola carmen", "dóriga ciro", "joaquín lópez",
                      "aguilar camín", "presidente andrés", "señora denise", 
                      "silva herzog", "ciro carmen", "lópez dóriga", "krauze aguilar",
                      "enrique krauze", "héctor aguilar", "mola etcétera", "aguilar krauze",
                      "gómez leyva", "woldenberg aguilar", "camín etcétera", 
                      "camín loret", "castañeda aguilar", "aguilar etcétera", "riva palacio",
                      "ciro gómez", "chumel denise", "mola denise")
filtrar_bigramas <- c("lópez obrador", "andrés manuel", "cada vez", "dos dos", 
                      "manuel lópez", "presidente andrés", "movimiento feminista", "primer movimiento")

res_palabra <- 
  res_procesado %>%
  unnest_tokens(input = Texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE) %>% 
  separate(bigrama, c("palabra1", "palabra2"),
           sep = " ") %>% 
  filter(!palabra1 %in% stopwords) %>%
  filter(!palabra2 %in% stopwords) %>% 
  unite(palabra1, palabra2,col="bigrama",sep=" ") %>% 
  filter(!bigrama %in% filtrar_bigramas) %>% 
  group_by(bigrama) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10)

res_palabra %>% 
  ggplot(aes(reorder(bigrama,n), n)) +
  geom_col() +
  coord_flip()+
  labs(title = 'INE',
       subtitle = '"lorenzo córdova", "woldenberg", "el ine", "instituto nacional electoral",\n"reforma electoral",
         "consejeros"',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 16, family="Futura LT")
  )
  
#------- GÉNERO ---------------

discurso <- mañaneras %>% 
  filter(Persona == "AMLO") %>% 
  dplyr::select(Texto) %>% 
  rowid_to_column("doc_id")



res_palabra <- discurso %>%
  unnest_tokens(input = "Texto", output = "Palabra") %>% 
  filter(!Palabra %in% stopwords)

res_palabra %>% 
  filter(Palabra == c("mujer", "mujeres", "hombre", "hombres")) %>% 
  group_by(Palabra) %>% 
  summarize(total = n())

#------- ÁNALISIS AMLO ---------------

#Obras

Aeropuerto <- c("felipe ángeles", "santa lucía", "nuevo aeropuerto", "aifa")
Tren_maya <- c("tren maya")
Dos_Bocas <- c("dos bocas", "refinería")

#Exoresiones

Neoliberalismo <- c("neoliberal", "neoliberalismo")
Conservadores <- c("conservador", "conservadores")
Fifis <- c("fifí", "fifís")
Mafia <- c("mafia del poder")
Prian <- c("prian")
Adversarios <- c("adversarios")

#Instituciones que ve como adversas

Reforma <- c("el reforma", "periódico reforma", "diario reforma")
Latinus <- c("latinus")
Televisa <- c("televisa")
TV_Azteca <- c("tv azteca", "televisión azteca")
Milenio <- c("milenio")
Universal <- c("el universal")


#Personajes adversos

Loret <- c("loret")
Krauze <- c("krauze")
Dresser <- c("dresser")
Brozo <- c("brozo")
Aristegui <- c("aristegui")
Majluf <- c("majluf")
Schettino <- c("schettino")
Herzog <- c("herzog")
Patan <- c("julio patán")
Zuckermann <- c("zuckermann")
Ferriz <- c("ferriz")
Ramos <- c("jorge ramos")
Castañeda <- c("jorge castañeda")
Camín <- c("camín")
Alazraki <- c("alazraki")


#Políticos y activistas

Lorenzo <- c("lorenzo córdova", "córdova")
Hoyos <- c("de hoyos")
Chumel <- c("chumel")
Claudio_x <- c("claudio x González")
Calderón <- c("calderón")
Fox <- c("fox")
Peña <- c("presidente peña", "peña nieto")
Salinas <- c("salinas de gortari", "carlos salinas")
Biden <- c("biden")
Trump <- c("trump")
Evo <- c("evo morales, evo")

#Corcholatas

Ebrard <- c("ebrard")
Sheinbaum <- c("sheinbaum")
Adan_augusto <- c("adan augusto", "augusto")
Monreal <- c("monreal")

list_data <- list(Aeropuerto, Tren_maya, Dos_Bocas, Neoliberalismo, Conservadores, Fifis, 
                  Mafia, Prian, Adversarios, Reforma, Latinus, Televisa, TV_Azteca, Milenio, Universal, Loret, Krauze, Dresser, Brozo, Aristegui, 
                  Majluf, Schettino, Herzog, Patan, Zuckermann, Ferriz, Ramos, Castañeda, Camín, Alazraki, Lorenzo, Hoyos, Chumel, Claudio_x, Calderón, Fox, Peña, Salinas,
                  Biden, Trump, Evo, Ebrard, Sheinbaum, Adan_augusto, Monreal)
list_text <- c("Aeropuerto", "Tren_maya", "Dos_Bocas", "Neoliberalismo", "Conservadores","Fifis", 
               "Mafia del poder", "Prian", "Adversarios", "Reforma", "Latinus", "Televisa", "TV Azteca", "Milenio", "El Universal",  "Loret de Mola", "Krauze",
               "Denise Dresser", "Brozo", "Aristegui", "Pablo Majluf", "Macario Schettino", "Silva Hérzog", "Julio Patán",
               "Leo Zuckermann", "Pedro Ferriz", "Jorge Ramos", "Jorge Castañeda", "Aguilar Camín", "Carlos Alazraki", "Lorenzo Córdova", "Chumel Torres", "Gustavo de Hoyos", "Claudio X González", 
               "Felipe Calderón", "Vicente Fox", "Enrique Peña Nieto", "Carlos Salinas de Gortari", "Joe Biden", "Donald Trump", "Evo", "Marcelo Ebrard", "Claudia Sheinbaum",
               "Adan Augusto", "Ricardo Monreal")




db1 <- mañaneras %>% 
filter(Persona == "AMLO") 


i = 1

for (x in list_data) {
  db1 <- db1 %>% 
    group_by(fecha) %>% 
    mutate(!!list_text[i]:= str_count(tolower(Texto),paste0(x, collapse = '|'))) %>%
    ungroup()
  i = i+1 
}

db_final <- db1 %>% 
  group_by(fecha) %>% 
  summarize(Aeropuerto = sum(Aeropuerto), 
            Tren_maya = sum(Tren_maya),
            Dos_Bocas = sum(Dos_Bocas),
            Neoliberalismo = sum(Neoliberalismo),
            Conservadores = sum(Conservadores),
            Fifis = sum(Fifis),
            `Mafia del poder` = sum(`Mafia del poder`),
            Prian = sum(Prian),
            Adversarios = sum(Adversarios),
            Reforma = sum(Reforma),
            Latinus = sum(Latinus),
            Televisa = sum(Televisa),
            `TV Azteca`= sum(`TV Azteca`),
            Milenio = sum(Milenio),
            `El Universal` = sum(`El Universal`),
            `Loret de Mola` = sum(`Loret de Mola`),
            Krauze = sum(Krauze),
            `Denise Dresser` = sum(`Denise Dresser`),
            `Brozo` = sum(`Brozo`),
            Aristegui = sum(Aristegui),
            `Macario Schettino` = sum(`Macario Schettino`),
            `Silva Hérzog` = sum(`Silva Hérzog`),
            `Pablo Majluf` = sum(`Pablo Majluf`),
            `Julio Patán` = sum(`Julio Patán`),
            `Leo Zuckermann` = sum(`Leo Zuckermann`),
            `Pedro Ferriz` = sum(`Pedro Ferriz`),
            `Jorge Ramos` = sum(`Jorge Ramos`),
            `Jorge Castañeda` = sum(`Jorge Castañeda`),
            `Aguilar Camín` = sum(`Aguilar Camín`),
            `Carlos Alazraki` = sum(`Carlos Alazraki`),
            `Lorenzo Córdova` = sum(`Lorenzo Córdova`),
            `Gustavo de Hoyos` = sum(`Gustavo de Hoyos`),
            `Chumel Torres` = sum(`Chumel Torres`),
            `Claudio X González` = sum(`Claudio X González`),
            `Felipe Calderón` = sum(`Felipe Calderón`),
            `Vicente Fox` = sum(`Vicente Fox`),
            `Enrique Peña Nieto` = sum(`Enrique Peña Nieto`),
            `Carlos Salinas de Gortari` = sum(`Carlos Salinas de Gortari`),
            `Joe Biden` = sum(`Joe Biden`),
            `Donald Trump` = sum(`Donald Trump`),
            `Marcelo Ebrard` = sum(`Marcelo Ebrard`),
            `Claudia Sheinbaum` = sum(`Claudia Sheinbaum`),
            `Adan Augusto` = sum(`Adan Augusto`),
            `Ricardo Monreal` = sum(`Ricardo Monreal`)
            
            
            
            ) %>% 
  arrange(fecha)
  
glimpse(db_final)


view(finale)

db_final %>% 
  mutate(mes = month(fecha), año =  year(fecha)) %>%
  mutate(fecha_nueva = as.yearmon(paste(mes, año), "%m %Y")) %>% 
  view()

ml <- db_final %>% 
  gather(concepto, número, `Loret de Mola`:`Carlos Alazraki`) %>% 
  select(fecha, concepto, número) %>% 
  mutate(mes = month(fecha), año =  year(fecha)) %>%
  mutate(fecha_nueva = as.yearmon(paste(mes, año), "%m %Y")) %>% 
  #mutate(fecha_nueva = as.yearmon(paste(año), "%Y")) %>%
  filter(fecha_nueva < '2023-01-01') %>% 
  mutate(fecha_nueva = as.Date(fecha_nueva)) %>% 
  group_by(fecha_nueva) %>% 
  summarize(número = sum(número, na.rm=T)) %>% 
  ungroup() 
  
  
db_final %>% 
  gather(concepto, número, `Marcelo Ebrard`:`Ricardo Monreal`) %>% 
  select(fecha, concepto, número) %>% 
  mutate(mes = month(fecha), año =  year(fecha)) %>%
  mutate(fecha_nueva = as.yearmon(paste(mes, año), "%m %Y")) %>% 
  #mutate(fecha_nueva = as.yearmon(paste(año), "%Y")) %>%
  filter(fecha_nueva < '2023-01-29') %>% 
  mutate(fecha_nueva = as.Date(fecha_nueva)) %>% 
  group_by(concepto, fecha_nueva) %>% 
  summarize(número = sum(número, na.rm=T)) %>% 
  ungroup() %>% 
  filter(concepto != 'Pablo Majluf' & concepto != 'Julio Patán' &
           concepto != 'Macario Schettino') %>% 
  ggplot(aes(fecha_nueva, número, label=número)) +
  geom_point() +
  #geom_label() +
  geom_smooth(se = FALSE, span = 0.7, color = "black")+
  facet_wrap(~concepto, ncol=3)+
  scale_x_date(date_labels = "%Y", date_breaks= "year")+
  scale_y_continuous(trans='log2')+
  labs(title = 'Menciones de políticos',
       subtitle = 'AMLO (por mes) - Escala logarítmica',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  #coord_flip()+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.margin.y = unit(2, "lines"),
    axis.text = element_text(size = 16, family="Futura LT")
  )

db_final %>% 
  gather(concepto, número, `Marcelo Ebrard`:`Ricardo Monreal`) %>% 
  select(concepto, número) %>% 
  group_by(concepto) %>% 
  summarize(número = sum(número, na.rm=T)) %>% 
  filter(número > 0) %>% 
  ggplot(aes(reorder(concepto, número), número))+
  geom_col()+
  #geom_label()+
  coord_flip()+
  labs(title = 'Menciones de políticos',
       subtitle = 'AMLO',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 16, family="Futura LT")
  )

#Esto es para guardar la base de datos.
ml <- db_final %>% 
  gather(concepto, número, `Loret de Mola`:`Carlos Alazraki`) %>% 
  select(fecha, concepto, número) %>% 
  mutate(dia = day(fecha), mes = month(fecha), año =  year(fecha)) %>%
  mutate(fecha_nueva = as.yearmon(paste(dia, mes, año), "%d %m %Y")) %>% 
  #mutate(fecha_nueva = as.yearmon(paste(año), "%Y")) %>%
  filter(fecha_nueva < '2023-01-01') %>% 
  mutate(fecha_nueva = as.Date(fecha_nueva)) %>% 
  group_by(fecha_nueva) %>% 
  summarize(número = sum(número, na.rm=T)) %>% 
  ungroup()

write_excel_csv(ml,"adversarios.csv")


#------- ANÁLISIS DE SENTIMIENTOS ---------------

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

db2 <- mañaneras %>% 
  filter(Persona == "AMLO" | Persona == "López-Gatell" | 
           Persona == "Luis Crescencio (Sec. Defensa Nacional)" | 
           Persona == "Marcelo Ebrard" | Persona == "García Vilchis" |
           Persona == "Zoe Robledo (IMSS)" | 
           Persona == "Rafael Ojeda (Sec. Marina)" |
           Persona == "Luis R. Bucio (Comandante Guardia Nacional)" )

sentimientos <- 
  db2 %>%
  separate(fecha, into = c("Año", "Mes", "Día"), sep = "-",
           remove = FALSE)

sentimientos_afinn <- 
  sentimientos %>%
  unnest_tokens(input = "Texto", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

sentimientos_afinn <-
  sentimientos_afinn 
  #filter(Palabra != "no") 

glimpse(sentimientos_afinn)

sentimientos_afinn_fecha <-
  sentimientos_afinn %>%
  group_by(id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Persona, fecha) %>%
  summarise(Media = mean(Puntuacion))

glimpse(sentimientos_afinn_fecha)

sentimientos_afinn_fecha %>%
  group_by(Persona) %>% 
  summarize(Promedio = mean(Media)) %>% 
  ggplot(aes(reorder(Persona, Promedio), Promedio))+
    geom_col()+
  coord_flip()+
  labs(title = 'Análisis de sentimientos de discurso',
       subtitle = 'Principales participantes',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.margin.x = unit(1, "lines"),
    axis.text = element_text(size = 16, family="Futura LT")
  )

sentimientos_afinn_fecha %>%
  ggplot(aes(fecha, Media)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = F, color="#333333")+
  geom_rug()+ 
  facet_wrap(~Persona, ncol=2)+
labs(title = 'Análisis de sentimientos de discurso',
                           subtitle = 'Principales participantes',
                           caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
                           x = "",
                           y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.margin.x = unit(1, "lines"),
    axis.text = element_text(size = 16, family="Futura LT")
  )

#Palabras más usadas por tono

palabras <- sentimientos_afinn %>% 
  filter(Persona == "AMLO") %>%
  filter(Palabra != "tiempo") %>% 
  filter(Puntuacion > 1) %>% 
  group_by(Puntuacion, Palabra) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total)) %>%
  ungroup() %>% 
  top_n(15)

palabras

palabras %>% 
  ggplot(aes(reorder(Palabra, total), total, fill=Puntuacion)) +
  geom_col() +
  coord_flip() + 
  labs(title = 'Palabras positivas más utilizadas',
       subtitle = 'AMLO',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  scale_fill_gradient(low = "#9e9cff", high = "#072abf")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 16, family="Futura LT")
  )
  
si_no <- sentimientos_afinn %>% 
  filter(Palabra == "sí" | Palabra == "no") %>% 
  group_by(Persona, Palabra) %>% 
  summarise(total = n())

si_no

si_no %>% 
  ggplot(aes(reorder(Persona, total), total, fill=Palabra))+
  geom_col(position = "fill")+
  coord_flip() + 
  labs(title = 'Palabras Sí y No',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/29",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "right",
    text = element_text(family="Futura LT"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.text = element_text(size = 16, family="Futura LT")
  )
