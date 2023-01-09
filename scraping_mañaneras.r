Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#devtools::install_github("Mikata-Project/ggthemr")

## Desabilitar notación científica.----
options(scipen = 999)
# Paquetes
library(pacman)
p_load(readxl, tidyverse, dplyr, cowplot, janitor, lmtest, ldatuning,
       sandwich, sjPlot, pander, pscl, haven, gridExtra, ggExtra, ggrepel,
       hexbin, janitor, mosaicData, scales, ggthemes, rtweet,
       lubridate, hms, tidytext, wordcloud2, tm, SnowballC, htmlTable, kableExtra,
       magick, magrittr, scales, textdata, syuzhet, visNetwork, stringi, topicmodels,
       ggthemr, viridis, forcats, rvest, writexl, quanteda, zoo)

#Aplicar estilo gráficas (opcional)
ggthemr('solarized')

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
  fecha_inicial <-  as_date('2023/01/06', format="%Y/%m/%d") #Fecha de última mañanera
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
                             TRUE ~ Personaje) 
         ) %>% 
  #Y arreglamos el orden de las columnas
  select(id, Persona, Texto, fecha)

#Y ya tenemos lista la base de datos con la que vamos a trabajar

# -------------------------------- ALGUNOS EJEMPLOS DE USO

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

participacion_amlo %>% 
  ggplot(aes(promedio_AMLO, total_palabras))+
  geom_point()+
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent)+
  labs(title = 'Lentitud de AMLO',
       subtitle = 'A mayor participación de AMLO, menos palabras se pronuncian\nen la mañanera',
       caption = "@elcerebrohabla - Mañaneras hasta 2022/08/20",
       x = "Porcentaje participación AMLO",
       y = "Número de palabras totales")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
  
# Monopolio AMLO

participacion_amlo %>% 
  ggplot(aes(fecha, promedio_AMLO))+
  geom_point()+
  geom_smooth(span = 0.8, method="loess", se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = '¿Qué tanto acapara AMLO las mañaneras?',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2022/08/20",
       x = "Fecha",
       y = "Porcentaje participación AMLO")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14)
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
  labs(title = '¿Quienes participan más?',
       subtitle = 'Palabras pronunciadas en mañaneras',
       caption = "@elcerebrohabla - Mañaneras hasta 2022/08/20",
       x = "",
       y = "")+
  coord_flip()+ 
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14)
  )
  
participaciones_fecha <- mañaneras %>% 
  filter((!str_detect(Persona, "PREGUNTA|INTERLOCUTOR|VOZ MUJER|VOZ HOMBRE"))) %>% 
  mutate(palabras = sapply(gregexpr("\\S+", Texto), length)) %>% 
  group_by(fecha, Persona) %>% 
  summarize(palabras = sum(palabras)) %>% 
  arrange(desc(palabras)) %>% 
  filter(Persona == "AMLO" | Persona == "López-Gatell" | 
           Persona == "Luis Crescencio (Sec. Defensa Nacional)" | 
           Persona == "Marcelo Ebrard" | Persona == "García Vilchis" |
           Persona == "Zoe Robledo (IMSS)" | 
           Persona == "Rafael Ojeda (Sec. Marina)" |
           Persona == "Rosa Icela Rodríguez (Sec Seguridad)" |
           Persona == "Ricardo Mejía (Sub Seguridad Pública)")

view(participaciones_fecha)

participaciones_fecha %>% 
  ggplot(aes(fecha, palabras))+
  geom_point()+ 
  geom_smooth(span = 0.8, method="loess", se = FALSE)+
  facet_wrap(~Persona, scales = "free")+
  labs(title = 'Palabras mencionadas por mañanera',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2022/08/20",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14))

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
  filter(Persona == "García Vilchis") %>%
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
  head(25)

bigramas

bigramas %>% 
  ggplot(aes(reorder(bigrama,n), n)) +
  geom_col() +
  coord_flip()+
  labs(title = 'Frases que García Vilchis\nmenciona más en la mañanera',
       subtitle = '',
       caption = "@elcerebrohabla - Mañaneras hasta 2022/08/20",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14))


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
INE <- c("el ine")
CIDE <- c("el cide")

#Personajes adversos

Loret <- c("loret")
Krauze <- c("krauze")
Dresser <- c("dresser")
Brozo <- c("brozo")
Aristegui <- c("aristegui")
Lorenzo <- c("lorenzo córdova")
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


#Políticos

Calderón <- c("calderón")
Fox <- c("fox")
Peña <- c("presidente peña", "peña nieto")
Salinas <- c("salinas de gortari", "carlos salinas")
Biden <- c("Biden")
Trump <- c("Trump")

list_data <- list(Aeropuerto, Tren_maya, Dos_Bocas, Neoliberalismo, Conservadores, Fifis, 
                  Mafia, Prian, Adversarios, Reforma, INE, CIDE, Loret, Krauze, Dresser, Brozo, Aristegui, Lorenzo,
                  Majluf, Schettino, Herzog, Patan, Zuckermann, Ferriz, Ramos, Castañeda, Camín, Alazraki, Calderón, Fox, Peña, Salinas,
                  Biden, Trump)
list_text <- c("Aeropuerto", "Tren_maya", "Dos_Bocas", "Neoliberalismo", "Conservadores",
               "Fifis", "Mafia del poder", "Prian", "Adversarios", "Reforma", "INE", "CIDE", "Loret de Mola", "Krauze",
               "Denise Dresser", "Brozo", "Aristegui", "Lorenzo Córdova", "Pablo Majluf", "Macario Schettino", "Silva Hérzog", "Julio Patán",
               "Leo Zuckermann", "Pedro Ferriz", "Jorge Ramos", "Jorge Castañeda", "Aguilar Camín", "Carlos Alazraki", "Felipe Calderón", "Vicente Fox", "Enrique Peña Nieto",
               "Carlos Salinas de Gortari", "Joe Biden", "Donald Trump")
i = 1

db1 <- mañaneras %>% 
filter(Persona == "AMLO") 


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
            INE = sum(INE),
            CIDE = sum(CIDE),
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
            `Felipe Calderón` = sum(`Felipe Calderón`),
            `Vicente Fox` = sum(`Vicente Fox`),
            `Enrique Peña Nieto` = sum(`Enrique Peña Nieto`),
            `Carlos Salinas de Gortari` = sum(`Carlos Salinas de Gortari`),
            `Joe Biden` = sum(`Joe Biden`),
            `Donald Trump` = sum(`Donald Trump`)
            
            
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
  gather(concepto, número, `Loret de Mola`:`Carlos Alazraki`) %>% 
  select(fecha, concepto, número) %>% 
  mutate(mes = month(fecha), año =  year(fecha)) %>%
  mutate(fecha_nueva = as.yearmon(paste(mes, año), "%m %Y")) %>% 
  #mutate(fecha_nueva = as.yearmon(paste(año), "%Y")) %>%
  filter(fecha_nueva < '2023-01-01') %>% 
  mutate(fecha_nueva = as.Date(fecha_nueva)) %>% 
  group_by(fecha_nueva) %>% 
  summarize(número = sum(número, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(fecha_nueva, número, label=número)) +
  geom_col() +
  geom_label() +
  #geom_smooth(se = FALSE, span = 0.5)+
  #facet_wrap(~concepto, ncol=1)+
  #scale_x_date(date_labels = "%m %Y", date_breaks= 'month')+
  #scale_y_continuous(trans='log2')+
  labs(title = 'Menciones de periodistas\nadversarios',
       subtitle = 'En las mañaneras',
       caption = "@elcerebrohabla - Mañaneras hasta 2023/01/06",
       x = "",
       y = "")+
  coord_flip()+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14))

db_final %>% 
  gather(concepto, número, `Loret de Mola`:`Carlos Alazraki`) %>% 
  select(concepto, número) %>% 
  group_by(concepto) %>% 
  summarize(número = sum(número, na.rm=T)) %>% 
  ggplot(aes(reorder(concepto, número), número))+
  geom_col()+
  coord_flip()+
  labs(title = 'Menciones a periodistas adversarios\nen las mañaneras',
       subtitle = 'Obras más importantes del sexenio',
       caption = "@elcerebrohabla - Mañaneras hasta 2022/08/20",
       x = "",
       y = "")+
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=12),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14))

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

ml
