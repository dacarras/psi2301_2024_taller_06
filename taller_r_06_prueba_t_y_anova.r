#------------------------------------------------
# cargar datos
#------------------------------------------------

#--------------------------------------
# cargar dplyr
#--------------------------------------

library(dplyr)

#--------------------------------------
# datos Vik (2014, p83)
#--------------------------------------

table_7_1 <- read.table(text = "
subject   score    group      
      1       3    'No Dx'    
      2       5    'No Dx'    
      3       6    'No Dx'    
      4       8    'No Dx'    
      5       9    'No Dx'    
      6      11    'No Dx'    
      7      19    'Depressed'
      8      15    'Depressed'
      9      16    'Depressed'
     10      16    'Depressed'
     11      19    'Depressed'
     12      17    'Depressed'
     13       3    'No Dx'    
     14       5    'No Dx'    
     15       4    'No Dx'    
     16       6    'No Dx'    
     17       8    'No Dx'    
     18      10    'No Dx'    
     19      24    'Depressed'
     20      24    'Depressed'
     21      22    'Depressed'
     22      23    'Depressed'
     23      19    'Depressed'
     24      20    'Depressed'
", header = TRUE, stringsAsFactors = FALSE)

# mostrar datos
knitr::kable(table_7_1, digits = 2)

#--------------------------------------
# recodificaciones
#--------------------------------------

table_7_1 <- table_7_1 %>%
mutate(deviation = case_when(
group == 'Depressed' ~  1,
group == 'No Dx'     ~ -1
)) %>%
mutate(dummy = case_when(
group == 'Depressed' ~ 1,
group == 'No Dx' ~ 0
))

# mostrar datos
knitr::kable(table_7_1, digits = 2)

#------------------------------------------------
# Prueba t
#------------------------------------------------

# prueba t que genera resultados similares una regresion
t.test(score ~ group, # formula
	var.equal = TRUE, # argumento sobre varianzas
	data = table_7_1  # datos analizados
	) 


t.test(score ~ group, # formula
#	var.equal = TRUE, # argumento sobre varianzas
	data = table_7_1  # datos analizados
	) 


#------------------------------------------------
# Prueba t, con library(dplyr)
#------------------------------------------------

library(infer)

d_hat <- table_7_1 %>% 
  specify(score ~ group) %>%
  calculate(stat = "t", 
  	order = c("No Dx", "Depressed")
  	)

null_dist <- table_7_1 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(
  	stat = "t",
  	order = c("No Dx", "Depressed")
  	)

visualize(null_dist) +
  shade_p_value(
  	obs_stat = d_hat,
  	direction = "two-sided"
  	)



#------------------------------------------------
# Prueba t de una sola muestra
#------------------------------------------------

library(infer)

# media de la poblacion
mean_pop <- 19 # media cercana al grupo Depressed

# media del grupo
x_bar <- table_7_1 %>%
         dplyr::filter(group == 'Depressed') %>%
         observe(response = score, stat = "mean")


# generacion de distribución probabilistica
# expresada por la hipotesis nula
null_dist <- table_7_1 %>%
  dplyr::filter(group == 'Depressed') %>%
  specify(response = score) %>%
  hypothesize(null = "point", mu = mean_pop) %>%
  generate(reps = 1000) %>%
  calculate(stat = "mean")

# visualización de el valor p
visualize(null_dist) +
shade_p_value(
	obs_stat = x_bar, 
	direction = "two-sided"
	)


null_dist %>%
get_p_value(
	obs_stat = x_bar, 
	direction = "two-sided"
	)


#------------------------------------------------
# ANOVA
#------------------------------------------------

# ANOVA
aov(score ~ group, data = table_7_1) %>%
summary()

# descriptivos
table_7_1 %>%
group_by(group) %>%
summarize(
mean = mean(score, na.rm = TRUE),
var  = var(score, na.rm = TRUE)
) %>%
knitr::kable(., digits = 2)

# Nota: infer no tiene habilitada la función
#       de anova y F, para escenarios de
#       variables categoricas de solo dos categorias.

#------------------------------------------------
# regresion
#------------------------------------------------

lm(score ~ dummy, data = table_7_1) %>%
summary()


lm(score ~ dummy, data = table_7_1) %>%
anova()


