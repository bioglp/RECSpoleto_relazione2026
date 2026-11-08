# Analisi della popolazione residente per UF

library(tidyverse)
library(knitr)
library()


## POPOLAZIONE ----
pop <- read.csv('input/popolazione/mapPopolazione.csv')

glimpse(pop)

pop |> 
  group_by(UNITA_FUNZ) |> 
  summarise(POP=sum(popolazione_POP, na.rm = T), FAM=sum(popolazione_FAM, na.rm=T), AREA=sum(Shape_Area/1000000)) |> 
  mutate(`Pop%`=POP/sum(POP)*100, `Fam%`=FAM/sum(FAM)*100, Dens=POP/AREA) |>
  select(UNITA_FUNZ,POP,`Pop%`,FAM,`Fam%`, Dens) |> 
  kable(digits = 2)


## VERDE URBANO ----
verde <- read.csv('input/verdeUrbano/verdeUrbano2026.csv')
frazioni <- read.csv('input/verdeUrbano/frazioni.csv')
glimpse(verde)

verde |> 
  filter(urbano==1) |> 
  group_by(NOME_2,Verde) |> 
  summarise(SUP=sum(superficie/10000,na.rm = T)) |> 
  pivot_wider(names_from = 'Verde', values_from = SUP, values_fill = 0) |> 
  mutate(TOT = rowSums(across(where(is.numeric)))) |> 
  rename(NOME=NOME_2) |> 
  left_join(frazioni) |> 
  mutate('Sup%'=TOT/(Shape_Area/10000)*100) |> 
  select(NOME:TOT,'Sup%') |> 
  kable(digits = 2)

