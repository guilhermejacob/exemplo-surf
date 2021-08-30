# Estimação de Fluxo usando a PNADC

# carrega libraries
library(data.table)
library(fst)
library(survey)
library(surf)

# opções
options( stringsAsFactors = FALSE )

# opções do survey
options( survey.lonely.psu = "adjust" )

# define colunas
these.cols <- c( "ano" , "trimestre" , "( upa || v1008 || v1014 ) AS hhid" , "v2003" , "vd4001" , "vd4009" , "v2009" )
design.cols <- c( "ano" , "trimestre" , "( upa || v1008 || v1014 ) AS hhid" , "v1016" , "estrato" , 'upa' , "v1027" , "v1028" , "posest" , "v1029" )

# lê base de dados
base.df <- read_fst( "/home/guilherme/GitHub/exemplo-surf/dados/pnadc-202003-202004-202101.fst" , as.data.table = TRUE )

# testa fluxo NR/NR
base.df[ is.na( v2009_1 ) & is.na( v2009_2 ) ]

# mantém apenas entrevistas 1:3
base.df <- base.df[ v1016 %in% 1:3 ]

### cria objeto de desenho

# cria objeto de totais
pop.tots <- base.df[ !duplicated( base.df[,posest] ) , .( posest , Freq = v1029 ) ]
pop.tots <- data.frame( pop.tots , stringsAsFactors = FALSE )

# ajusta propensão a responder
base.df[ , peso.ajust := v1027 / resp.propensity ]

# cria objeto de desenho
# pnadc.design <- svydesign( ids = ~upa , strata = ~estrato , weights = ~v1027 , data = base.df , nest = TRUE ) # peso desenho
pnadc.design <- svydesign( ids = ~upa , strata = ~estrato , weights = ~peso.ajust , data = base.df , nest = TRUE )   # peso com ajuste de não-resposta

# aplica calibração
pnadc.design <- postStratify( pnadc.design , ~posest , population = pop.tots )

### cria variáveis para análise

# cria variável
pnadc.design <- update( pnadc.design , status_1 = ifelse( vd4001_1 == 2 , 3 , vd4002_1 ) )
pnadc.design <- update( pnadc.design , status_2 = ifelse( vd4001_2 == 2 , 3 , vd4002_2 ) )
pnadc.design <- update( pnadc.design , status_3 = ifelse( vd4001_3 == 2 , 3 , vd4002_3 ) )

# adiciona rótulos às variáveis criadas
pnadc.design <- update( pnadc.design , status_1 = factor( status_1 , labels = c( "ocupado" , "desocupado" , "fora da força" ) ) )
pnadc.design <- update( pnadc.design , status_2 = factor( status_2 , labels = c( "ocupado" , "desocupado" , "fora da força" ) ) )
pnadc.design <- update( pnadc.design , status_3 = factor( status_3 , labels = c( "ocupado" , "desocupado" , "fora da força" ) ) )

### estimação

# filtra osbervações
flow.des <- subset( pnadc.design , pia )

# tabulação ingênua
svytable( ~status_2+status_3 , flow.des )

# tabulação ingênua com observações ausentes
svytable( ~status_2+status_3 , flow.des , addNA = TRUE )

# ajusta modelo
modA <- svyflow( ~status_2+status_3 , flow.des , model = "A" , verbose = TRUE )
modB <- svyflow( ~status_2+status_3 , flow.des , model = "B" , verbose = TRUE )
modC <- svyflow( ~status_2+status_3 , flow.des , model = "C" , verbose = TRUE )
modD <- svyflow( ~status_2+status_3 , flow.des , model = "D" , verbose = TRUE )

# # ajusta modelo
# modA.visita <- svyflow( v1016~status_2+status_3 , flow.des , model = "A" , verbose = TRUE )
# modB.visita <- svyflow( v1016~status_2+status_3 , flow.des , model = "B" , verbose = TRUE )
# modC.visita <- svyflow( v1016~status_2+status_3 , flow.des , model = "C" , verbose = TRUE )
# modD.visita <- svyflow( v1016~status_2+status_3 , flow.des , model = "D" , verbose = TRUE )
