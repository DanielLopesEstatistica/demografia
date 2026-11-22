#################################
#### Medidas de Mortalidade #####
#################################

#Taxa bruta de mortalidade - Acre 1980 e 1991.			
#Populacao vivendo em areas urbanas/Populacao Total

## Ex. Acre TBM 1980 e 1991.	
#Ano	obitos	Populacao	 
#1980	  1424	   301276	
#1991	  1938	   417718	

TBM1980=(1424/301276)*1000
TBM1991=(1938/417718)*1000

TBM1980;TBM1991

### Trabalhando com medidas especificas ###
## Lendo os dados ###
## Cuidado com fontes e titulos separados
pop=read.csv('A194442143_106_180_97.csv',sep=';', skip=4, nrows=33, header=F) ## retira os titulos e as fontes no final do arquivo

colnames(pop)=c('Idade','Masculino','Feminino','Total') 

## Vendo o dado ###
View(pop)

### Gerando grupos tradicionais 
#0 a 1 ano,1 a 4 anos,5 a 9 anos,10 a 14 anos,15 a 19 anos,20 a 24 anos,
#25 a 29 anos,30 a 34 anos,35 a 39 anos,40 a 44 anos,
#45 a 49 anos,50 a 54 anos,55 a 59 anos,60 a 64 anos,
#65 a 69 anos,70 a 74 anos,75 a 79 anos,80 anos e mais

## Trabalhando a informacao de idade ###
rownames(pop)=pop$Idade
pop=pop[,-1]

### Criando grupos de idade para agrupar as idades simples
grupo_idade=cut(rep(0:32),breaks=c(0,0.99,4,9,14,19,20,21,22,
                                   23,24,25,26,27,28,29,30,31,32),include.lowest = T)

pop.n=aggregate(pop,list(Idade=grupo_idade),sum)

### Nova informacao de idade
pop.n$Idade=c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)

View(pop.n)

## Dados de obitos ###
morte=read.csv('Mortes_Acre_1980.csv',sep=',',skip='2',header=F)

colnames(morte)=c('Idade','Masculino','Feminino','Total') 

View(morte)

### Criando grupos de idade para idades abaixo de 1 ano
morte=morte[,-1]

idade_morte=cut(rep(0:19),breaks=c(0,2,3,4,5,6,7,8,9,
                                   10,11,12,13,14,15,16,17,18,19),include.lowest = T)

morte.n=aggregate(morte,list(Idade=idade_morte),sum)

View(morte.n)

morte.n$Idade=pop.n$Idade

### TEM - Taxas especificas de mortalidade
TEM=morte.n[,-1]/pop.n[,-1]

TEM.n=log10(TEM)

## Grafico com as taxas especificas das duas fontes
limy=range(TEM.n)

windows()
plot(c(0,1,seq(5,80,5)),TEM.n$Masculino, ylim=limy, type='l', xlab='Idades', ylab='TEM',col='black', lwd=3, main='Distribuicao de Mortalidade entre os sexos.Acre, 1980')
lines(c(0,1,seq(5,80,5)),TEM.n$Feminino, col='red',lwd=3)
legend('topleft', c('Morte homens','Mortes mulheres'), col=c('black','red'), lwd=c(2,2))

###################################################
### Taxa de mortalidade infantil e na infancia ####
###################################################

#TMI= (Mortes entre 0 e 1/nascidos vivos)*1000

## Acre 1980
TMI80=morte.n[1,4]/pop.n[1,4]
TMI80*1000

#Para outros anos
# Ano	 obitos <1 	Nascimentos
#1994	       462        12127
#2004	       341	      16584

TMI94=462/12127
TMI04=341/16584

TMI94*1000;TMI04*1000

## Taxa de mortalidade na infancia
#TMI5= (Mortes entre 0 e 5/nascidos vivos)*1000

TMI5.80=sum(morte.n[1:2,4])/sum(pop.n[1,4])
TMI5.80*1000

## Outro anos
#Para outros anos
# Ano	 obitos <1 	Nascimentos
#1994	       540        12127
#2004	       410	      16584

TMI5.94=540/12127
TMI5.04=410/16584

TMI5.94*1000;TMI5.04*1000

################################################
#### Mortalidade proporcional de uma causa #####
################################################

#MP=Obitos de uma causa X/Total de obitos

#ex 
#ACRE	obitos por causas externas	Total de obitos
#1980	                       138	           1424
#1991	                       273	           1938

MPex.80=138/1424
MPex.91=273/1938

MPex.80*100;MPex.91*100

## Taxa de mortalidade por causa###
##obitos por uma casa X/total populacao em risco

Tx.ex80=(138/sum(pop.n[,4]))*100000
Tx.ex80

Tx.ex91=(273/417718)*100000
Tx.ex91



