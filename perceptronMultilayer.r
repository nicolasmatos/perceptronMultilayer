gerarTreinoTeste <- function(classe1, classe2, classe3, classe4, classe5, classe6) {
  #Embaralhando os datasets das classes
  classe1 <- classe1[sample(1:nrow(classe1)), ]
  classe2 <- classe2[sample(1:nrow(classe2)), ]
  classe3 <- classe3[sample(1:nrow(classe3)), ]
  classe4 <- classe4[sample(1:nrow(classe4)), ]
  classe5 <- classe5[sample(1:nrow(classe5)), ]
  classe6 <- classe6[sample(1:nrow(classe6)), ]
  
  library(dplyr)
  dataTreinoClasse1<-sample_frac(classe1, 0.80)
  dataTesteClasse1<-setdiff(classe1, dataTreinoClasse1)
  
  dataTreinoClasse2<-sample_frac(classe2, 0.80)
  dataTesteClasse2<-setdiff(classe2, dataTreinoClasse2)
  
  dataTreinoClasse3<-sample_frac(classe3, 0.80)
  dataTesteClasse3<-setdiff(classe3, dataTreinoClasse3)
  
  dataTreinoClasse4<-sample_frac(classe4, 0.80)
  dataTesteClasse4<-setdiff(classe4, dataTreinoClasse4)
  
  dataTreinoClasse5<-sample_frac(classe5, 0.80)
  dataTesteClasse5<-setdiff(classe5, dataTreinoClasse5)
  
  dataTreinoClasse6<-sample_frac(classe6, 0.80)
  dataTesteClasse6<-setdiff(classe6, dataTreinoClasse6)
  
  dataTreino <- rbind(dataTreinoClasse1, dataTreinoClasse2, dataTreinoClasse3, dataTreinoClasse4, dataTreinoClasse5, dataTreinoClasse6)
  dataTeste <- rbind(dataTesteClasse1, dataTesteClasse2, dataTesteClasse3, dataTesteClasse4, dataTesteClasse5, dataTesteClasse6)
  
  
  dataTreino <- dataTreino[sample(1:nrow(dataTreino)), ]
  dataTeste <- dataTeste[sample(1:nrow(dataTeste)), ]
  
  r = list()
  r$dataTreino = dataTreino
  r$dataTeste = dataTeste
  
  return(r)
}

classeDesejada <- function(classe) {
  desejado<-c()
  if(classe == 1) {
    desejado<-c(1, 0, 0, 0, 0, 0)
  }
  else if(classe == 2) {
    desejado<-c(0, 1, 0, 0, 0, 0)
  }
  else if(classe == 3) {
    desejado<-c(0, 0, 1, 0, 0, 0)
  }
  else if(classe == 4) {
    desejado<-c(0, 0, 0, 1, 0, 0)
  }
  else if(classe == 5) {
    desejado<-c(0, 0, 0, 0, 1, 0)
  }
  else {
    desejado<-c(0, 0, 0, 0, 0, 1)
  }
  
  return(desejado)
}

aprendizagem <- function(wPesos, mPesos, xLinhaTreino, yEntrada, vOsL, vECs, pA) {
  wPesosNovo<-matrix((34*19), nrow = 34, ncol = 19) 
  for (i in 1:19) {
    for (j in 1:34) {
      wPesosNovo[j,i] = wPesos[j,i] + (pA * 1 * (yEntrada[,i] * (1 - yEntrada[,i])) * xLinhaTreino[,j])
    }
  }
  
  mPesosNovo<-matrix((20*6), nrow = 20, ncol = 6) 
  for (i in 1:6) {
    for (j in 1:20) {
      mPesosNovo[j,i] = mPesos[j,i] + (pA * vECs[i] * (vOsL[i] * (1 - vOsL[i])) * yEntrada[,j])
    }
  }
  
  r = list()
  r$wPesosNovo = wPesosNovo
  r$mPesosNovo = mPesosNovo
  
  return(r)
}

processaPerceptron <- function(classe1, classe2, classe3, classe4, classe5, classe6, n, pA) {
  txAcertos<-c()
  txAcertosUm<-c()
  txAcertosDois<-c()
  txAcertosTres<-c()
  txAcertosQuatro<-c()
  txAcertosCinco<-c()
  txAcertosSeis<-c()
  
  #Criando uma matriz aleatória de pesos da camada intermediária
  #OBS: Os pesos da linha 34 são os bias
  wPesos<-matrix(runif(34*19), nrow = 34, ncol = 19) 
  
  #Criando uma matriz aleatória de pesos da camada de saida
  #OBS: Os pesos da linha 7 são os bias
  mPesos<-matrix(runif(20*6), nrow = 20, ncol = 6) 
  
  numeroRodadas = 0
  #Laço para rodar n vezes
  for (k in 1:n) {
    #Função para gerar dataset de treino e teste
    treinoTeste = gerarTreinoTeste(classe1, classe2, classe3, classe4, classe5, classe6)
    dataTreino = treinoTeste$dataTreino
    dataTeste = treinoTeste$dataTeste
    
    #r = list()
    #r$qtd1 = nrow(classe1)
    #r$qtd2 = nrow(classe2)
    #r$qtd3 = nrow(classe3)
    #r$qtd4 = nrow(classe4)
    #r$qtd5 = nrow(classe5)
    #r$qtd6 = nrow(classe6)
    #r$dataTreino = nrow(dataTreino)
    #r$dataTeste = nrow(dataTeste)
    
    #return(r)
    
    #Criando a nova matriz de pesos camada intermediária
    wPesosNovo<-matrix((34*19), nrow = 34, ncol = 19) 
    
    #Criando a nova matriz de pesos camada de saida
    mPesosNovo<-matrix((20*6), nrow = 20, ncol = 6) 
    
    for (i in 1: (nrow(dataTreino))) {
      #Recebe a linha atual do conjunto de treino
      linhaTreino = dataTreino[i,]
      
      #Montando o vetor desejado
      vD = classeDesejada(linhaTreino[34])
      
      #Transformando a linha em uma matriz
      vX<-c()
      for (j in 1:33) {
        vX[j] = as.numeric(gsub(",", ".", linhaTreino[[j]]))
      }
      
      #Adicionando o valor para representar o x0
      vX[34] = -1
      xLinhaTreino = matrix(vX, 1, 34)
      
      #Multiplicando a linha de entrada da camada intermediária pela matriz de pesos dos neuronios da camada intermediária
      u = xLinhaTreino %*% wPesos
      
      #Montando o y pela formula da sigmóide logística
      vYsL<-c()
      for (j in 1:19) {
        vYsL[j] = 1 / (1 + exp(-1 * u[,j]))
      }
      
      #Adicionando o valor para representar o x0
      vYsL[20] = -1
      yEntrada = matrix(vYsL, 1, 20)
      
      #Multiplicando a linha de entrada da camada de saida pela matriz de pesos dos neuronios da camada de saida
      a = yEntrada %*% mPesos
      
      #Montando o 'o' pela formula da sigmóide logística
      vOsL<-c()
      vO<-c()
      for(j in 1:6) {
        sL = 1 / (1 + exp(-1 * a[,j]))
        vOsL[j] = sL
        if (sL > 0.5) {
          vO[j] = 1
        }
        else {
          vO[j] = 0
        }
      }
      
      #Gerando o vetor de erro da camada de saida(Vetor desejado - Vetor obtido)
      vECs = vD - vO
      
      #Aplicado a regra de aprendizagem para atualização dos pesos
      pesosNovos = aprendizagem(wPesos, mPesos, xLinhaTreino, yEntrada, vOsL, vECs, pA)
      wPesosNovo = pesosNovos$wPesosNovo
      mPesosNovo = pesosNovos$mPesosNovo
      
      #r = list()
      #r$xLinhaTreino = xLinhaTreino
      #r$wPesos = wPesos
      #r$wPesosNovo = wPesosNovo
      #r$u = u
      #r$vYsL = vYsL
      #r$yEntrada = yEntrada
      #r$mPesos = mPesos
      #r$mPesosNovo = mPesosNovo
      #r$a = a
      #r$vO = vO
      #r$d = vD
      #r$e = vECs
      
      #return(r)
    }
    
    #Variáveis para controlar os acertos do algoritmo
    qntUm = 0
    qntDois = 0
    qntTres = 0
    qntQuatro = 0
    qntCinco = 0
    qntSeis = 0
    qntAcertosUm = 0
    qntAcertosDois = 0
    qntAcertosTres = 0
    qntAcertosQuatro = 0
    qntAcertosCinco = 0
    qntAcertosSeis = 0
    
    #Laço para percorrer todas as linha do dataframe de teste
    for (i in 1: (nrow(dataTeste))) {
      #Recebe a linha atual do conjunto de treino
      linhaTeste = dataTeste[i,]
      
      #Montando o vetor desejado
      vD = classeDesejada(linhaTeste[34])
      
      #Guardando o número da classe que está sendo testada
      nClasse = linhaTeste[34]
      
      #Transformando a linha em uma matriz
      vX<-c()
      for (j in 1:33) {
        vX[j] = as.numeric(gsub(",", ".", linhaTeste[[j]]))
      }
      #Adicionando o valor para representar o x0
      vX[34] = -1
      xLinhaTeste = matrix(vX, 1, 34)
      
      #Multiplicando a linha de entrada pela nova matriz de pesos
      u = xLinhaTeste %*% wPesosNovo
      
      #Montando o y pela formula da sigmóide logística
      vYsL<-c()
      for (j in 1:19) {
        vYsL[j] = 1 / (1 + exp(-1 * u[,j]))
      }
      
      #Adicionando o valor para representar o x0
      vYsL[20] = -1
      yEntrada = matrix(vYsL, 1, 20)
      
      #Multiplicando a linha de entrada da camada de saida pela matriz de pesos dos neuronios da camada de saida
      a = yEntrada %*% mPesos
      
      #Montando o 'o' pela formula da sigmóide logística
      vOsL<-c()
      vO<-c()
      for(j in 1:6) {
        sL = 1 / (1 + exp(-1 * a[,j]))
        vOsL[j] = sL
        if (sL > 0.5) {
          vO[j] = 1
        }
        else {
          vO[j] = 0
        }
      }
      
      #Gerando o vetor de erro da camada de saida(Vetor desejado - Vetor obtido)
      vECs = vD - vO
      
      #Verificando quantas chamadas teve de cada classe
      if (nClasse == 1) {
        qntUm = qntUm + 1
      }
      else if (nClasse == 2) {
        qntDois = qntDois + 1
      }
      else if (nClasse == 3) {
        qntTres = qntTres + 1
      }
      else if (nClasse == 4) {
        qntQuatro = qntQuatro + 1
      }
      else if (nClasse == 5) {
        qntCinco = qntCinco + 1
      }
      else {
        qntSeis = qntSeis + 1
      }
      
      #Verificando se o algoritmo acertou na classificação
      if (vECs[1] == 0 && vECs[2] == 0 && vECs[3] == 0 && vECs[4] == 0 && vECs[5] == 0 && vECs[6] == 0) {
        if (nClasse == 1) {
          qntAcertosUm = qntAcertosUm + 1
        }
        else if (nClasse == 2) {
          qntAcertosDois = qntAcertosDois + 1
        }
        else if (nClasse == 3) {
          qntAcertosTres = qntAcertosTres + 1
        }
        else if (nClasse == 4) {
          qntAcertosQuatro = qntAcertosQuatro + 1
        }
        else if (nClasse == 5) {
          qntAcertosCinco = qntAcertosCinco + 1
        }
        else {
          qntAcertosSeis = qntAcertosSeis + 1
        }
      }
    }
    
    #Calculando a taxa de acertos (Numero de acertos total / Quantidade de elementos testados)
    txAcerto = (qntAcertosUm + qntAcertosDois + qntAcertosTres + qntAcertosQuatro + qntAcertosCinco + qntAcertosSeis) / (qntUm + qntDois + qntTres + qntQuatro + qntCinco + qntSeis)
    
    #Calculando a taxa de acertos para classe um (Numero de acertos um / Quantidade de elementos testados para um)
    txAcertoUm = qntAcertosUm / qntUm
    
    #Calculando a taxa de acertos para classe dois (Numero de acertos dois / Quantidade de elementos testados para dois)
    txAcertoDois = qntAcertosDois / qntDois
    
    #Calculando a taxa de acertos para classe tres (Numero de acertos tres / Quantidade de elementos testados para tres)
    txAcertoTres = qntAcertosTres / qntTres
    
    #Calculando a taxa de acertos para classe Quatro (Numero de acertos Quatro / Quantidade de elementos testados para Quatro)
    txAcertoQuatro = qntAcertosQuatro / qntQuatro
    
    #Calculando a taxa de acertos para classe Cinco (Numero de acertos Cinco / Quantidade de elementos testados para Cinco)
    txAcertoCinco = qntAcertosCinco / qntCinco
    
    #Calculando a taxa de acertos para classe Seis (Numero de acertos Seis / Quantidade de elementos testados para Seis)
    txAcertoSeis = qntAcertosSeis / qntSeis
    
    #r = list()
    #r$txAcerto = txAcerto
    #r$txAcertoUm = txAcertoUm
    #r$txAcertoDois = txAcertoDois
    #r$txAcertoTres = txAcertoTres
    #r$txAcertoQuatro = txAcertoQuatro
    #r$txAcertoCinco = txAcertoCinco
    #r$txAcertoSeis = txAcertoSeis
    #r$qntUm = qntUm
    #r$qntDois = qntDois
    #r$qntTres = qntTres
    #r$qntQuatro = qntQuatro
    #r$qntCinco = qntCinco
    #r$qntSeis = qntSeis
    #r$qntAcertosUm = qntAcertosUm
    #r$qntAcertosDois = qntAcertosDois
    #r$qntAcertosTres = qntAcertosTres
    #r$qntAcertosQuatro = qntAcertosQuatro
    #r$qntAcertosCinco = qntAcertosCinco
    #r$qntAcertosSeis = qntAcertosSeis
    
    #return(r)
    
    txAcertos[k]<-txAcerto
    txAcertosUm[k]<-txAcertoUm
    txAcertosDois[k]<-txAcertoDois
    txAcertosTres[k]<-txAcertoTres
    txAcertosQuatro[k]<-txAcertoQuatro
    txAcertosCinco[k]<-txAcertoCinco
    txAcertosSeis[k]<-txAcertoSeis
    
    numeroRodadas = k
    
    #Critério de para: Se a nova matriz de peso não mudar para o treinamento
    #if(all(wPesos == wPesosNovo)) {
    if(FALSE) {
      break
    }
    else {
      wPesos = wPesosNovo
      mPesos = mPesosNovo
    }
  }
  
  resultado = list()
  
  resultado$txAcertos = txAcertos
  resultado$txErros = 1 - txAcertos
  resultado$grafico1 = plot(resultado$txErros~c(1:numeroRodadas), type="l", main=paste("Gráfico com as taxas de acerto e erro do algoritmo Perceptron Simples para", numeroRodadas, "rodadas", sep=" "), xlab="Número de rodadas", ylab="Taxa de erro/acerto %", col="red", ylim=c(0,1))
  resultado$grafico2 = lines(c(1:numeroRodadas), resultado$txAcertos,col="blue")
  #resultado$grafico3 = legend(1, 1.3, c("Taxa acerto","Taxa erro"), col =c("blue","red"), pch=rep(10,2))
  resultado$txErrosMin = min(resultado$txErros)  
  resultado$txErrosMax = max(resultado$txErros)
  resultado$txErrosMed = median(resultado$txErros)
  resultado$txAcertosMin = min(txAcertos)  
  resultado$txAcertosMax = max(txAcertos)
  resultado$txAcertosMed = median(txAcertos)
  resultado$txErrosUmMed = median(resultado$txErros)
  resultado$txErrosDoisMed = median(resultado$txErros)
  resultado$txErrosTresMed = median(resultado$txErros)
  resultado$txErrosQuatroMed = median(resultado$txErros)
  resultado$txErrosCincoMed = median(resultado$txErros)
  resultado$txErrosSeisMed = median(resultado$txErros)
  
  return (resultado)
}