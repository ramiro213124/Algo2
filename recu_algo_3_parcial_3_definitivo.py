# -*- coding: utf-8 -*-
"""Recu algo 3 parcial 3 definitivo.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1fJSTPHxU4IYcE_WWSpKmM3BrLBOWTyGo
"""

import numpy as np
## Algoritmo O(n) para maximizar alfajores. 


def maxAlfajoresAux(A,i,j):
   
  if i+1==j:
    return {'mejor': A[i],
          'sinBordeIzq': 0,
          'sinBordeDer': 0,
          'sinBordes': 0}

  if i+2==j:
    return {'mejor': max(A[i],A[i+1]),
          'sinBordeIzq': A[i+1],
          'sinBordeDer': A[i],
          'sinBordes': 0}

  else:
    ## Divido
    m = int((i+j)/2)
    ladoIzq = maxAlfajoresAux(A, i, m)
    ladoDer = maxAlfajoresAux(A, m, j)

    ## Conquisto

    ## Declaro la variable mejor
    mejor = max(ladoIzq['mejor'] + ladoDer['sinBordeIzq'],
                ladoDer['mejor'] + ladoIzq['sinBordeDer'])
    
    ## Me ocupo de la variable sinBordeIzq           
    sinBordeIzq = max(ladoIzq['sinBordeIzq'] + ladoDer['sinBordeIzq'],
                      ladoIzq['sinBordes'] + ladoDer['mejor'])
    
    ## Me ocupo de la variable sinBordeDer
    sinBordeDer = max(ladoDer['sinBordeDer'] + ladoIzq['sinBordeDer'],
                      ladoDer['sinBordes'] + ladoIzq['mejor'])
    
    ## Me ocupo de la variable sinBordes
    sinBordes = max(ladoIzq['sinBordes'] + ladoDer['sinBordeDer'],
                      ladoDer['sinBordes'] + ladoIzq['sinBordeIzq'])

  return {'mejor': mejor,
          'sinBordeIzq': sinBordeIzq,
          'sinBordeDer': sinBordeDer,
          'sinBordes': sinBordes}

def maxAlfajores(A):
    res = maxAlfajoresAux(A, 0, len(A))['mejor']
    return res