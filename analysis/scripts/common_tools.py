#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan 23 15:02:22 2021

@author: jruiz
"""


import numpy as np


def get_bias_correction( Pred , Coef ) :
    
    #Pred es la matriz de predictores (NPred,NObs)
    #Coef son los coeficientes de correccion del bias (NPred)
    #Las dimensiones entre ambos deben ser consistentes. Pero por el momento
    #no estoy chequeando eso.
    [NPred,NObs] = Pred.shape
    
    Bias = np.dot( Pred.T , Coef )
    
    return Bias


def estimate_coef( x_obs , x_mod , Pred , Coef_Prior , RDiag , BDiag , MinObs=100 ) :
    #x_obs son las observaciones
    #x_mod es el modelo llevado al espacio de las observaciones
    #Pred es la matriz de predictores (NPred, NObs)
    #Coef_Prior es el valor a prior de los coeficientes
    #RDiag es la diagnoal de la matriz del error de las obs (que asumimos diagnoal)
    #BDiag es la diagonal de la matriz del error de los coeficientes (que asumimos diagonal tambien.)
    #Miyoshi et al 2010 
    #https://journals.ametsoc.org/view/journals/mwre/138/7/2010mwr3209.1.xml
    
    [NCoef,NObs] = Pred.shape 
  

    if NObs < MinObs :
        RDiag = np.array(RDiag) / MinObs
    else             :
        coef = NObs / ( np.log10( NObs / MinObs ) +  1.0 )
        RDiag = np.array(RDiag) / coef
    
    
    bias = get_bias_correction( Pred , Coef_Prior )

    d = (x_obs - x_mod - bias)[:,0]

    PRinv = np.dot( Pred , np.diag(1.0/RDiag) )

    PRinvPt= np.dot( Pred , np.dot( np.diag(1.0/RDiag) , Pred.T ) )

    A= np.linalg.inv( np.diag(1.0/np.array(BDiag)) + PRinvPt )

    Coef_Est = np.dot( A , np.dot( PRinv , d ) )

    return Coef_Est

