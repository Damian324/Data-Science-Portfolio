# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 13:33:53 2020

@author: Usuario
"""

import pandas as pd
import numpy as np
from mlxtend.frequent_patterns import apriori
from mlxtend.frequent_patterns import association_rules

#import data
#wrangle data(group by transaction and barcode, unstack data and convert numers to 0 and 1s)
#run apriori algo
#run asociation algo
#recomendations

path_excel = r'C:\Users\Usuario\Desktop\Ventas\tickets\rojo\transacciones\'
transactions = pd.read_excel(path_excel+'transacciones.xlsx')

tran_non0 = transactions[transactions['Codigo'] != 0]

(tran_non0.groupby(['Transaccion','Codigo'])['Cant']
.sum().unstack().reset_index().fillna(0)
.set_index('Transaccion'))

tran_matrix = (tran_non0.groupby(['Transaccion','Codigo'])['Cant']
                .sum().unstack().reset_index().fillna(0)
                .set_index('Transaccion'))

    
def encode_matrix(x):
    if x >= 1:
        return 1
    else:
        return 0


my_basket = tran_matrix.applymap(encode_matrix)

#train model
#generate frequent itemsets
my_frequent_itemsets = apriori(my_basket, min_support=0.02, use_colnames=True)

my_rules = association_rules(my_frequent_itemsets, metric="lift", min_threshold=1)


