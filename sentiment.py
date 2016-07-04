from vaderSentiment import vaderSentiment
import csv
import matplotlib.pyplot as plt
import re
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from collections import Counter
from collections import defaultdict
import operator 
import string
import vincent
from nltk import bigrams

#Shuopeng Wu
#reference : https://pypi.python.org/pypi/vaderSentiment


punctuation = list(string.punctuation)
A = ["amazondeals'","#amazon" ,"amazon" ,"amazon's","@amazon"]
W = ["#walmart","walmart","walmart's"]
stop = stopwords.words('english') + punctuation + A + W

def is_ascii(s):
	return all(ord (c) < 128 for c in s)
    
        
def preprocess(s):
        sentence = ""
        for word in s:
                if is_ascii(word):
                        if word not in stop:
                                sentence += word
        return sentence
       


D = {}
D["amazon"]  = [0,0,0,0]
D["walmart"] = [0,0,0,0]

with open('final.csv', 'rb') as f:
    mycsv = csv.reader(f)
    lamazon = 0
    lwalmart = 0
    for row in mycsv:
    	txt = row[2]
    	pre = preprocess(txt)
        vs = vaderSentiment.sentiment(pre.encode("utf8"))
        if row[0] in A:
        	D["amazon"][0] += vs["neg"]
        	D["amazon"][1] += vs["neu"]
        	D["amazon"][2] += vs["pos"]
        	D["amazon"][3] += vs["compound"]
        	lamazon += 1
        elif row[0] in W:
        	D["walmart"][0] += vs["neg"]
        	D["walmart"][1] += vs["neu"]
        	D["walmart"][2] += vs["pos"]
        	D["walmart"][3] += vs["compound"]
        	lwalmart += 1
print D

D["amazon"][0] = D["amazon"][0] /lamazon
D["amazon"][1] = D["amazon"][1] /lamazon
D["amazon"][2] = D["amazon"][2] /lamazon
D["amazon"][3] = D["amazon"][3] /lamazon

D["walmart"][0] = D["walmart"][0] /lwalmart
D["walmart"][1] = D["walmart"][1] /lwalmart
D["walmart"][2] = D["walmart"][2] /lwalmart
D["walmart"][3] = D["walmart"][3] /lwalmart
print lamazon
print lwalmart
print D







