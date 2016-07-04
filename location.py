import json
from vaderSentiment import vaderSentiment
import operator


path = 'data.txt'

input = open(path, 'r')
line = input.readline()


location_tweets = {}



def parse(tweet):
    "Extracts country_code, state, text from a tweet"
    try:
        country = tweet['place']['country_code']
        state = tweet['place']['full_name'].split(", ")[1]
        text = tweet['text']
        return [country, state, text]
    except (KeyError, TypeError, IndexError):
        return None

while line != '':
	tweet = json.loads(line)
	status = parse(tweet)
	if status:
		if len(status[1]) == 2 and status[0] == 'US':
			if status[1] not in location_tweets:
				location_tweets[status[1]] = [status[2]]
			else:
				location_tweets[status[1]].append(status[2])

	line = input.readline()

with open("location_tweets.json", "w") as f:
	f.write(json.dumps(location_tweets))

company_keywords = { 
	"Microsoft": [ "Microsoft","microsoft","MICROSOFT" ],
	"lenovo": ["lenovo","Lenovo","LENOVO"],
	"APPLEOFFIClAL": ["apple", "Apple","APPLEOFFIClAL","APPPLE","Appleofficial"],
	"BestBuy":["BestBuy","BESTBUY","bestbuy"],
	"amazon":["amazon","Amazon","AMAZON"],
	"Walmart":["Walmart","walmart","WALMART"],
	"Macys":["Macys","MACYS","macys"],
	"Nordstorm":["Nordstorm","nordstorm","NORDSTORM"],
	"Sephora":["Sephora","SEPHORA","sephora"],
	"Target":["Target","TARGET","target"]
}

finalanswer1 = {}
finalanswer2 = {}
with open("location_tweets.json", "r") as f:
    contents = f.read()
    _location_tweets = json.loads(contents)
    for location in _location_tweets:
    	finalanswer1[location] = []
        finalanswer2[location] = 0
    	company_tweets = {}
    	for c in company_keywords:
            company_tweets[c] = []
	    for content in _location_tweets[location]:
                for kw in company_keywords[c]:
                    if kw in content:
                        company_tweets[c].append(content)
                        break
        for company in company_tweets:
                sentences = company_tweets[company]
                neg = 0.0
                neu = 0.0
                pos = 0.0
                for sentence in sentences:
                   vs = vaderSentiment.sentiment(sentence.encode("utf8"))
                   neg += vs["neg"]
                   neu += vs["neu"]
                   pos += vs["pos"]
                finalanswer1[location].append([company,(neg+neu+pos)])
                finalanswer2[location] += neg+neu+pos

# finalanswer2 gives the happiness of states from most to least
finalanswer2 = sorted(finalanswer2.items(), key=lambda x: (-x[1], x[0]))

# top 5 -> each company
for stateL in finalanswer2[:5]:
    print stateL[0]
    print finalanswer1[stateL[0]]

