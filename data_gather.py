import tweepy
from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener

import config
import json



#need to install all the package listed for later analysis:
#on http://penandpants.com/2013/04/04/install-scientific-python-on-mac-os-x/


class MyListener(StreamListener):
    def on_data(self, data):
        
        try:
            with open('data.json', 'a') as f:
                f.write(data.encode('utf8'))
                print data
                return False
        except BaseException as e:
            time.sleep(3)
        return True
 
    def on_error(self, status):
        return True


if __name__ == '__main__':

    auth = OAuthHandler(config.consumer_key, config.consumer_secret)
    auth.set_access_token(config.access_token, config.access_secret)
    api = tweepy.API(auth)

    query_list = ["Microsoft","lenovo","APPLEOFFIClAL","BestBuy","amazon","Walmart","Macys","Nordstorm","Sephora"]

    twitter_stream = Stream(auth, MyListener(query_list))
    twitter_stream.filter(track=query_list,languages=["en"])






