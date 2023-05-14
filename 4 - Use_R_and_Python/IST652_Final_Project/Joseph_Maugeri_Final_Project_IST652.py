# %% 
# About the File
'''
Author: Joseph R Maugeri
Class: IST 652
Week: 10
Activity: Final Project: Crypto and tweets.
Purpose: identify Elon Musks Influence through twitter on 
    Cryptocurrency twitter community
        Through Activity related to 
            # of Retweets, Likes, and Quotes related to ETH BTC and DOGE
            # Most popular days for activty related to ETH BTC and DOGE from Musk
    Cryptocurrency Market Value
        Market value Open in USD - Market value Close USD
'''
# %% Importing packages to use 
# importing libraries and packages
import snscrape.modules.twitter as sntwitter
import pandas as pd
import numpy as np
from datetime import date, datetime as dt
import csv
import nltk
import re
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.cbook as cbook

# %% snscrape fields avaialable for tweet class objects
'''

 class Tweet(typing.NamedTuple, snscrape.base.Item): 
 	url: str 
 	date: datetime.datetime 
 	content: str 
 	renderedContent: str 
 	id: int 
 	username: str # Deprecated, use user['username'] instead 
 	user: 'User' 
 	outlinks: list 
 	outlinksss: str # Deprecated, use outlinks instead 
 	tcooutlinks: list 
 	tcooutlinksss: str # Deprecated, use tcooutlinks instead 
 	replyCount: int 
 	retweetCount: int 
 	likeCount: int 
 	quoteCount: int 
 	conversationId: int 
 	lang: str 
 	source: str 
 	media: typing.Optional[typing.List['Medium']] = None 
 	retweetedTweet: typing.Optional['Tweet'] = None 
 	quotedTweet: typing.Optional['Tweet'] = None 
 	mentionedUsers: typing.Optional[typing.List['User']] = None 
     '''

# %% Creating list to append tweet data 
musk_tweets = []

# %%
sntwitter.TwitterSearchScraper('from:elonmusk since:2020-01-01 until:2021-12-31').get_items()
# %%
# Using TwitterSearchScraper to scrape data and append tweets to list
for i,tweet in enumerate(sntwitter.TwitterSearchScraper('from:elonmusk since:2021-01-01 until:2021-12-31').get_items()): #declare a username and timeframe
    if i>15000: #number of tweets you want to scrape
        print('There were:', i,'Tweets.')
        break
    musk_tweets.append([tweet.date, tweet.id, tweet.content, tweet.user.username ,tweet.retweetCount, tweet.mentionedUsers, tweet.likeCount,tweet.quoteCount]) #declare the attributes to be returned

# %%
# Creating a dataframe from the tweets list above 
musk_tweets_df = pd.DataFrame(musk_tweets, columns=['Datetime', 'Tweet Id', 'Text', 'Username','retweetCount','mentionedUsers','likeCount','quoteCount'])
type(musk_tweets_df)
musk_tweets_df

# %% Ran repeatedly to see a couple examples of NaN in different fields
musk_tweets_df.sample()
# %% Checking to see how many tweets were pulled
len(musk_tweets_df)
# %% Replacing 0s in our counts of popularity indicators
musk_tweets_df['retweetCount'] = musk_tweets_df['retweetCount'].fillna(0)
musk_tweets_df['likeCount'] = musk_tweets_df['likeCount'].fillna(0)
musk_tweets_df['quoteCount'] = musk_tweets_df['quoteCount'].fillna(0)
# %% 
type(musk_tweets_df['Datetime'].iloc[0])

# %%
# Converting timestamps to utc , and also then removes the timezone through localization
musk_tweets_df['Datetime'] = pd.to_datetime(musk_tweets_df['Datetime'], utc=True)
musk_tweets_df['Datetime'] = musk_tweets_df['Datetime'].dt.tz_localize(None)
musk_tweets_df['Datetime'].iloc[0]
musk_tweets_df.dtypes

# %%
# Turning the timestamp into a string to then turn back into DateTweet and TimeTweet
musk_tweets_df['Datetime'] = musk_tweets_df['Datetime'].dt.strftime('%Y-%m-%d %H:%M:%S')

# %%
# Taking the Datetime column's str and turning it back into a datetime stored in final locations
musk_tweets_df['DateTweet'] = pd.to_datetime(musk_tweets_df['Datetime'])
musk_tweets_df['timeTweet'] = pd.to_datetime(musk_tweets_df['Datetime'])
musk_tweets_df['DateTweet'].iloc[0]
musk_tweets_df.dtypes

# %%
# Using lambda to change datetime64 to a date for ['DateTweet' and time for ['timeTweet']
musk_tweets_df['DateTweet'] = musk_tweets_df.DateTweet.apply(lambda x: x.date())
musk_tweets_df['timeTweet'] = musk_tweets_df.timeTweet.apply(lambda x: x.time())
# %%
# Check to make sure the new columns are now datetime.date and datetime.time , GREAT!
print( type(musk_tweets_df['DateTweet'].iloc[0]), musk_tweets_df['DateTweet'].iloc[0])
print( type(musk_tweets_df['timeTweet'].iloc[0]), musk_tweets_df['timeTweet'].iloc[0])

# %%
# Need to move the index out for ability to join in future
musk_tweets_df.reset_index(inplace=True)
# %%
# Copying the dataframe to have a breakpoint for testing different configurations
musk_tweet_daily = musk_tweets_df[['DateTweet','retweetCount','likeCount','quoteCount','Text']]
# %%
musk_tweet_daily.describe()
len(musk_tweet_daily)
# %%
# %%
# A forenote, this code will look strange, as the ultimate solution
# was to put new columns with aggregates by 'DateTweet' column, 
# and then ulitmately dropping the duplicates out of the dataframe, 
# this is so we can look at the frequency of Musks' tweeting, 
# as well as the overall impact on a daily basis
# as we will later merge this dataset with crypto market datasets.
# 
# Using .groupby and .transform to make a new column that is a duplicated Daily count of the tweets
musk_tweet_daily['TweetsOnDay'] = musk_tweet_daily.groupby(['DateTweet'])['DateTweet'].transform('count')
musk_tweet_daily.iloc[100:110,:]
# %%
# Adding a daily point value in this dataframe so for each individual day we could calculate statistics
# related to what percentage of the total bin that day would one tweet weigh. 
musk_tweet_daily['Daily_RT'] = musk_tweet_daily.groupby(['DateTweet'])['retweetCount'].cumsum()
musk_tweet_daily['Daily_L'] = musk_tweet_daily.groupby(['DateTweet'])['likeCount'].cumsum()
musk_tweet_daily['Daily_Q'] = musk_tweet_daily.groupby(['DateTweet'])['quoteCount'].cumsum()
# %%
#Checking despite the setting with copy warning, which shouldn't be ignored
musk_tweet_daily.describe()
musk_tweet_daily.sample()

# %%
# Now grouping all text fields together into the 'Text' column , separated by a space.
musk_tweet_daily['Text'] = musk_tweets_df.groupby(['DateTweet'])['Text'].transform( lambda x : ' '.join(x))
musk_tweet_daily.sample()
# %%
musk_tweet_daily.iloc[100:110,:]

# %%
musk_tweet_daily = musk_tweet_daily.drop_duplicates('DateTweet')
musk_tweet_daily.iloc[100:110,:]
# %%
len(musk_tweet_daily)


# %%
# BEGINNING OF MARKET DATA SECTION
#
# # Loading Twitter stock  data from csv files in directory 
# (same format as crypto below, USD from https://finance.yahoo.com)
# Single difference with twitter is that the market closes so there are days with no trades - no value
TWTRcsv = 'TWTR-USD.csv'
TWTRdf = pd.read_csv(TWTRcsv)
len(TWTRdf)
# %%
# Loading DOGE Cryptocurrency data from csv files in directory
DOGEcsv = 'DOGE-USD.csv'
DOGEdf = pd.read_csv(DOGEcsv)
len(DOGEdf)
# %%
# Loading BTC Cryptocurrency data from csv files in directory
BTCcsv = 'BTC-USD.csv'
BTCdf = pd.read_csv(BTCcsv)
len(BTCdf)
# %%
# Loading ETH Cryptocurrency data from csv files in directory
ETHcsv = 'ETH-USD.csv'
ETHdf = pd.read_csv(ETHcsv)
len(ETHdf)

# %%
# Take a look to see what columns are present
BTCdf.sample()
# %%
# Bitcoin dataset statstics, we have two years of data present,
BTCdf.describe()

# %%
# adding columns to each dataset termed as follows
# OCDelta is Open minus Close value, negative would mean a net decrease, positive means net increase
TWTRdf['OCDelta'] = TWTRdf['Open']-TWTRdf['Close']
DOGEdf['OCDelta'] = DOGEdf['Open']-DOGEdf['Close']
BTCdf['OCDelta'] = BTCdf['Open']-BTCdf['Close']
ETHdf['OCDelta'] = ETHdf['Open']-ETHdf['Close']
# HLDelta is the High minus Low value
TWTRdf['HLDelta'] = TWTRdf['High']-TWTRdf['Low']
DOGEdf['HLDelta'] = DOGEdf['High']-DOGEdf['Low']
BTCdf['HLDelta'] = BTCdf['High']-BTCdf['Low']
ETHdf['HLDelta'] = ETHdf['High']-ETHdf['Low']
# PerChange is the 'OCDelta' over the 'Open' to let us know how much was gained or lost
TWTRdf['HLDelta'] = TWTRdf['OCDelta']/TWTRdf['Open']
DOGEdf['PerChange'] = DOGEdf['OCDelta']/DOGEdf['Open']
BTCdf['PerChange'] = BTCdf['OCDelta']/BTCdf['Open']
ETHdf['PerChange'] = ETHdf['OCDelta']/ETHdf['Open']
# %%
ETHdf.describe()
# %%
# Ensuring date is in datetime.date format using pandas.to_datetime
TWTRdf['Date'] = pd.to_datetime(TWTRdf['Date'])
DOGEdf['Date'] = pd.to_datetime(DOGEdf['Date'])
BTCdf['Date'] = pd.to_datetime(BTCdf['Date'])
ETHdf['Date'] = pd.to_datetime(ETHdf['Date'])
# %%
#checking one of the dataframes, we still need to apply .date to each df
type(ETHdf)
ETHdf.dtypes
type(ETHdf['Date'].iloc[0])
# %%
TWTRdf['Date'] = pd.to_datetime(TWTRdf['Date'], utc=True)
#
DOGEdf['Date'] = pd.to_datetime(DOGEdf['Date'], utc=True)
#DOGEdf['Date'] = DOGEdf['Date'].dt.strftime('%Y-%m-%d')

BTCdf['Date'] = pd.to_datetime(BTCdf['Date'], utc=True)
#BTCdf['Date'] = BTCdf['Date'].dt.strftime('%Y-%m-%d')

ETHdf['Date'] = pd.to_datetime(ETHdf['Date'], utc=True)
#ETHdf['Date'] = ETHdf['Date'].dt.strftime('%Y-%m-%d')

# %%
# using lambda function to convert all to .date
TWTRdf['Date'] = TWTRdf.Date.apply(lambda x: x.date())
DOGEdf['Date'] = DOGEdf.Date.apply(lambda x: x.date())
BTCdf['Date'] = BTCdf.Date.apply(lambda x: x.date())
ETHdf['Date'] = ETHdf.Date.apply(lambda x: x.date())
# %%
# Adding a string date for plotting later
TWTRdf['strDate'] = TWTRdf['Date'].to_string()
DOGEdf['strDate'] = DOGEdf['Date'].to_string()
BTCdf['strDate'] = BTCdf['Date'].to_string()
ETHdf['strDate'] = ETHdf['Date'].to_string()
# %%
# Checking here , we see all df should be formatted as datetime.date
print(ETHdf['Date'].iloc[0])
type(ETHdf['Date'].iloc[0])
# This will be string
print(ETHdf['strDate'].iloc[0])
type(ETHdf['strDate'].iloc[0])
# %%
# Checking here , we see all df should be formatted as datetime.date
print(type(musk_tweet_daily['DateTweet'].iloc[0]))
# %%
# one final look at the musk_tweet_daily dataframe before creating merges
musk_tweet_daily.describe()
# %%
# merging all of our market dataframes with 
# our refined twitter dataframe  -  musk_tweet_daily
#
musk_TWTR = pd.merge(
    TWTRdf,
    musk_tweet_daily, 
    how="outer",
    left_on = 'Date',
    right_on = 'DateTweet'
    )
display(musk_TWTR)

musk_BTC = pd.merge(
    BTCdf,
    musk_tweet_daily, 
    how="outer",
    left_on = 'Date',
    right_on = 'DateTweet'

    )
display(musk_BTC)


musk_ETH = pd.merge(
    ETHdf,
    musk_tweet_daily, 
    how="outer",
    left_on = 'Date',
    right_on = 'DateTweet'

    )
display(musk_ETH)

musk_doge = pd.merge(
    DOGEdf,
    musk_tweet_daily, 
    how="outer",
    left_on = 'Date',
    right_on = 'DateTweet'

    )
display(musk_doge)

# %%
# We will need to fill our NA's with 0
musk_TWTR= musk_TWTR.fillna(0)

musk_BTC = musk_BTC.fillna(0)

musk_ETH = musk_ETH.fillna(0)

musk_doge = musk_doge.fillna(0)
# %%
#
# BEGINNING OF TOKENIZATION SECTION
#
msglist = musk_tweet_daily['Text']
musk_tokens = [tok for msg in msglist for tok in nltk.word_tokenize(msg)]
len(musk_tokens)
# %%
# Using nltk.FreqDist to create a distribution of msgFD
# Using most_common() function, and 
msgFD = nltk.FreqDist(musk_tokens)
msgFD.most_common(30)
# %%
# Using .corpus attribute of nltk to get stopwords.words list for english language
# May need to download stopwords using---> nltk.download('stopwords')
nltk_stopwords = nltk.corpus.stopwords.words('english')
len(nltk_stopwords)

# %%
# Make sure re is imported 
# Defining a function to exclude regex string for nonsense / special characters
def musk_filter1(w):
    pattern = re.compile('^[^a-z]+$')
    if (pattern.match(w)):
        return True

    else:
        return False

# %%
# pattern = re.compile(r'\b(' + r'|'.join(stopwords.words('english')) + r')\b\s*')
def musk_filter2(w):
	pattern = re.compile(r'\b(' + r'|'.join(nltk_stopwords) + r')\b\s*')
	if (pattern.match(w)):
		return True
	else:
		return False
# %%
# Using musk_filter1 to remove any special characters / nonsense
token_list = [tok for tok in musk_tokens if not musk_filter1(tok)]
token_list[:30]

# %%
# Using musk_filter2 to remove stopwords
token_list2 = [tok for tok in token_list if not musk_filter2(tok)]
token_list2[:30]


# %%
# Using the FreqDist package and .mostcommon attribute for a frequency distribution
msgFD = nltk.FreqDist(token_list2)
# creating topwords call by using these attributes of a freq. table
top_words = msgFD.most_common(300)
    # and then printing the word and the frequency
for word, freq in top_words:
        print(word, freq)


# %%
# %%
# BEGINNING OF VISUALIZATION SECITON
#
# Plotting Twitter Open Data
fig, ax = plt.subplots()
ax.plot('Date', 'Open', data=TWTRdf)

# Major ticks every 6 months.
fmt_half_year = mdates.MonthLocator(interval=6)
ax.xaxis.set_major_locator(fmt_half_year)

# Minor ticks every month.
fmt_month = mdates.MonthLocator()
ax.xaxis.set_minor_locator(fmt_month)

# Text in the x axis will be displayed in 'YYYY-mm' format.
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))

# Format the coords message box, i.e. the numbers displayed as the cursor moves
# across the axes within the interactive GUI.
ax.format_xdata = mdates.DateFormatter('%Y-%m')
ax.format_ydata = lambda x: f'${x:.2f}'  # Format the price.
ax.grid(True)
ax.set_title('Twitter "Share" Open \'20-\'21')
# Rotates and right aligns the x labels, and moves the bottom of the
# axes up to make room for them.
fig.autofmt_xdate()

plt.show()
# %%
# Plotting ETH Open Data
fig, ax = plt.subplots()
ax.plot('Date', 'Open', data=ETHdf)

# Major ticks every 6 months.
fmt_half_year = mdates.MonthLocator(interval=6)
ax.xaxis.set_major_locator(fmt_half_year)

# Minor ticks every month.
fmt_month = mdates.MonthLocator()
ax.xaxis.set_minor_locator(fmt_month)

# Text in the x axis will be displayed in 'YYYY-mm' format.
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))

# Format the coords message box, i.e. the numbers displayed as the cursor moves
# across the axes within the interactive GUI.
ax.format_xdata = mdates.DateFormatter('%Y-%m')
ax.format_ydata = lambda x: f'${x:.2f}'  # Format the price.
ax.grid(True)
ax.set_title('Ether "Share" Open \'20-\'21')
# Rotates and right aligns the x labels, and moves the bottom of the
# axes up to make room for them.
fig.autofmt_xdate()

plt.show()# %%

# %%
# Plotting BTC Open Data
fig, ax = plt.subplots()
ax.plot('Date', 'Open', data=BTCdf)

# Major ticks every 6 months.
fmt_half_year = mdates.MonthLocator(interval=6)
ax.xaxis.set_major_locator(fmt_half_year)

# Minor ticks every month.
fmt_month = mdates.MonthLocator()
ax.xaxis.set_minor_locator(fmt_month)

# Text in the x axis will be displayed in 'YYYY-mm' format.
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))

# Format the coords message box, i.e. the numbers displayed as the cursor moves
# across the axes within the interactive GUI.
ax.format_xdata = mdates.DateFormatter('%Y-%m')
ax.format_ydata = lambda x: f'${x:.2f}'  # Format the price.
ax.grid(True)
ax.set_title('Bitcoin "Share" Open \'20-\'21')
# Rotates and right aligns the x labels, and moves the bottom of the
# axes up to make room for them.
fig.autofmt_xdate()

plt.show()# %%
# %%
# Plotting Dogecoin Open Data
fig, ax = plt.subplots()
ax.plot('Date', 'Open', data=DOGEdf)

# Major ticks every 6 months.
fmt_half_year = mdates.MonthLocator(interval=6)
ax.xaxis.set_major_locator(fmt_half_year)

# Minor ticks every month.
fmt_month = mdates.MonthLocator()
ax.xaxis.set_minor_locator(fmt_month)

# Text in the x axis will be displayed in 'YYYY-mm' format.
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))

# Format the coords message box, i.e. the numbers displayed as the cursor moves
# across the axes within the interactive GUI.
ax.format_xdata = mdates.DateFormatter('%Y-%m')
ax.format_ydata = lambda x: f'${x:.2f}'  # Format the price.
ax.grid(True)
ax.set_title('Dogecoin "Share" Open \'20-\'21')
# Rotates and right aligns the x labels, and moves the bottom of the
# axes up to make room for them.
fig.autofmt_xdate()

plt.show()# %%


# %%
