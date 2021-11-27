
# textblob
# https://textblob.readthedocs.io/en/dev/api_reference.html

from textblob import TextBlob 

print("*" * 25, '\n')
print("program is running ...\n")

wiki = TextBlob("Python is the best language, high level programming language")

print('\t', wiki)
print(" wiki tags: ",wiki.tags,'\n')


print(wiki.noun_phrases)


# sentiment analysis, polarity -1 : +1
message = TextBlob("This TextBlob is really great")
print(' sentiment: ',message.sentiment)
# print(' polarity: ',message.polarity)




# TOKENIZATION
zen = TextBlob("Curb your attitude. Second sentence here.")

print('words: ',zen.words)
print('sentences',zen.sentences)



for sentence in zen.sentences:
	print(sentence.sentiment)













print('\n',"*" * 25)


