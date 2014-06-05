from bs4 import BeautifulSoup
# from BeautifulSoup import NavigableString, Tag
import requests
import csv
import unicodedata
import re

entries = []
entry = []
urlnumber = 1 # Give the page number to start with

while urlnumber<61: # Give the page number to end with

    print type(urlnumber), urlnumber

    url = 'http://movie.douban.com/subject/10574622/comments?start=%d&limit=20&sort=new_score' % (urlnumber,) #Give the url of the forum, excluding the page number in the hyperlink
    print url


    try:
        r = requests.get(url, timeout = 10) #Sending a request to access the page
    except Exception,e:
        break

    data = r.text

    soup = BeautifulSoup(data) # Getting the page source into the soup
    for div in soup.find_all('div'):
        entry = []
        if(div.get('class') != None and div.get('class')[0] == 'comment'): # A single post is referred to as a comment. Each comment is a block denoted in a div tag which has a class called comment. 
            ps = div.find_all('p') #gets the message / body of the post
            aas = div.find_all('a') # gets the name of the person posting
            spans = div.find_all('span') #
            
            # Name
            concat_str = ''
            for a in aas:
                if (a.get('class') != None and a.get('class')[0] == ''):
                    for str in a.contents:
                        if str != "<br>" or str != "<br/>": # This denotes breaks in post which we need to work around.
                            concat_str = (concat_str + ' '+ str.encode('utf-8')).strip() # The encoding is because the format exracted is a unicode. We need a uniform structure to work with the strings.
                    entry.append(concat_str)

                    rate = a.next_sibling.next_sibling
                    star = rate['class'][0]

                    entry.append(star.replace("allstar",""))
            
            # Time
            concat_str = ''
            for time in spans:
                if (time.get('class') != None and time.get('class')[0] == ''):
                    for str in time.contents:
                        if str != "<br>" or str != "<br/>":
                            concat_str = (concat_str + ' '+ str.encode('utf-8')).strip()
                    entry.append(concat_str)

            #print "-------------------------"

            # Star rate
            #concat_str = ''
            #for rate in spans:
                #star = rate.find_all(class_=re.compile("allstar"))[0]
                #for str in star.name
                    #concat_str = (concat_str + ' '+ str.replace("allstar","").replace("rating","").encode('utf-8')).strip()

            # Usefulness
            concat_str = ''
            usefulness = div.find_all("span", "votes pr5")[0]
            for str in usefulness.contents:
                concat_str = (concat_str + ' '+ str.encode('utf-8')).strip()
            entry.append(concat_str)

            # Comment
            concat_str = ''
            for str in ps[0].strings:
                if str != "<br>" or str != "<br/>":
                    concat_str = (concat_str + ' '+ str.encode('utf-8')).strip()
            entry.append(concat_str)

 
            entries.append(entry)

    urlnumber = urlnumber + 21 # increment so that we can extract the next page

with open('douban_taijiong.csv', 'w') as output:
    writer = csv.writer(output, delimiter= ',', lineterminator = '\n')
    writer.writerows(entries)
soup.decompose()
print "Wrote to douban_taijiong.csv"

