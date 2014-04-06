from bs4 import BeautifulSoup
# from BeautifulSoup import NavigableString, Tag
import requests
import csv
import unicodedata

entries = []
entry = []
urlnumber = 1 # Give the page number to start with

while urlnumber<100: # Give the page number to end with

    print type(urlnumber), urlnumber

    url = 'http://forums.edmunds.com/discussion/2864/bmw/tl/entry-level-luxury-performance-sedans/p%d' % (urlnumber,) #Give the url of the forum, excluding the page number in the hyperlink
    print url


    try:
        r = requests.get(url, timeout = 10) #Sending a request to access the page
    except Exception,e:
        break

    data = r.text

    soup = BeautifulSoup(data) # Getting the page source into the soup
    for div in soup.find_all('div'):
        entry = []
        if(div.get('class') != None and div.get('class')[0] == 'Comment'): # A single post is referred to as a comment. Each comment is a block denoted in a div tag which has a class called comment. 
            ps = div.find_all('p') #gets the message / body of the post
            aas = div.find_all('a') # gets the name of the person posting
            spans = div.find_all('span') #
            times = div.find_all('time') # Date of the post
            
            concat_str = ''
            for str in aas[1].contents:
                if str != "<br>" or str != "<br/>": # This denotes breaks in post which we need to work around.
                    concat_str = (concat_str + ' '+ str.encode('iso-8859-1')).strip() # The encoding is because the format exracted is a unicode. We need a uniform structure to work with the strings.
            entry.append(concat_str)
            
            concat_str = ''
            for str in times[0].contents:
                if str != "<br>" or str != "<br/>":
                    concat_str = (concat_str + ' '+ str.encode('iso-8859-1')).strip()
            entry.append(concat_str)

            #print "-------------------------"
            for div in div.find_all('div'):
                if (div.get('class') != None and div.get('class')[0] == 'Message'): # Extracting the div tag witht the class attribute as message.
                    blockqoutes = []
                    x = div.get_text()
                    for bl in div.find_all('blockquote'):
                        blockqoutes.append(bl.get_text()) #Block quote is used to get the quote made by a person
                        bl.decompose()
                

                    entry.append(div.get_text().replace("\n"," ").replace("<br/>","").encode('ascii','replace').encode('iso-8859-1'))

#print blockqoutes
                    for bl in blockqoutes:
                        print bl
                        entry.append(bl.replace("\n"," ").replace("<br/>","").encode('ascii','replace').encode('iso-8859-1'))
                
                #print entry
            entries.append(entry)

    urlnumber = urlnumber + 1 # increment so that we can extract the next page

with open('edmunds_sample12345.csv', 'w') as output:
    writer = csv.writer(output, delimiter= ',', lineterminator = '\n')
    writer.writerows(entries)
print "Wrote to edmunds_sample1.csv"

