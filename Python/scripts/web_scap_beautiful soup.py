"""
scrap data from National weather service using beautiful soup library
analyse with pandas library
"""
# shit+alt+e = run selected lines
# use request library's `get` function to download html contents of a given server

import sys
import requests

page = requests.get("https://dataquestio.github.io/web-scraping-pages/simple.html")
page

# a status code starting with 2 indicates success, 4 and 5 indicate failure
# print html content with content
page.content

# parsing page with beautiful soup

from bs4 import BeautifulSoup
soup = BeautifulSoup(page.content, "html.parser")
print(soup.prettify())

# As all the tags are nested, we can move through the structure one
# level at a time. We can first select all the elements at the top
# level of the page using the children property of soup.
# Note that children returns a list generator, so we need to call the list function on it:

list(soup.children)
# The above tells us that there are two tags at the top level of the page —
# the initial <!DOCTYPE html> tag, and the <html> tag. There is a newline character
# (n) in the list as well. Let’s see what the type of each element in the list is

[type(item) for item in list(soup.children)]
# As we can see, all of the items are BeautifulSoup objects:
#
# The first is a Doctype object, which contains information about the type of the document.
# The second is a NavigableString, which represents text found in the HTML document.
# The final item is a Tag object, which contains other nested tags.

# The most important object type, and the one we’ll deal with most often, is the Tag object.
#
# The Tag object allows us to navigate through an HTML document, and extract other tags
# and text. You can learn more about the various BeautifulSoup objects
# more on BeautifulSoup objects:  https://www.crummy.com/software/BeautifulSoup/bs4/doc/#kinds-of-objects

#  We can now select the html tag and its children by taking the third item in the list

html = list(soup.children)[2]

# Each item in the list returned by the children property is also a BeautifulSoup object,
# so we can also call the children method on html.
# Now, we can find the children inside the html tag:

list(html.children)

# As we can see above, there are two tags here, head, and body. We want to
# extract the text inside the p tag, so we’ll dive into the body:

body = list(html.children)[3]
# Now we can find the p tag by finding the children of the body tag

list(body.children)
# we can now isolate the p tag
p = list(body.children)[1]
# Once we’ve isolated the tag, we can use the get_text
# method to extract all of the text inside the tag:
p.get_text()

# finding all instances of a tag at once with find_all()
soup = BeautifulSoup(page.content, 'html.parser')
# Note that find_all returns a list, so we’ll have to loop through,
# or use list indexing, it to extract text
soup.find_all('p')[0].get_text()

# If you instead only want to find the first instance of a tag,
# you can use the find method, which will return a single BeautifulSoup object:

soup.find_all('p')

# searching for tags by class and id

page = requests.get("https://dataquestio.github.io/web-scraping-pages/ids_and_classes.html")
soup = BeautifulSoup(page.content, 'html.parser')
soup

# Now, we can use the find_all method to search for items
# by class or by id. In the below example, we’ll search for any p tag that has the class outer-text:

soup.find_all('p', class_='outer-text')

# In the below example, we’ll look for any tag that has the class outer-text:

soup.find_all(class_='outer-text')

# we can also search for elements by id:

soup.find_all(id='first')

# Using CSS Selectors - using selectorGadget extension : https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb?hl=en

# p a — finds all a tags inside of a p tag.
# body p a — finds all a tags inside of a p tag inside of a body tag.
# html body — finds all body tags inside of an html tag.
# p.outer-text — finds all p tags with a class of outer-text.
# p#first — finds all p tags with an id of first.
# body p.outer-text — finds any p tags with a class of outer-text inside of a body tag.


# BeautifulSoup objects support searching a page via CSS selectors using
# the select method. We can use CSS selectors to find all the p tags
# in our page that are inside of a div like this:

soup.select('div p')

# Downloading weather data: https://forecast.weather.gov/MapClick.php?lat=37.7772&lon=-122.4168#.YJf847UzaUk


# If we click around on the console, and explore the div,
# we’ll discover that each forecast item (like “Tonight”, “Thursday”,
# and “Thursday Night”) is contained in a div with the class tombstone-container

page = requests.get('https://forecast.weather.gov/MapClick.php?lat=37.7772&lon=-122.4168')
soup = BeautifulSoup(page.content, 'html.parser')
seven_day = soup.find(id='seven-day-forecast')
forecast_items = seven_day.find_all(class_='tombstone-container')
tonight = forecast_items[0]
print(tonight.prettify())

# Extracting information from the page

# As we can see, inside the forecast item tonight is all the information we want.
# There are four pieces of information we can extract:
#
# The name of the forecast item — in this case, Tonight.
# The description of the conditions — this is stored in the title property of img.
# A short description of the conditions — in this case, Mostly Clear.
# The temperature low — in this case, 49 degrees.

period = tonight.find(class_='period-name').get_text()
short_desc = tonight.find(class_='short-desc').get_text()
temp = tonight.find(class_='temp').get_text()

print(period, short_desc, temp, sep='\n')

# Now, we can extract the title attribute from the img tag. To do this,
# we just treat the BeautifulSoup object like a dictionary, and pass in the attribute we want as a key:

img = tonight.find('img')
desc = img['title']
print(desc)

# Extracting all the information from the page

# Now that we know how to extract each individual piece of information,
# we can combine our knowledge with CSS selectors and list comprehensions to extract everything at once.

# In the below code, we will:

# Select all items with the class period-name inside an item with the class tombstone-container in seven_day.
# Use a list comprehension to call the get_text method on each BeautifulSoup object.

period_tags = seven_day.select('.tombstone-container .period-name')
periods = [pt.get_text() for pt in period_tags]
periods

# As we can see above, our technique gets us each of the period names, in order.

# We can apply the same technique to get the other three fields:

short_desc = [sd.get_text() for sd in seven_day.select('.tombstone-container .short-desc')]
temps = [t.get_text() for t in seven_day.select('.tombstone-container .temp')]
descs = [d['title'] for d in seven_day.select('.tombstone-container img')]

# put to pandas dataframe

import pandas as pd
weather = pd.DataFrame({
    "period": periods,
    "short_desc": short_descs,
    "temp": temps,
    "desc":descs
})
weather