import requests
from bs4 import BeautifulSoup
import pandas as pd
from skimage import io
import cv2
import numpy
from sklearn.cluster import KMeans

#code for image color function from https://stackoverflow.com/questions/43111029/how-to-find-the-average-colour-of-an-image-in-python-with-opencv
def visualize_colors(cluster, centroids):
    #get the number of different clusters, create histogram, and normalize
    labels = numpy.arange(0, len(numpy.unique(cluster.labels_)) + 1)
    (hist, _) = numpy.histogram(cluster.labels_, bins = labels)
    hist = hist.astype("float")
    hist /= hist.sum()

    #create frequency rect and iterate through each cluster's color and percentage
    colors_list = [(percent, color) for (percent, color) in zip(hist, centroids)]
    colors = sorted(colors_list, key=lambda x: x[0])
    #calculate which of the first three dominant colors are "brightest" - have values for R, G, B that are the furthest apart
    c1_diff = abs(colors[-1][1][0] - colors[-1][1][1]) + abs(colors[-1][1][1] - colors[-1][1][2]) + abs(colors[-1][1][0] - colors[-1][1][2])
    c2_diff = abs(colors[-2][1][0] - colors[-2][1][1]) + abs(colors[-2][1][1] - colors[-2][1][2]) + abs(colors[-2][1][0] - colors[-2][1][2])
    c3_diff = abs(colors[-3][1][0] - colors[-3][1][1]) + abs(colors[-3][1][1] - colors[-3][1][2]) + abs(colors[-3][1][0] - colors[-3][1][2])
    max_diff = max(c1_diff, c2_diff, c3_diff)
    #return the brightest of the top three dominant colors
    if max_diff is c1_diff:
        return(colors[-1][1])
    if max_diff is c2_diff:
        return(colors[-2][1])
    return(colors[-3][1])

#web scrape the list of movies
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=1"
page1 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=2"
page2 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=3"
page3 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=4"
page4 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=5"
page5 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=6"
page6 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=7"
page7 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=8"
page8 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=9"
page9 = requests.get(url)
url = "https://www.imdb.com/list/ls098063263/?sort=list_order,asc&st_dt=&mode=detail&page=10"
page10 = requests.get(url)

#create the dataframe that will store the data
final_data = pd.DataFrame(index = range(1000), columns = ["Title", "Year", "Audience", "Runtime", "Rating", "Earnings", "Color", "Genres", "G_Action", "G_Adventure", \
    "G_Animation", "G_Biography", "G_Comedy", "G_Crime", "G_Documentary", "G_Drama", "G_Family", "G_Fantasy", "G_History", "G_Horror", "G_Music", "G_Musical", \
    "G_Mystery", "G_Romance", "G_SciFi", "G_Thriller", "G_War", "G_Western"])
index_now = 0

#go through each page and retrieve the desired elements
for page in [page1, page2, page3, page4, page5, page6, page7, page8, page9, page10]:
    soup = BeautifulSoup(page.content, "html.parser")
    soup_result = soup.find(class_ = "lister-list").find_all(class_ = "lister-item mode-detail")

    #retrieve elements of each movie using the closest class 
    for movie in soup_result:
        title_element = movie.find(class_ = "lister-item-header")
        year_element = movie.find(class_ = "lister-item-year text-muted unbold")
        audience_element = movie.find(class_ = "certificate")
        runtime_element = movie.find(class_ = "runtime")
        genres_element = movie.find(class_ = "genre")
        rating_element = movie.find(class_ = "ipl-rating-star__rating")
        earnings_element = movie.find(class_ = "list-description")
        color_element = movie.find("img", class_ = "loadlate")

        #clean up the retrieved html
        title = str(title_element).split("\n")[2].split(">")[1][:-3].replace("&amp;", "&").replace("·", "-").replace("é", "e")      #relacements done to clean up annoying Unicode characters
        year = str(year_element)[-12:-8]
        if str(audience_element) == "None":
            audience = "Not Rated"
        else:
            audience = str(audience_element).split(">")[1][:-6]
        runtime = str(runtime_element).split(">")[1][:-10]
        genres_old = str(genres_element).split(">")[1].split()[:-1]
        genres = []
        for genre in genres_old:
            if "," in genre:
                genre = genre[:-1]
            genres.append(genre)
        rating = str(rating_element).split(">")[1][:-6]
        earnings = str(earnings_element).split("$")[1][:-14].replace(",", "")

        #code for image color from https://stackoverflow.com/questions/43111029/how-to-find-the-average-colour-of-an-image-in-python-with-opencv
        color_old = io.imread(str(color_element).split("=")[5][1:-5])
        color_old = cv2.cvtColor(color_old, cv2.COLOR_BGR2RGB)
        reshape = color_old.reshape((color_old.shape[0] * color_old.shape[1], 3))
        cluster = KMeans(n_clusters=5).fit(reshape)                                     #this line may generate warnings; run with -W ignore in terminal to avoid them
        color = visualize_colors(cluster, cluster.cluster_centers_)

        #color code retrieved and converted from RGB values to hex values for better use in R
        color_list = [hex(round(int(color[0])))[2:], hex(round(int(color[1])))[2:], hex(round(int(color[2])))[2:], "#"]
        color_list.reverse()
        for i in range(1,len(color_list)):
            if len(color_list[i]) == 1:
                color_list[i] = "".join(["0", color_list[i]])
        color = "".join(color_list)

        #creating genre variables for better use in R
        if "Action" in genres:
            g_action = True
        else:
            g_action = False
        if "Adventure" in genres:
            g_adventure = True
        else:
            g_adventure = False
        if "Animation" in genres:
            g_animation = True
        else:
            g_animation = False
        if "Biography" in genres:
            g_biography = True
        else:
            g_biography = False
        if "Comedy" in genres:
            g_comedy = True
        else:
            g_comedy = False
        if "Crime" in genres:
            g_crime = True
        else:
            g_crime = False
        if "Documentary" in genres:
            g_documentary = True
        else:
            g_documentary = False
        if "Drama" in genres:
            g_drama = True
        else:
            g_drama = False
        if "Family" in genres:
            g_family = True
        else:
            g_family = False
        if "Fantasy" in genres:
            g_fantasy = True
        else:
            g_fantasy = False
        if "History" in genres:
            g_history = True
        else:
            g_history = False
        if "Horror" in genres:
            g_horror = True
        else:
            g_horror = False
        if "Music" in genres:
            g_music = True
        else:
            g_music = False
        if "Musical" in genres:
            g_musical = True
        else:
            g_musical = False
        if "Mystery" in genres:
            g_mystery = True
        else:
            g_mystery = False
        if "Romance" in genres:
            g_romance = True
        else:
            g_romance = False
        if "Sci-Fi" in genres:
            g_scifi = True
        else:
            g_scifi = False
        if "Thriller" in genres:
            g_thriller = True
        else:
            g_thriller = False
        if "War" in genres:
            g_war = True
        else:
            g_war = False
        if "Western" in genres:
            g_western = True
        else:
            g_western = False
    
        #add movie's data to the dataframe
        final_data.loc[index_now] = [title, year, audience, runtime, rating, earnings, color, genres, g_action, g_adventure, g_animation, g_biography, g_comedy, \
            g_crime, g_documentary, g_drama, g_family, g_fantasy, g_history, g_horror, g_music, g_musical, g_mystery, g_romance, g_scifi, g_thriller, g_war, g_western]
        index_now += 1

#export dataframe to csv for use with R
final_data.to_csv("movies.csv")