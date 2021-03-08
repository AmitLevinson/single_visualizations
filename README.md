# Single Visualizations

In this folder I display assorted single visualizations I created in different contexts and from datatsets. Some of these have been recreated in a blog post or shared in some format on my website which I invite you to explore:    


#### [amitlevinson.com](https://amitlevinson.com/)  


Anyways, enjoy!  
</br>


### International Women's Day (IWD)
*08.03.2021* 
Decided to celebrate IWD by plotting all 57 female winning the Nobel Prize. Data and images were collected from Wikipedia and the full reproducible code can  be found below.
[Link to Code](https://github.com/AmitLevinson/single_visualizations/blob/master/point-distance/point_distance.R)

<p align="center">
<img src="nobel-women/nobel-women.png" width="700" alt="plot of women winning the nobel prize">
</p>



### Distance to various points
*17.01.2021* 
I'm working on a new blog post that includes calculating distances from a point to some other points. It was my first time diving deeper (than I'm used to) in to the `{sf}` R package.

[Link to Code](https://github.com/AmitLevinson/single_visualizations/blob/master/point-distance/point_distance.R)

<p align="center">
<img src="point-distance/distances.png" width="700">
</p>


### Happy holiday word-cloud
*18.09.2020*  
For the new Jewish year I created a word-cloud from our [Israel 2050 ](https://israel2050.co.il/en/home/) whatsapp chat group. Once you have the word-cloud set up you can easily make it for other groups which I did. I was having issues doing everything in R so I exported it to html and manually saved it. The logo and light-blue bar was also manually added. No chats are provided in this repo, but checkout the [rwhatsapp package](https://github.com/JBGruber/rwhatsapp) that helps with all the heavy lifting and guide you through the process (which is really easy).  

[(Link to code)](https://github.com/AmitLevinson/single_visualizations/blob/master/whatsapp-word-cloud/random-word-clouds.Rmd)

<p align="center">
<img src="whatsapp-word-cloud/chag-sameach-with-logo.jpg" width="700">
</p>



### An example of patchwork
*16.06*  
I shared on Linkedin the amazing features of [patchwork](https://patchwork.data-imaginist.com/index.html) and decided to share it here too if you stumble upon it. patchwork is an R package for combining several ggplots by [Thomas Lin Pedersen](https://www.data-imaginist.com/) with an easy and simple syntax. If you're looking for something to combine several ggplots into one graphic check it out!

[(Link to code)](https://github.com/AmitLevinson/single_visualizations/blob/master/patchwork-example/pactchwork-eg.R) 

<p align="center">
<img src="patchwork-example/p.png" width="700">
</p>


### A revisit to #TidyTuesday
*01.06*  
On behalf of [Almog Simchon](https://almogsi.com/)'s course at Ben-Gurion university of the Negev We were required to explore and visualize findings from a [#TidyTuesday](https://github.com/rfordatascience/tidytuesday) dataset. My partner and I chose a dataset about spotify songs which I explore in a sub-folder of this repository. Below is the final visualization from the exploration:  

[(Link to code and exploration)](https://github.com/AmitLevinson/single_visualizations/blob/master/exploring-tt/README.md) 

<p align="center">
<img src="exploring-tt/plot.png" width="700" height = "850">
</p>



### Choosing an appropriate font
*11.05*  
I decided to plot all my available fonts so that I don't have to try a font just to see how it looks. You can always find fonts on [Google fonts](https://fonts.google.com/), download and [install them to windowns](https://www.digitaltrends.com/computing/how-to-install-fonts-in-windows-10/). My personal favorite? ["Roboto Condensed"](https://fonts.google.com/specimen/Roboto+Condensed?preview.text=&preview.text_type=custom&query=roboto+condensed). Plot idea comes from Hadley Wickham's [ggplot 2 book](https://ggplot2-book.org/annotations.html), specifically ch.8.  

[(Link to plot code)](https://github.com/AmitLevinson/single_visualizations/blob/master/plotting-fonts/font-plot.R)  

<p align="center">
<img src="plotting-fonts/text_plot.png" width="700">
</p>

### **A street map of Be'er-Sheva, IL (left) and the city's light posts (right)**  
*23.11.2019*  
I mapped the streets of Be'er Sheva thanks to [Christian Burkhart](https://ggplot2tutor.com/streetmaps/streetmaps/) blog. Once I mapped the streets (left) I could map the city lights of the city (right) thanks to the municipality's free [open data](https://www.beer-sheva.muni.il/OpenData/Pages/Data.aspx).  

[Link to street map code](https://github.com/AmitLevinson/Projects/blob/master/beer_sheva_municipality/city_streets/street_map_gold.R) | [Link to light posts code](https://github.com/AmitLevinson/Projects/blob/master/beer_sheva_municipality/city_light_posts/city_light_b7_sf.R)  

<p align = "center">
<img src="beer_sheva_municipality/city_streets/street_map_gold.png" width = "350"/> <img src="beer_sheva_municipality/city_light_posts/street_light_of_b7.png" width = "350"/>
</p>  

### **Mapping bomb shelters in Be'er-Sheva, Israel**  
*12.11.2019*  

In this visualization I mapped the bomb shelters near my neighborhood in Be'er-Sheva. I discovered open data sets from our municipality's website and took the next day's opportunity (missiles fired towards Israel) to plot bomb shelters. I've since created a blog post on my website explaining how I made this map along with an interactive leaflet package; you can find it [here](https://amitlevinson.com/post/bomb-shelters/).    

[Link to code](https://github.com/AmitLevinson/Projects/blob/master/beer_sheva_municipality/mapping_bomb_shelters/shelters_b.R)

<p align = "center">
<img src="beer_sheva_municipality/mapping_bomb_shelters/shelters_b_heb.png" width = "600">
</p>  

### **Eliud Kipchoge unformal marathon record**  
*12.10.2019*  

In this visualization I took Eliud Kipchoge's marathon score of under 2 hours (1:59:40) and situated it in comparison to previous yearly records. I've since created a blog post where I scrape the data from Wikipedia and explain how I create this plot; you can find it [here](https://amitlevinson.com/post/eliud-kichoge/)    

[Link to code](https://github.com/AmitLevinson/Random_Visualizations/blob/master/Marathon_Records/marathon_runs.R)
</br>

<p align="center">
<img src="Marathon_Records/marathon_runs.png" width="600">
</p>