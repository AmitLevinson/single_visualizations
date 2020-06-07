Exploring spotify songs
================
Amit Levinson
6/1/2020

``` r
library(tidyverse)
library(lubridate)
library(ggpomological)
library(extrafont)
library(gt)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
```

Explore playlist genre by mean duration

``` r
by_duration <- spotify_songs %>%
    mutate(year = year(as.Date(track_album_release_date, "%Y-%m-%d"))) %>% 
    group_by(playlist_genre,year) %>% 
    summarise_at(vars(duration_ms),list(mean_ = mean, sd_ = sd)) %>% 
    ungroup()

ggplot(by_duration, aes(x = year, y= mean_, fill = playlist_genre, color = playlist_genre, group = playlist_genre))+
    geom_line()+
    theme_light()
```

<img src="README_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

That’s bad and unhelpful, let’s try by Explore genre by mean
danceability:

``` r
spotify_songs %>%
  mutate(year = year(as.Date(track_album_release_date, "%Y-%m-%d"))) %>% 
  group_by(playlist_genre,year) %>% 
  summarise_at(vars(danceability),list(mean_ = mean, sd_ = sd)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y= mean_, fill = playlist_genre, color = playlist_genre, group = playlist_genre))+
  geom_line()+
  theme_light()
```

<img src="README_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Again, doesn’t seem helpful. How about by popularity?

``` r
spotify_songs_y <- spotify_songs %>%
  mutate(track_year = year(as.Date(track_album_release_date, "%Y-%m-%d"))) 

spotify_pop <- spotify_songs_y %>% 
  group_by(playlist_genre, track_year) %>% 
  summarise(mean_pop = mean(track_popularity),
            count_n = n()) %>%
  filter(track_year >= 2000) %>% 
  ungroup()

ggplot(spotify_pop,aes(x = track_year, y = mean_pop, color = playlist_genre))+
  geom_line()
```

<img src="README_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Hmm, What’s happenning in edm throughout 2000-2010?

``` r
spotify_pop %>% 
  filter(playlist_genre == "edm") %>% 
  arrange(track_year) %>% 
  gt() %>% 
  tab_style(style = cell_fill("lightgreen"),
    locations = cells_body(rows = track_year == "2009" | track_year == "2010"))
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fxgtlmztzv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fxgtlmztzv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fxgtlmztzv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fxgtlmztzv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fxgtlmztzv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fxgtlmztzv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fxgtlmztzv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fxgtlmztzv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fxgtlmztzv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fxgtlmztzv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fxgtlmztzv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fxgtlmztzv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#fxgtlmztzv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fxgtlmztzv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fxgtlmztzv .gt_from_md > :first-child {
  margin-top: 0;
}

#fxgtlmztzv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fxgtlmztzv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fxgtlmztzv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#fxgtlmztzv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fxgtlmztzv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#fxgtlmztzv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fxgtlmztzv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fxgtlmztzv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fxgtlmztzv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fxgtlmztzv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#fxgtlmztzv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fxgtlmztzv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#fxgtlmztzv .gt_left {
  text-align: left;
}

#fxgtlmztzv .gt_center {
  text-align: center;
}

#fxgtlmztzv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fxgtlmztzv .gt_font_normal {
  font-weight: normal;
}

#fxgtlmztzv .gt_font_bold {
  font-weight: bold;
}

#fxgtlmztzv .gt_font_italic {
  font-style: italic;
}

#fxgtlmztzv .gt_super {
  font-size: 65%;
}

#fxgtlmztzv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="fxgtlmztzv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

playlist\_genre

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

track\_year

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

mean\_pop

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

count\_n

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2000

</td>

<td class="gt_row gt_right">

56.25000

</td>

<td class="gt_row gt_center">

4

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2001

</td>

<td class="gt_row gt_right">

45.00000

</td>

<td class="gt_row gt_center">

3

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2002

</td>

<td class="gt_row gt_right">

65.50000

</td>

<td class="gt_row gt_center">

2

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2003

</td>

<td class="gt_row gt_right">

40.33333

</td>

<td class="gt_row gt_center">

3

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2004

</td>

<td class="gt_row gt_right">

47.00000

</td>

<td class="gt_row gt_center">

3

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2005

</td>

<td class="gt_row gt_right">

41.00000

</td>

<td class="gt_row gt_center">

4

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2006

</td>

<td class="gt_row gt_right">

19.78571

</td>

<td class="gt_row gt_center">

14

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2007

</td>

<td class="gt_row gt_right">

15.71429

</td>

<td class="gt_row gt_center">

21

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2008

</td>

<td class="gt_row gt_right">

15.41463

</td>

<td class="gt_row gt_center">

41

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="background-color: #90EE90;">

edm

</td>

<td class="gt_row gt_right" style="background-color: #90EE90;">

2009

</td>

<td class="gt_row gt_right" style="background-color: #90EE90;">

16.70455

</td>

<td class="gt_row gt_center" style="background-color: #90EE90;">

44

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="background-color: #90EE90;">

edm

</td>

<td class="gt_row gt_right" style="background-color: #90EE90;">

2010

</td>

<td class="gt_row gt_right" style="background-color: #90EE90;">

35.20000

</td>

<td class="gt_row gt_center" style="background-color: #90EE90;">

35

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2011

</td>

<td class="gt_row gt_right">

34.06061

</td>

<td class="gt_row gt_center">

66

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2012

</td>

<td class="gt_row gt_right">

35.26752

</td>

<td class="gt_row gt_center">

157

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2013

</td>

<td class="gt_row gt_right">

19.49836

</td>

<td class="gt_row gt_center">

305

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2014

</td>

<td class="gt_row gt_right">

20.92143

</td>

<td class="gt_row gt_center">

560

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2015

</td>

<td class="gt_row gt_right">

26.34565

</td>

<td class="gt_row gt_center">

460

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2016

</td>

<td class="gt_row gt_right">

30.51411

</td>

<td class="gt_row gt_center">

496

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2017

</td>

<td class="gt_row gt_right">

33.43918

</td>

<td class="gt_row gt_center">

485

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2018

</td>

<td class="gt_row gt_right">

36.33377

</td>

<td class="gt_row gt_center">

755

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2019

</td>

<td class="gt_row gt_right">

43.26023

</td>

<td class="gt_row gt_center">

2321

</td>

</tr>

<tr>

<td class="gt_row gt_left">

edm

</td>

<td class="gt_row gt_right">

2020

</td>

<td class="gt_row gt_right">

40.31522

</td>

<td class="gt_row gt_center">

184

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

Did something happen in playlist\_genre of edm from 2009-2010 jumping 19
points?

``` r
spotify_songs_y %>%
  filter(playlist_genre == "edm", between(track_year, 2009,2010)) %>% 
  arrange(-track_popularity) %>% 
  select(track_year,everything()) %>% 
  head(10)
```

    ## # A tibble: 10 x 24
    ##    track_year track_id track_name track_artist track_popularity track_album_id
    ##         <dbl> <chr>    <chr>      <chr>                   <dbl> <chr>         
    ##  1       2010 15JINEq~ Love The ~ Eminem                     82 47BiFcV59TQi2~
    ##  2       2009 0SiywuO~ Bad Roman~ Lady Gaga                  78 6rePArBMb5nLW~
    ##  3       2010 2CEgGE6~ Dynamite   Taio Cruz                  77 0eGvq1J5Ke7Vl~
    ##  4       2010 1gv4xPa~ Raise You~ P!nk                       76 3uQMzfrf4kUNG~
    ##  5       2010 4lLtanY~ Grenade    Bruno Mars                 75 6J84szYCnMfzE~
    ##  6       2010 7GAaTpS~ Need You ~ Lady Antebe~               73 5RypFF6rN9MUx~
    ##  7       2010 0C4ejWm~ S&M        Rihanna                    72 7vN82vd1Vq44f~
    ##  8       2010 1bM50IN~ OMG (feat~ Usher                      72 6A1F3Fkq5dYeY~
    ##  9       2010 7ElF5zx~ Break You~ Taio Cruz                  71 0eGvq1J5Ke7Vl~
    ## 10       2010 1QnvpPF~ Airplanes~ B.o.B                      68 6f06neONZ0xqq~
    ## # ... with 18 more variables: track_album_name <chr>,
    ## #   track_album_release_date <chr>, playlist_name <chr>, playlist_id <chr>,
    ## #   playlist_genre <chr>, playlist_subgenre <chr>, danceability <dbl>,
    ## #   energy <dbl>, key <dbl>, loudness <dbl>, mode <dbl>, speechiness <dbl>,
    ## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
    ## #   tempo <dbl>, duration_ms <dbl>

9/10 songs dominant the top 10 songs, did something change?

``` r
spotify_songs_y %>% 
  filter(playlist_genre == "edm", track_year >= 2008) %>%
  select(danceability, valence, liveness, loudness, track_year) %>% 
  group_by(track_year) %>%
  summarise_at(vars(danceability, valence, liveness, loudness),list(mean = mean, sd = sd)) %>%
  right_join(spotify_pop, by = c("track_year" = "track_year")) %>% 
  drop_na() %>% 
  pivot_longer(cols = ends_with("mean"), names_to = "criteria", names_prefix = "_mean") %>% 
  pivot_longer(cols = ends_with("sd"), names_to = "criteria_sd", names_prefix = "_mean", values_to = "sd") %>% 
  ggplot(aes(x = track_year, y = value, color = criteria))+
  geom_line()
```

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

Not sure, doesn’t seem like a pattern.

## By Band

It’s time to move onto something else. Let’s look at the top bands. Top
= with the most count of records.

``` r
by_artist <- spotify_songs_y %>%
  group_by(track_artist) %>%
  filter(n() >= 80) %>% 
  select(track_artist, track_name, track_popularity) %>% 
  distinct(track_artist, track_name, .keep_all = TRUE)

by_artist_mean <- by_artist %>% 
  group_by(track_artist) %>% 
  summarise(popularity = mean(track_popularity)) %>% 
  mutate(track_name = "mean", .before = popularity,
        track_artist = fct_reorder(track_artist, popularity))

ggplot(by_artist, aes(y = track_artist, x = track_popularity))+
  geom_point()+
  geom_point(data = by_artist_mean, aes(y = track_artist, x = popularity), color = "red")+
    theme(
      panel.background = element_blank())
```

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

Hmm, again, we see some variance in between bands and artist, for
example Drake doesn’t have a song too close to the mean. Queen all in
all have an OK average compared to the rest but with a distribution
somewhat across it with classics ranked high.

Let’s turn to look at difference from mean and see if there’s a pattern
according to when the song was released.

``` r
by_artist_mean <- by_artist %>% 
  group_by(track_artist) %>% 
  summarise(popularity = mean(track_popularity)) %>% 
  top_n(7) %>% 
  pull(track_artist)

artist_songs <- spotify_songs %>% 
  filter(track_artist %in% by_artist_mean) %>% 
  mutate(track_date = as.Date(track_album_release_date, "%Y-%m-%d")) %>%
  distinct(track_id, .keep_all = TRUE) %>%
  arrange(track_date) %>%
  select(track_artist, track_name, track_popularity, track_date) %>% 
  add_count(track_artist) %>% 
  group_by(track_artist) %>% 
  mutate(song_seq = seq(1,length(track_name), 1),.before = track_popularity,
         mean_pop = mean(track_popularity),
         mean_diff = mean_pop - track_popularity,
         val = ifelse(mean_diff >0, "pos", "neg")) %>% 
  ungroup() %>% 
  mutate(track_artist = fct_reorder(track_artist, n))


ggplot(artist_songs, aes(x = song_seq, y = mean_diff, fill = val))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ track_artist, scales = "free", ncol = 1)+
  theme_minimal()+
  scale_fill_manual(values = c(pos = "#919c4c", neg = "#c03728"))+
  labs(title = "Band Song Popularity", subtitle = "Songs' difference from mean from the first to the last song released. Left side of X axis is the first\nsong released moving right in a chronological order, where the right side is the last song released.\nThe bands (according to avergae) out of those with 80 records were chosen\n",
    x = "Chronological order of songs", y = NULL)+
  theme(
    text = element_text(family = "Roboto Condensed", hjust = 0.5),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey50", hjust = 0.5),
    strip.text = element_text(size = 14, color = "grey25"),
    plot.background = element_rect(fill = "#F5F5F5"),
    axis.text = element_blank(),
    axis.title.x = element_text(size = 12, color = "grey50"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

ggsave("plot.png", height = 16, width = 12)
```

![](plot.png)

Not too bad with some things we saw previously such as Drake’s high
variance and Martin Garrix with also a large difference between his
first to last songs.
