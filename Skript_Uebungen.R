# ┌─────────────────────────────────────────────────────────────┐
# │                                                             │
# │         Begleitendes R Script und Übungen zum Kurs          |
# |   "Grafiken mit ggplot2 - the grammer of graphics plots"    |
# |                     Regina Weber                            |
# │                                                             │
# │                                                             │
# │                                                             │
# |   Based on:                                                 |
# |   A Gentle Guide to the Grammar of Graphics with ggplot2    |
# |   by: Garrick Aden-Buie,http://bit.ly/gentle-ggplot2        │
# │                                                             │
# └─────────────────────────────────────────────────────────────┘

# ---- 0. Setup ----
# Kommentar entfernen, um die benötigten Pakete zu installieren
# install.packages("tidyverse") # beinhaltet u.a. ggplot2
# install.packages("gapminder") # gapminder Daten

library(tidyverse)
library(gapminder)

# ---- 1. Grafiken: Beispiele und Grundlagen ----
## ---- guess-data-from-plot ----
# Daten zu Sprit-Verbrauch von Automobilen, zwei Typen und drei Hersteller
df_mpg <- mpg %>% 
  filter(class %in% c("compact", "suv")) %>% 
  filter(manufacturer %in% c("toyota", "ford", "audi")) %>% 
  filter(year == 2008) %>% 
  group_by(manufacturer, model) %>% 
  sample_n(1)

head(df_mpg)

# g wird als einfacher Plot definiert
g <- ggplot(df_mpg) + # Daten festlegen
  aes(x = cty, y = hwy, color = class, shape = manufacturer) + # Aesthetics festlegen
  geom_point(size = 4) + # geometrische Figur festlegen (und Größe manuell definieren)
  labs(x = NULL, y = NULL, shape = NULL, color = NULL) + # Bezeichnung der Achsen festlegen
  theme_minimal() + # Thema zuweisen und individuell anpassen
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = rgb(250, 250, 250, max = 255),
                                   color = "#777777")
  )

# Minimalistischer Plot: Nur Punkte in Farben, keine Legende und Achsen
g + 
  guides(color = FALSE, shape = FALSE) +
  theme(axis.text = element_blank())

# Finaler Plot (Zwischenstufen durch hinzufügen der einzelnen Code-Zeilen)
## ---- Übung 0: Diesen Code Schritt für Schritt aufbauen ----

g + 
  ggtitle("MPG Ratings") +
  labs(x = "City", y = "Highway", shape = "Manufacturer", color = "Class") +
  theme(
    panel.border = element_rect(fill = NA, color = "grey85"),
    panel.grid.major = element_line(color = "grey90")
  )

## ---- tidy-messy1 ----
# Ausschnitt aus den Gapminder-Daten in der Messy-Version 
pop_simple <- gapminder %>% 
  filter(
    country %in% c("Canada", "China", "United States"), 
    year >= 1997
  ) %>% 
  select(country, year, pop) %>% 
  mutate(pop = pop / 10^6)

messy_pop <- pop_simple %>% spread(year, pop)

head(messy_pop)
# knitr::kable(messy_pop, format = 'html')

## ---- tidy-tidy ----
# Gapminder-Daten als Tidy-Data
tidy_pop <- gather(messy_pop, 'year', 'pop', -country)

head(tidy_pop)


## ---- geom-demo ----
# Beispiele für geometrische Objekte, gezeigt an generierten Zufallsdaten
minimal_theme <- theme_bw() + # definieren des minimal themes für diese Darstellung
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

set.seed(4242)
df_geom <- data_frame(y = rnorm(10), x = 1:10)

g_geom <- list()
g_geom$point <- ggplot(df_geom, aes(x, y)) + geom_point() + ggtitle("geom_point")
g_geom$line <- ggplot(df_geom, aes(x, y)) + geom_line() + ggtitle("geom_line")
g_geom$bar <- ggplot(df_geom, aes(x, y)) + geom_col() + ggtitle("geom_bar")
g_geom$boxplot <- ggplot(df_geom, aes(y = y)) + geom_boxplot() + ggtitle("geom_boxplot") + labs(x = "x")

g_geom <- map(g_geom, ~ . + minimal_theme)

g_geom

# --- 2. Schritt für Schritt-Aufbau einer Grafik ---- 
# ---- grammar-of-graphics ----
tidy_pop <- left_join(tidy_pop, select(gapminder, country, continent))

# Einfache Grafik mit zwei geometrischen Objekten
ggplot(tidy_pop) + # Daten definieren
  aes(x = year, # aesthetics definieren
      y = pop,
      color = country) +
  geom_point() + # geometrisches Objekt definieren
  geom_line(aes(group = country)) # zweites geometrisches Objekt definieren

# diese Grafik speichern, um sie mit weiteren Elementen zu erweitern 
g <- ggplot(tidy_pop) +
  aes(x = year,
      y = pop,
      color = country) +
  geom_point() +
  geom_line(aes(group = country))

# Verschiedene Modifikationen 
## ---- Übung 1: Modifikationen ausprobieren und eigene durchführen ---- 
g + facet_wrap(~ country)

g + facet_grid(continent ~ country)

g + labs(x = "Year", y = "Population")

g + coord_flip()

g + coord_polar()

g + scale_color_manual(values = c("peru", "pink", "plum"))

g + scale_y_log10()

g + scale_x_discrete(labels = c("MCMXCVII", "MMII", "MMVII"))

g + theme_bw()

g + theme_minimal() + theme(text = element_text(family = "Palatino"))

my_theme <- theme_bw() +
  theme(
    text = element_text(family = "Palatino", size = 12),
    panel.border = element_rect(colour = 'grey80'), 
    panel.grid.minor = element_blank()
  )

# theme_set(my_theme) # globales Setzen des Themes
g + my_theme # einmaliges Nutzen des Themes

g + my_theme + theme(legend.position = 'bottom')

# ---- Übung 2: Grafiken mit gapminder Daten aufbauen ---- 
# Alternative Übung: GDP und Population-Zusammenhänge grafisch darstellen

head(gapminder)
summary(gapminder)

## ----gapminder-le-gdp-1 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp)

## ----gapminder-le-gdp-2 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp) +
  geom_point() #<<

## ----gapminder-le-gdp-3 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) + # points: Color
  geom_point()

## ----gapminder-le-gdp-4 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_point() +
  scale_x_log10() # log skalierte X-Achse

## ----gapminder-le-gdp-5 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ continent) + # nach Kontinenten aufgeteilt
  guides(color = FALSE)     # keine Legende

## ----gapminder-le-gdp-6 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_point(size = 0.25) + # Punkte verkleinern
  scale_x_log10() +
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-gdp-7 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_line() + # geom_line als Alternative? 
  geom_point(size = 0.25) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-gdp-8 ----
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country) # geom_line gruppiert nach Ländern?
  ) +
  geom_point(size = 0.25) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-gdp-year-1 ----
ggplot(gapminder) +
  aes(x = year, # neue Variable auf der x-Achse
      y = gdpPercap, # neue Variable auf der y-Achse
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  scale_y_log10() + # logarithmische y-Achse
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-gdp-year-2 ----
ggplot(gapminder) +
  aes(x = year,
      y = gdpPercap,
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(1950, 2000, 25)) + # x-Achse in 25er Schritten
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-1 ----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp, # neue Variable auf der y-Achse
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  #scale_y_log10() + # nicht mehr logarithmierte y-Achse
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-2 ----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  #geom_smooth() + # smoothing Line (Regressionslinie) 
  geom_smooth(method = lm, level=.99) + # level kontrolliert das Konfidenzintervall
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-3 -----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75" # Verlausfslinien in grau
  ) +
  geom_point(size = 0.25) +
  geom_smooth() + 
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-4 -----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  #geom_point(size = 0.25) + # ohne Punkte
  geom_smooth() + 
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-5 ----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() #+ 
  # scale_x_continuous(
  #   breaks = 
  #     seq(1950, 2000, 25)
  # ) +  
  # facet_wrap(~ continent) + # alle Kontinente in einer Grafik
  # guides(color = FALSE)     # Mit Legende nd vollständiger x-Achse

## ----gapminder-le-year-6 ----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  theme(legend.position = "bottom") # Legende unten

## ----gapminder-le-year-7 ----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  theme_minimal() + # anderes Theme
  theme(legend.position = "bottom")

## ----gapminder-le-year-8 ----
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  theme_minimal( 
    base_size = 8) + # kleinere Schrift
  theme(
  legend.position = "bottom"
  )

# --- Übung 3: Americas Grafik mit Gapminder Daten ---- 
# Abgewandelte Übung aus der Präsentation (andere Länder)
## ----gapminder-americas-data 
americas <- 
  gapminder %>% 
  filter(
    country %in% c(
      "United States",
      "Canada",
      "Mexico",
      "Ecuador"
    )
  )

head(americas)

## ----gapminder-americas-1 ----
ggplot(americas) +
  aes(
    x = year,
    y = pop
  ) +
  geom_col()

## ----gapminder-americas-2 ----
ggplot(americas) +
  aes(
    x = year,
    y = pop,
    fill = country # pro Land mit Farbe füllen
  ) +
  geom_col()

## ----gapminder-americas-3 ----
ggplot(americas) +
  aes(
    x = year,
    y = pop,
    fill = country
  ) +
  geom_col(
    position = "dodge" # Nebeneinander
  )

## ----gapminder-americas-4 ----
ggplot(americas) +
  aes(
    x = year,
    y = pop / 10^6, # Notation
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  )

## ----gapminder-americas-5 ----
ggplot(americas) +
  aes(
    x = year,
    y = pop / 10^6,
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  ) +
  facet_wrap(~ country) + # eine Grafik pro Land
  guides(fill = FALSE) # keine Legenden

## ----gapminder-americas-6 ----
ggplot(americas) +
  aes(
    x = year,
    y = pop / 10^6,
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  ) +
  facet_wrap(~ country,
    scales = "free_y") + # unterschiedliche Achsen 
  guides(fill = FALSE)

## ----gapminder-americas-7 ----
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp, # andere Variable auf der y-Achse
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  ) +
  facet_wrap(~ country,
    scales = "free_y") +
  guides(fill = FALSE)

## ----gapminder-americas-8 ----
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp,
    fill = country
  ) +
  geom_line() + # Linie passt besser
  facet_wrap(~ country,
    scales = "free_y") +
  guides(fill = FALSE)

## ----gapminder-americas-9 ----
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp,
    color = country # Farblegende, Achtung: color vs. fill!
  ) +
  geom_line() +
  facet_wrap(~ country,
    scales = "free_y") +
  guides(color = FALSE) # Legende muss ebenfalls geändert werden

## ----gapminder-americas-10 ----
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp,
    color = country
  ) +
  geom_line()

# ---- Übung 4: Alle amerikanischen Länder ----
## ----gapminder-all-americas-1  ----
gapminder %>% 
  filter(
    continent == "Americas"
  ) %>% # 'pipen' funktioniert auch innerhalb von ggplot2
  ggplot() + # Aufruf ohne Daten
  aes(
    x = year,
    y = lifeExp
  )

## ----gapminder-all-americas-2  ----
gapminder %>% 
  filter(
    continent == "Americas"
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = lifeExp
  ) +
  geom_boxplot() # einfacher Boxplot

## ----gapminder-all-americas-3  ----
gapminder %>% 
  filter(
    continent == "Americas"
  ) %>%
  mutate(
    year = factor(year) # Boxplot pro Jahr möglich, wenn year eine Faktorvariable
  ) %>%  
  ggplot() +
  aes(
    x = year,
    y = lifeExp
  ) +
  geom_boxplot()

## ----gapminder-all-americas-4  ----
gapminder %>% # Was wird hier angezeigt?
  # filter(
  #   continent == "Americas"
  # ) %>%
  mutate(
    year = factor(year)
  ) %>% 
  ggplot() +
  aes(
    x = year,
    y = lifeExp
  ) +
  geom_boxplot()

## ----gapminder-all-americas-5  ----
gapminder %>% 
  mutate(
    year = factor(year)
  ) %>% 
  ggplot() +
  aes(
    x = year,
    y = lifeExp,
    fill = continent # nach Kontinenten
  ) +
  geom_boxplot()

## ----gapminder-all-americas-6  ----
gapminder %>% 
  mutate(
    year = factor(year)
  ) %>% 
  ggplot() +
  aes(
    x = year,
    y = lifeExp,
    fill = continent
  ) +
  geom_boxplot() +
  coord_flip() # 90 Grad-Drehung

## ----gapminder-all-americas-7  ----
gapminder %>% 
  mutate(
    decade = floor(year / 10), # Dekaden berechnen
    decade = decade * 10,      # und als neue x-Achse nutzen
    decade = factor(decade)    # (Achtung, coord_flip!)
  ) %>% 
  ggplot() +
  aes(
    x = decade, 
    y = lifeExp,
    fill = continent
  ) +
  geom_boxplot() +
  coord_flip()

## ----gapminder-all-americas-8 -----
g <- gapminder %>% 
  filter( #<<
    continent != "Oceania" # ohne Ozeanien
  ) %>% #<<
  mutate(
    decade = floor(year / 10) * 10, decade = factor(decade)      
  ) %>% 
  ggplot() +
  aes(
    x = decade,
    y = lifeExp,
    fill = continent
  ) +
  geom_boxplot() +
  coord_flip()

g # Grafik gespeichert in g, wird erst jetzt angezeigt

## ----gapminder-all-americas-9 ----
g +
  theme_minimal(8) +
  labs(
    x = "Life Expectancy",
    y = "Decade",
    fill = NULL,
    title = "Life Expectancy by Continent and Decade",
    caption = "gapminder.org"
  )

# Übung 5: Hans Rosling Grafik ----
## ----hans-rosling-1 ----
g_hr <- 
  ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  facet_wrap(~year)
g_hr # unsinnige Legende

## ----hans-rosling-1a ----
g_hr <- 
  ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  facet_wrap(~year) +
  guides(color = FALSE, size = FALSE)
g_hr

## ----hans-rosling-2 ----
g_hr <- 
  g_hr + # basierend auf dem Vorherigen plus optische Erweiterung
  scale_x_log10(breaks = c(10^3, 10^4, 10^5), labels = c("1k", "10k", "100k")) +
  scale_color_manual(values = gapminder::country_colors) +
  scale_size(range = c(0.5, 12))
  
g_hr

## ----hans-rosling-3 ----
g_hr <- 
  g_hr + # weitere optische Erweiterungen
  labs(
    x = "GDP per capita",
    y = "Life Expectancy"
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(fill = NA, color = "grey40"),
    panel.grid.minor = element_blank()
  )
g_hr

## ----hans-rosling-final -----
# Der komplette Code. Vergleiche oben und hier. Was ist guter Code?
ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  facet_wrap(~year) +
  guides(color = FALSE, size = FALSE) +
  scale_x_log10(
    breaks = c(10^3, 10^4, 10^5), 
    labels = c("1k", "10k", "100k")) +
  scale_color_manual(values = gapminder::country_colors) +
  scale_size(range = c(0.5, 12)) +
  labs(
    x = "GDP per capita",
    y = "Life Expectancy") +
  theme_minimal(14, base_family = "Fira Sans") +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(fill = NA, color = "grey40"),
    panel.grid.minor = element_blank())


# ---- Weitere Daten zum Üben, Testen und weiter Arbeiten 
# library(fivethirtyeight)
# library(nycflights)
# library(ggplot2movies)
# library(tidyr) # `population` und `who` Datasets


# ---- Authorship ----
# Original Author: 
# Garrick Aden-Buie, garrickadenbuie.com, Twitter: @grrrck
# Slides and code on GitHub: http://github.com/gadenbuie/gentle-ggplot2
# 
# Modifikations and translations:
# Regina Weber, reginaweber.github.io, Twitter: @elisasdottir