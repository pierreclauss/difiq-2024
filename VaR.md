Risques extrêmes et application à la mesure du risque de marché
================
Pierre Clauss
Mars 2024

*Ce document R Markdown a pour objet la résolution des exercices 1.1 et
2.1 du cours.*

## Préambule

Je précise en préambule les 3 étapes nécessaires pour la réussite d’un
projet de data science :

1.  données : (i) importation, (ii) wrangling et (iii) visualisation (ou
    appelée encore *analyse exploratoire des données*)
2.  modélisation
3.  communication des résultats

L’univers du package **tidyverse** est essentiel pour réaliser ces 3
étapes avec R aujourd’hui.

``` r
library(tidyverse)
```

## 1 Données

### 1.1 Importation

Pour les 2 exercices, les données sont les mêmes. J’importe ces données
à l’aide du package
[**tidyquant**](https://business-science.github.io/tidyquant/) qui est
très performant pour importer entre autres des data financières. Je les
importe de [*Yahoo
Finance*](https://fr.finance.yahoo.com/indices-mondiaux) et plus
particulièrement les indices actions CAC 40, NASDAQ Composite, Nikkei
225 et SMI. L’échantillon commence en janvier 1990 et se termine en mars
2023. Je transforme alors ces données en rentabilités.

``` r
library(tidyquant)
symbols <- c("^FCHI", "^IXIC", "^N225", "^SSMI")
stock_prices <- symbols %>%
  tq_get(get  = "stock.prices",
         from = "1990-01-03",
         to   = "2023-04-01") %>%
  group_by(symbol)

(stock_prices %>% slice(1, n()))
```

    ## # A tibble: 8 × 8
    ## # Groups:   symbol [4]
    ##   symbol date         open   high    low  close     volume adjusted
    ##   <chr>  <date>      <dbl>  <dbl>  <dbl>  <dbl>      <dbl>    <dbl>
    ## 1 ^FCHI  1990-03-01  1836   1838   1827   1832           0    1832 
    ## 2 ^FCHI  2023-03-31  7271.  7342.  7265.  7322.   84335300    7322.
    ## 3 ^IXIC  1990-01-03   461.   462.   460    461.  152660000     461.
    ## 4 ^IXIC  2023-03-31 12032. 12228. 12030. 12222. 5521150000   12222.
    ## 5 ^N225  1990-01-04 38922. 38951. 38705. 38713.          0   38713.
    ## 6 ^N225  2023-03-31 28009. 28125. 27987. 28041.   91100000   28041.
    ## 7 ^SSMI  1990-11-09  1379.  1389   1375.  1387.          0    1387.
    ## 8 ^SSMI  2023-03-31 11047. 11116. 11042. 11106.   56883100   11106.

``` r
# plot sur données mensuelles
monthly_prices <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = to.monthly,
               indexAt = "lastof")

monthly_prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 0.25) +
  labs(
    title = "Stock Prices",
    x = "Date",
    y = "Adjusted Prices",
    color = ""
  ) +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() +
  scale_color_tq()
```

![](VaR_files/figure-gfm/importation-1.png)<!-- -->

``` r
daily_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    type       = "arithmetic",
    col_rename = "dreturns"
  )
```

### 1.2 Démêlage (wrangling en anglais)

“Tidying and transforming are called *wrangling*, because getting your
data in a form that’s natural to work with often feels like a fight”
[**R for Data Science**](https://r4ds.had.co.nz/introduction.html)
(Grolemund G. and Wickham H.).

Je peux à l’aide du package **DataExplorer** obtenir un résumé des
données et évaluer si je peux les considérer comme **tidy**.

``` r
library(DataExplorer)
plot_intro(daily_returns)
```

![](VaR_files/figure-gfm/wrangling-1.png)<!-- -->

### 1.3 Visualisation

Les statistiques de base sont résumées par le tableau et les graphiques
ci-dessous. Nous pouvons observer un fait stylisé très important des
rentabilités d’indices de marché, à savoir la leptokurticité de leur
densité.

``` r
daily_returns %>%
  group_by(symbol) %>%
  summarise(moyenne = mean(dreturns),
            ecartype = sd(dreturns),
            nombre = n(),
            min = min(dreturns),
            max = max(dreturns)
  )
```

    ## # A tibble: 4 × 6
    ##   symbol   moyenne ecartype nombre     min   max
    ##   <chr>      <dbl>    <dbl>  <int>   <dbl> <dbl>
    ## 1 ^FCHI  0.000258    0.0137   8400 -0.123  0.112
    ## 2 ^IXIC  0.000500    0.0147   8376 -0.123  0.142
    ## 3 ^N225  0.0000702   0.0148   8161 -0.114  0.142
    ## 4 ^SSMI  0.000318    0.0112   8139 -0.0964 0.114

``` r
daily_returns %>%
  ggplot(aes(x = dreturns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densités des rentabilités arithmétiques",
       x = "Rentabilités quotidiennes", y = "Densité") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)
```

![](VaR_files/figure-gfm/viz%20data-1.png)<!-- -->

``` r
daily_returns %>%
  ggplot(aes(sample = dreturns, colour = factor(symbol))) +
  stat_qq() +
  stat_qq_line() +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)
```

![](VaR_files/figure-gfm/viz%20data-2.png)<!-- -->

## 2 Modélisation

Les VaR sont soit non-paramétriques (historique et bootstrap) soit
paramétriques (Gaussienne, Skew Student, GEV et GPD).

### 2.1 Résolution de *l’exercice 1.1* du cours

``` r
library(scales)
library(sn)

VaR_classiques <- function(data, alpha, boot = 100)
{
  #VaR Historique
  Hist <- quantile(data, probs = alpha)
  Hist <- percent(Hist, 0.01)
  
  #VaR Bootstrap
  x <- numeric(boot)
  for (j in 1:boot)
  {
    databoot <- sample(data, replace = T)
    x[j] <- quantile(databoot, probs = alpha)
  }
  Boot <- mean(x)
  Boot <- percent(Boot, 0.01)
  
  # VaR Gaussienne
  Gauss <- mean(data) + sd(data) * qnorm(alpha, 0, 1)
  Gauss <- percent(Gauss, 0.01)
  
  # VaR skew Student
  esti <- st.mple(y = data)
  Skt <- qst(alpha, esti$dp["xi"], esti$dp["omega"], esti$dp["alpha"], esti$dp["nu"])
  Skt <- percent(Skt, 0.01)
  
  tibble(
    Historique = Hist,
    Bootstrap = Boot,
    Gaussienne = Gauss,
    Skew_Student = Skt
  )
}
```

Voici ci-dessous les VaR demandées dans *l’exercice 1.1* pour les
différents indices avec alpha = 1%.

``` r
library(pander)
VaR_1 <- daily_returns %>%
  group_by(symbol) %>%
  reframe(VaR_classiques(dreturns, alpha = 0.01))
pander(VaR_1)
```

| symbol | Historique | Bootstrap | Gaussienne | Skew_Student |
|:------:|:----------:|:---------:|:----------:|:------------:|
| ^FCHI  |   -3.94%   |  -3.89%   |   -3.15%   |    -4.46%    |
| ^IXIC  |   -4.11%   |  -4.11%   |   -3.37%   |    -6.86%    |
| ^N225  |   -3.94%   |  -3.93%   |   -3.44%   |    -5.39%    |
| ^SSMI  |   -3.21%   |  -3.24%   |   -2.57%   |    -3.58%    |

Voici ci-dessous les VaR demandées dans *l’exercice 1.1* pour les
différents indices avec alpha = 0.1%.

``` r
VaR_01 <- daily_returns %>%
  group_by(symbol) %>%
  reframe(VaR_classiques(dreturns, alpha = 0.001))
pander(VaR_01)
```

| symbol | Historique | Bootstrap | Gaussienne | Skew_Student |
|:------:|:----------:|:---------:|:----------:|:------------:|
| ^FCHI  |   -6.64%   |  -6.50%   |   -4.20%   |   -11.29%    |
| ^IXIC  |   -7.27%   |  -7.57%   |   -4.50%   |   -25.57%    |
| ^N225  |   -6.87%   |  -6.99%   |   -4.57%   |   -14.66%    |
| ^SSMI  |   -5.57%   |  -5.64%   |   -3.43%   |    -8.13%    |

### 2.2 Résolution de *l’exercice 2.1* du cours

Pour définir la VaR GPD, il est nécessaire de déterminer le seuil à
partir duquel il est raisonnable de penser que les extrêmes suivent une
loi GPD. Cela se fait grâce au mean-excess plot : le seuil optimal est
la valeur à partir de laquelle la tendance est croissante.

``` r
library(evir)

layout(matrix(1:4,2,2))

invisible(daily_returns %>%
            filter(symbol == '^FCHI') %>%
            reframe(meplot(-dreturns[dreturns < 0])))

invisible(daily_returns %>%
            filter(symbol == '^IXIC') %>%
            reframe(meplot(-dreturns[dreturns < 0])))

invisible(daily_returns %>%
            filter(symbol == '^N225') %>%
            reframe(meplot(-dreturns[dreturns < 0])))

invisible(daily_returns %>%
            filter(symbol == '^SSMI') %>%
            reframe(meplot(-dreturns[dreturns < 0])))
```

![](VaR_files/figure-gfm/var%20TVE-1.png)<!-- -->

``` r
VaR_TVE <- function(data,
                    alpha,
                    bloc = 21,
                    seuil = 0.01)
{
  # VaR GEV
  g1 <- gev(-data, bloc)
  alphaGEV <- 1 - bloc * alpha
  GEV <-
    -qgev(alphaGEV, g1$par.ests["xi"], g1$par.ests["mu"], g1$par.ests["sigma"])
  GEV <- percent(GEV, 0.01)
  
  # VaR GPD
  g2 <- gpd(-data, seuil)
  p <- length(g2$data) / length(data)
  GPD <- -qgpd(1 - alpha / p, g2$par.ests["xi"], seuil, g2$par.ests["beta"])
  GPD <- percent(GPD, 0.01)
  
  tibble(
    GEV = GEV, 
    GPD = GPD)
}
```

Voici ci-dessous les VaR TVE demandées dans *l’exercice 2.1* pour les
différents indices avec alpha = 1%.

``` r
VaR_1 <- daily_returns %>%
  group_by(symbol) %>%
  reframe(VaR_TVE(dreturns, alpha = 0.01, seuil = 0.03))
pander(VaR_1)
```

| symbol |  GEV   |  GPD   |
|:------:|:------:|:------:|
| ^FCHI  | -3.11% | -3.85% |
| ^IXIC  | -3.20% | -4.14% |
| ^N225  | -3.43% | -3.94% |
| ^SSMI  | -2.49% | -3.22% |

Voici ci-dessous les VaR TVE demandées dans *l’exercice 2.1* pour les
différents indices avec alpha = 0.1%.

``` r
VaR_01 <- daily_returns %>%
  group_by(symbol) %>%
  reframe(VaR_TVE(dreturns, alpha = 0.001, seuil = 0.03))
pander(VaR_01)
```

| symbol |  GEV   |  GPD   |
|:------:|:------:|:------:|
| ^FCHI  | -6.11% | -6.69% |
| ^IXIC  | -7.02% | -7.36% |
| ^N225  | -6.43% | -7.21% |
| ^SSMI  | -5.50% | -5.97% |
