``` r
# Changement du thème d’un graphique en utilisant un thème prédéfini
library(ggthemes)
library(ggplot2)
infos <- data.frame(
    name = c("Sherlock Holmes", "Dr. Watson", "Inspector Lestrade", "Mrs. Hudson", "Mycroft Holmes", "Irene Adler", "Professor Moriarty", "Mary Morstan", "James Phillimore", "Charles Augustus Milverton"),
    residence = c("221B Baker Street", "221B Baker Street", "Scotland Yard", "221B Baker Street", "Diogenes Club", "Briony Lodge", "Unknown", "Unknown", "Unknown", "Unknown"),
    weapon = c("Mind", "Force", "Force", "Mind", "Mind", "Mind", "Mind", "Mind", "Mind", "Mind"),
    motive = c("Justice", "Justice", "Justice", "Love", "Justice", "Love", "Folie", "Justice", "Money", "Money"),
    outcome = c("Vivant", "Vivant", "Vivant", "Vivant", "Vivant", "Vivant", "Vivant", "Vivant", "Vivant", "Vivant"),
    notes = c("Le seul détective consultant au monde", "Le meilleur ami et colocataire de Sherlock Holmes", "Le meilleur détective de Scotland Yard", "La logeuse de Sherlock Holmes", "Le frère aîné de Sherlock Holmes", "Le seul amour de Sherlock Holmes", "La némésis de Sherlock Holmes", "La femme de Sherlock Holmes", "Le client de Sherlock Holmes", "L'ennemi de Sherlock Holmes")
)
ggplot(infos) +
    aes(x = name, y = outcome) +
    geom_bar(stat = "identity") +
    facet_wrap(~weapon) +
    theme_map()
```

![](07-06-facette-personnalisation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# https://jrnold.github.io/ggthemes/
# https://github.com/jrnold/ggthemes/
library(ggthemes)
library(ggrepel)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(dslabs)
data(murders)
r <- murders %>%
    summarize(rate = sum(total) / sum(population) * 10^6) %>%
    pull(rate)
murders |> ggplot(aes(population / 10^6, total, label = abb)) +
    geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
    geom_point(aes(col = region), size = 3) +
    # geom_text_repel() +
    # scale_x_log10() +
    # scale_y_log10() +
    xlab("Populations en millions") +
    ylab("Nombre de meurtres") +
    ggtitle("Meurtres par armes à feu aux États-Unis en 2010") +
    scale_color_discrete(name = "Region") +
    theme_economist()
```

![](07-06-facette-personnalisation_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->
