``` r
# Découverte des exports de graphiques en mode manuel ou automatique
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ───────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggthemes)
library(ggrepel)
library(dslabs)
data(murders)
r <- murders %>%
    summarize(rate = sum(total) / sum(population) * 10^6) %>%
    pull(rate)
plot <- ggplot(murders, aes(population / 10^6, total, label = abb)) +
    geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
    geom_point(aes(col = region), size = 3) +
    geom_text_repel() +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Populations en millions") +
    ylab("Nombre de meurtres") +
    ggtitle("Meurtres par armes à feu aux États-Unis en 2010") +
    scale_color_discrete(name = "Region") +
    theme_economist()

## Automatique : Bouton Export
## Manuel
### Exporter les graphiques en png :
ggsave("mon_graph.png", plot, width = 10, height = 10, units = "in")

### Exporter les graphiques en pdf :
pdf("mon_graph.pdf")
plot
dev.off()
```

    ## png 
    ##   2
