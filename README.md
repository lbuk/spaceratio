# spaceratio

### Overview
spaceratio is an R package for charting density potentials in the built environment using <a href="https://www.liamthomasbolton.com/portfolio/SpaceRatio/">Space Ratio</a>, or the ratio of the existing density to the permissible density, and the Space Ratio Chart (Bolton, 2021). 

For more information about Space Ratio, you can read this <a href="https://discovery.ucl.ac.uk/id/eprint/10156128/">journal article</a> published in <a href="https://www.sciencedirect.com/journal/sustainable-cities-and-society">Sustainable Cities and Society</a>.

### Installation
spaceratio can be installed from Github.
```
library(devtools)
install_github("lbuk/spaceratio")
```

### Use
```
library(spaceratio)

## Example Scenario

# Density variables
d_var = c("FAR", "H", "DPH", "GSI")

# Space Ratios
s_val = c(0.75, 0.8, 0.95, 0.91)

# Space Ratio Chart
spaceratio(density_var = d_var, space_ratio = s_val, output = 'plot')
```
![](https://github.com/lbuk/spaceratio/blob/master/img/spaceratio_chart.png)

### References
Bolton, L.T. (2021). 'Space ratio: a measure of density potentials in the built environment', _Sustainable Cities and Society_, 75, p.103356. doi: 10.1016/j.scs.2021.103356.