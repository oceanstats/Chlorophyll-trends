# Chlorophyll-trends
Main code for the GBC paper "Assessing trends and uncertainties in satellite-era ocean chlorophyll using space-time modeling

https://doi.org/10.5281/zenodo.804404

Script 1 formats the matlab input for files for use in the individual Longhurst regions as read by spTimer

Script 2 was designed for a compute server and has the different regions being run in parallel with the MCMC  separated into individual sections of 1000 iterations, with the next chain running from where the last chain finished. Parallel is used to allow for the slow computation which is particularly obvious in larger regions. With the MCMC being run in sections to avoid memory constraints. 

Script 3 combines the subdivided chains.

Script 4 provides tabulation of the results and regional time series

Script 5 reads this tabulation for the distribution and bar chart plots

Script 6 again reads this tabulation for global plots


spT_simple is included as a simple script based off this code that can be more easily adapted for other usage and testing.
