## Macrorefugia Indices for Canadian Tree Species
###Data description
Velocity-based microrefugia metrics for 25 North American tree species were developed for two future time periods (2041-2070, 2071-2100) , four greenhouse gas emission scenarios (SSP1.26, SSP2.45, SSP3.70, and SSP5.85), and 13 global climate models (ACCESS-ESM1-5, BCC-CSM2-MR, CanESM5, CNRM-ESM2-1, EC-Earth3, GFDL-ESM4, GISS-E2-1-G, INM-CM5-0, IPSL-CM6A-LR, MIROC6, MPI-ESM1-2-HR, MRI-ESM2-0, and UKESM1-0-LL). based on species distribution model projections by Campell and Wang (2024) and using the approach described in Stralberg et al. (2018).
Each species distribution projection was reclassified using the threshold suggested by Zhao et al. (2023).
Backward biotic velocity (Carroll et al., 2015) for each species was calculated using the nearest-analog velocity algorithm defined by Hamann et al. 2015 and applied to binary presence/absence rasters representing current and projected future distributions. Presence thresholds were based on mean probability of occurrence in the baseline period. To convert biotic velocity into an index of microrefugia ranging from 0 to 1, a distance-decay function was applied to the backward velocity distance valuer at each pixel, i.e., the shortest distance from a projected future location to the current distribution. The distance-decay function was based on a fat-tailed distribution (c= 0.5, and alpha = 8333.33) parameterized to result in a mean migration rate of 500 m/year or 50 km/century (details in Stralberg et al. 2018). Refugia index values were calculated separately for each GCM and then averaged to produce an overall index.
Code (updated from Stralberg et al. 2018 is available: https://doi.org/10.5281/zenodo.14597790
Macrorefugia indices are provided as GeoTIFFs with a 1-km resolution, with map images provided in png format. 

###References
Carroll, C., Lawler, J. J., Roberts, D. R., & Hamann, A. (2015). Biotic and Climatic Velocity Identify Contrasting Areas of Vulnerability to Climate Change. PLOS ONE, 10(10), e0140486. https://doi.org/10.1371/journal.pone.0140486

Hamann, A., Roberts, D. R., Barber, Q. E., Carroll, C., & Nielsen, S. E. (2015). Velocity of climate change algorithms for guiding conservation and management. Global Change Biology, 21(2), 997–1004. https://doi.org/10.1111/gcb.12736

Stralberg, D., Carroll, C., Pedlar, J. H., Wilsey, C. B., McKenney, D. W., & Nielsen, S. E. (2018). Macrorefugia for North American trees and songbirds: Climatic limiting factors and multi-scale topographic influences. Global Ecology and Biogeography, 27(6), 690–703. https://doi.org/10.1111/geb.12731
Zhao, Y., O’Neill, G.A., and Wang, T. (2023). Predicting fundamental climate niches of forest trees based on species occurrence data. Ecological indicators, 148. https://doi.org/10.1016/j.ecolind.2023.110072 

