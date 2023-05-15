Journal Club - Cartographie sous R : méthodes et outils
================
Gaspard Dubost, Vianney Guibourd de Luzinais, Romain Lécuyer et Thomas
Outrequin
15 mai 2023

# Réalisation de figures cartographiques sous R

Carto à partir de shapefiles, raster … Utilisation de base R, ggplot2 …

# Outils cartographiques en parallèle de R

QGIS (ArcGIS)[^1] BDD géoréférencée (PostgreSQL)[^2]

# Structure du repository GitHub

## Données (`/data`)

## Programmation (`/scripts`)

# Principaux *packages* R utilisés

All packages used throughout the workflow are stored in the R folder.
They are to be loaded using R version 4.2.3 (2023-03-15 ucrt) running on
Windows 10 x64 (build 19045).

| Package | Version |
|:--------|:--------|
| dplyr   | 1.1.2   |
| tidyr   | 1.3.0   |
| sf      | 1.0.12  |

[^1]: installation gratuite sur <https://www.qgis.org/fr/site/>

[^2]: installation gratuite sur <https://www.postgresql.org/>
