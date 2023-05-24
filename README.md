Cartographie sous <img src="data/logo_R.png" style="width:0.4in" /> :
méthodes et outils
================
Journal Club - 24 mai 2023  
Gaspard Dubost, Vianney Guibourd de Luzinais, Romain Lécuyer & Thomas
Outrequin

### Contenu du dépôt

Ce dépôt contient différents scripts permettant de réaliser des
représentations cartographiques par divers moyens : `R base`, `ggplot2`,
`plotly`, application `Shiny`. L’intérêt de ce dépôt est de partager des
scripts opérationnels que chacun peut adapter à son cas précis pour
générer une carte sous R.

Les cartes combinent des objets géoréférencés comme des shapefiles ou
des rasters, téléchargés depuis Internet, créés à la main sous un
logiciel de SIG (comme QGIS[^1]) ou stockés dans une base de données
géoréférencées (comme PostgreSQL[^2]) et des données issues d’analyses
dans lesquelles les variables spatiales occupent une place importante et
représentées par divers figurés (points, lignes, polygones de taille ou
encore de couleur variables).

### Structure du dépôt

Le document principal se présente sous la forme d’un fichier HTML
`Presentation.html` structurant la présentation en sections : de la
compréhension des bases aux cas d’application concrets. Une partie des
scripts de cartographie y figure sous forme de chunks.

Les jeux de données et objets géoréférencés publics utilisés sont
stockés dans le dossier `/data`. Les fichiers de programmation amenés à
être explorés sont stockés dans le dossier `/script`.

Les sections présentées sont les suivantes :

- Introduction à la cartographie sous R (préfixe des fichiers associés
  “Loire\_”)
- Création de cartes avancées avec `ggplot2` et `plotly` (“Peche\_”)
- Connexion à une base de données géospatialisées avec `RPostgreSQL`
  (“MHW\_”)
- Exploration interactive *via* l’application `Shiny` (“MHW\_” &
  “Alose\_”)

### Principaux *packages* utilisés

| Package     | Version |
|:------------|:--------|
| dplyr       | 1.1.2   |
| sf          | 1.0.12  |
| ggplot2     | 3.4.2   |
| plotly      | 4.10.1  |
| leaflet     | 2.1.2   |
| RPostgreSQL | 0.7.5   |
| shiny       | 1.7.4   |

**Note:** ^^ Version logiciel R 4.2.3 (Shortstop Beagle) sous Windows 10
x64 (build 19045)

[^1]: installation gratuite sur <https://www.qgis.org/fr/site/>

[^2]: installation gratuite sur <https://www.postgresql.org/>
