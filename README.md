Cartographie sous <img src="logoR.png" style="width:0.3in" /> : méthodes
et outils
================
Journal Club - 15 mai 2023  
Gaspard Dubost, Vianney Guibourd de Luzinais, Romain Lécuyer & Thomas
Outrequin

## Réalisation de figures cartographiques

Carto à partir de shapefiles, rasters (préexistants ou créés
manuellement). Utilisation de base R, ggplot2, package central sf.

## Outils cartographiques externes

Logiciel de SIG (QGIS[^1]) et BDD géoréférencée (PostgreSQL[^2])

## Structure du *repository* GitHub

Le document de travail principal se présente sous la forme d’un fichier
RMarkdown structurant la présentation en sections : de la compréhension
des bases aux cas d’application concrets.

Les jeux de données et objets géoréférencés utilisés sont stockés dans
`/data`. Les fichiers de programmation exécutés sont stockés dans
`/script`.

## Principaux *packages* utilisés

<table>
<thead>
<tr>
<th style="text-align:left;">
Package
</th>
<th style="text-align:left;">
Version
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
dplyr
</td>
<td style="text-align:left;">
1.1.2
</td>
</tr>
<tr>
<td style="text-align:left;">
tidyr
</td>
<td style="text-align:left;">
1.3.0
</td>
</tr>
<tr>
<td style="text-align:left;">
ggplot2
</td>
<td style="text-align:left;">
3.4.2
</td>
</tr>
<tr>
<td style="text-align:left;">
sf
</td>
<td style="text-align:left;">
1.0.12
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border:0;" colspan="100%">
<sup></sup> Version logiciel R 4.2.3 (Shortstop Beagle) sous Windows 10
x64 (build 19045)
</td>
</tr>
</tfoot>
</table>

[^1]: installation gratuite sur <https://www.qgis.org/fr/site/>

[^2]: installation gratuite sur <https://www.postgresql.org/>
