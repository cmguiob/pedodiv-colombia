## Análisis geoespacial de pedodiversidad y diversidad geomorfológica en Colombia

![estado](https://img.shields.io/badge/estado-en_progreso-lightgrey&?style=for-the-badge&color=%23EEC900) ![made-with-R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)

--

**Este repositorio presenta un *análisis geoespacial de pedodiversidad* (i.e. diversidad de suelos) y la *diversidad geomorfológica* en Colombia.**

La pedodiversidad se ha relacionado a la biodiversidad ([Ibañez & Feoli, 2013](https://acsess.onlinelibrary.wiley.com/doi/epdf/10.2136/vzj2012.0186)) y a otras expresiones de diversidad de los elementos abióticos que componen la tierra, i.e. a la geodivesidad ([Hjort et al. 2023](https://royalsocietypublishing.org/doi/10.1098/rsta.2023.0060)). Sin embargo, no hay un concenso sobre la integración de la pedodiversidad con otras expresiones de la geodiversidad, conceptualmente ([Ibañez & Brevik 2019](https://www.sciencedirect.com/science/article/abs/pii/S0341816219302528)) o metodológicamente, en particular con relación al estudio de patrones espaciales ([Ibañez & Brevik 2022](https://www.frontierspartnerships.org/journals/spanish-journal-of-soil-science/articles/10.3389/sjss.2022.10456/full)). Este rezago, no solo dificulta expresar de forma objetiva el soporte que tiene la geodiversidad para la biodiversidad ([Malienimi et al. 2024](https://onlinelibrary.wiley.com/doi/10.1111/ddi.13843), [Tukianen et al. 2022](https://www.lyellcollection.org/doi/full/10.1144/SP530-2022-107)), sino también la planeación y monitoreo sobre los servicios ecosistémicos interrelacionados ([Mikhailova et al. 2021](https://www.mdpi.com/2073-445X/10/3/288), [Fu et al. 2022](https://www.sciencedirect.com/science/article/abs/pii/S034181622200491X#:~:text=Pedodiversity%20can%20increase%20soil%20retention,indirect%20effects%20on%20ecosystem%20services.)) y el diseño de soluciones basadas en la naturaleza ([Schrodt et al. 2019](https://www.pnas.org/doi/10.1073/pnas.1911799116)). A pesar de que Colombia es un país reconocido por su biodiversidad, las expresiones e interacciones de la geodiversidad no han sido estudiadas de forma cuantitativa. En este sentido, *este estudio se enfoca en la diversidad de suelos -al estar la pedósfera en la intersección entre las demás esferas geoecosistémicas- y geoformas*, que en Colombia condicionan los estudios de suelos ([IGAC 2022](https://www.igac.gov.co/sites/default/files/listadomaestro/in-agr-pc02-05_elaboracion_de_cartografia_geomorfologica_0.pdf)).


Este estudio responde a la pregunta, ¿cómo ocurren espacialmente y se relacionan entre sí la diversidad de suelos y de geoformas? Para esto, se utilizan distintas metodologías de análisis espacial. La pedodiversidad se calcula mediante índices ([Ibañez & Pfeiffer 2023](https://www.sciencedirect.com/science/article/abs/pii/B9780128229743000045?via%3Dihub), [Daly et al. 2018](https://www.mdpi.com/2227-7390/6/7/119)), como la entropía de Shannon (H') y Rao (Q), con base en distancias taxonómicas-jerárquicas ([Rossiter et al. 2017](https://www.sciencedirect.com/science/article/abs/pii/S0016706116303901)) y proporciones de los suelos reportados dentro de polígonos de unidades cartográficas de suelos (UCS). Una vez asignados los valores a cada polígono, se analizan la dependencia espacial (e.g. con la I de Moran, semivariograma), hotspots/coldspots (e.g. LISA - Local Indicators of Spatial Association), así como  valores extremos mediante estadísticas descriptivas. Una vez identificados los valores extremos, se analizan como patrones de puntos, bajo la suposición de que su ocurrencia es aleatoria, producto de la interacción múltiples factores ambientales a través del tiempo. Finalmente, se utiliza un modelo de elevación digital (DEM)  para estimar diversidad geomorfológica, mediante la estimación de Q a partir de variables geomorfométricas rasterizadas ([Rocchini et al 2017](https://www.sciencedirect.com/science/article/abs/pii/S1470160X16304319)).   

### Los datos
1. La fuente de datos vectoriales es el Instituto Geográfico Agustin Codazzi (IGAC). Las UCS se utilizan en el marco conceptual desarrollado por ([Zinck et al. 2016](https://link.springer.com/book/10.1007/978-3-319-19159-1)), seguido en los levantamientos de suelos del IGAC 
([2022](https://www.igac.gov.co/sites/default/files/listadomaestro/in-agr-pc02-05_elaboracion_de_cartografia_geomorfologica_0.pdf)), según el cual la geomorfología ofrece el marco natural para estudiar la formación, evolución y distribución de suelos.
2. La fuente de datos de elevación rasterizados es el satélite SRTM de la NASA, de resolución de 30m, disponibles a través de [Google Earth Engine](https://developers.google.com/earth-engine/datasets/catalog/USGS_SRTMGL1_003?hl=es-419)

### Contenido del repositorio (en proceso)
- [Presentación del proyecto](https://cmguiob.github.io/pedodiv-colombia/Slides/Slides_Pedodiversidad-Colombia#/pedodiversidad-en-colombia)
- Script (R) para la carga masiva de datos de UCS via API
- Script (R) para el preprocesamiento de datos
- Script (js) para la carga de datos de elevación digital desde GEE
- Script (R) para la estimación de métricas de pedodiversidad 
- Script (R) para el análisis geoespacial de la pedodiversidad
- Script (R) para la estimación de métricas de diversidad geomorfológica
- Script (R) de análisis de la relación entre pedodiversidad y diversidad geomorfológica
- Figuras
