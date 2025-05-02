# Análisis geoespacial de pedodiversidad y diversidad geomorfológica en Colombia
![estado](https://img.shields.io/badge/estado-en_progreso-lightgrey&?style=for-the-badge&color=%23EEC900) ![made-with-R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)

---
**¿Cómo ocurren espacialmente y se relacionan entre sí estas dos expresiones de la geodiversdad?**
---

Este repositorio presenta un *análisis geoespacial de pedodiversidad* (i.e. diversidad de suelos) en Colombia. La pedodiversidad se calcula mediante los índices ([Ibañez & Pfeiffer 2023](https://www.sciencedirect.com/science/article/abs/pii/B9780128229743000045?via%3Dihub), [Daly et al. 2018](https://www.mdpi.com/2227-7390/6/7/119)), como la entropía de Shannon (H') y Rao (Q), con base en distancias taxonómicas-jerárquicas ([Rossiter et al. 2017](https://www.sciencedirect.com/science/article/abs/pii/S0016706116303901)) y proporciones de los suelos reportados dentro de polígonos de unidades cartográficas de suelos (UCS). Una vez asignados los valores a cada polígono, se se analizan la dependencia espacial (e.g. con la I de Moran, semivariograma), hotspots/coldspots (e.g. LISA - Local Indicators of Spatial Association), así como la diferenciación de valores extremos mediante estadísticas descriptivas. La identificación de valores extremos se analiza mediante el análisis de patrones de puntos, bajo la suposición de que su ocurrencia es aleatoria, producto de la interacción múltiples factores ambientales a través del tiempo. Finalmente, se utiliza un modelo de elevación digital (DEM)  para estimar diversidad geomorfológica, mediante la estimación de Q a partir de índices geomorfométricos rasterizados ([Rocchini et al 2017](https://www.sciencedirect.com/science/article/abs/pii/S1470160X16304319)).   

### Los datos
1. La fuente de datos vectoriales es el Instituto Geográfico Agustin Codazzi (IGAC). Las UCS se utilizan en el marco conceptual desarrollado por ([Zinck et al. 2016](https://link.springer.com/book/10.1007/978-3-319-19159-1)), seguido en los levantamientos de suelos del IGAC 
([2022](https://www.igac.gov.co/sites/default/files/listadomaestro/in-agr-pc02-05_elaboracion_de_cartografia_geomorfologica_0.pdf)), según el cual la geomorfología ofrece el marco natural para estudiar la formación, evolución y distribución de suelos.
2. La fuente de datos de elevación rasterizados es el satélite SRTM de la NASA, de resolución de 30m, disponibles a través de ([Google Earth Engine](https://developers.google.com/earth-engine/datasets/catalog/USGS_SRTMGL1_003?hl=es-419))

### Contenido del repositorio (en proceso)
- [Presentación del proyecto](https://cmguiob.github.io/pedodiv-colombia/2025_SLIDES_Pedodiversidad-Colombia.html#/pedodiversidad-en-colombia)
- Conjunto de datos (para pruebas) de UCS
- Script (R) para la carga masiva de datos de UCS via API
- Script (js) para la carga de datos de elevación digital desde GEE
- Script (R) para la estimación de métricas de pedodiversidad 
- Script (R) para el análisis geoespacial de la pedodiversidad
- Script (R) para la estimación de métricas de digersidad geomorfológica
- Script (R) de análisis de la relación entre pedodiversidad y diversidad geomorfológica
- Figuras
