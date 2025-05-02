# Análisis geoespacial de pedodiversidad y su relación con variables geomorfométricas en Colombia

![estado](https://img.shields.io/badge/estado-en_progreso-lightgrey&?style=for-the-badge&color=%23EEC900) ![made-with-R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)

Este repositorio presenta un *análisis geoespacial de pedodiversidad* (i.e. diversidad de suelos) en Colombia. La pedodiversidad se calcula mediante los índices de pedodiversidad ([Ibañez & Pfeiffer 2023](https://www.sciencedirect.com/science/article/abs/pii/B9780128229743000045?via%3Dihub), [Daly et al. 2018](https://www.mdpi.com/2227-7390/6/7/119)), como la entropía de Shannon (H') y Rao (Q), con base en distancias taxonómicas-jerárquicas ([Rossiter et al. 2017](https://www.sciencedirect.com/science/article/abs/pii/S0016706116303901)) y proporciones de los suelos reportados dentro de polígonos de unidades cartográficas de suelos (UCS). La fuente de datos es el Instituto Geográfico Agustin Codazzi (IGAC). 

**Este repositorio contiene**:
- [Presentación del proyecto](https://cmguiob.github.io/pedodiv-colombia/2025_SLIDES_Pedodiversidad-Colombia.html#/pedodiversidad-en-colombia)
- Conjunto de datos (para pruebas) de UCS
- Script (R) para la carga masiva de datos de UCS via API
- Script (R) para la estimación de métricas de pedodiversidad 
- Script (R) para el análisis geoespacial de la pedodiversidad
- Figuras
