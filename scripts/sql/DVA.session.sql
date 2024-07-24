-- Consulta serie temporal V.1
SELECT 

    ANO,
    SUM(Governo)/SUM(VATD) * 100 AS Governo,
    SUM(Pessoal)/SUM(VATD) * 100 AS Pessoal,
    SUM(RCP)/SUM(VATD) * 100 AS RCP,
    SUM(RCT)/SUM(VATD) * 100 AS RCT

FROM dva_contas
GROUP BY ANO;

-- Consulta serie temporal V.2
SELECT 
    ANO,
    'Governo' AS Indicador,
    SUM(Governo)/SUM(VATD) * 100 AS Valor
FROM dva_contas
GROUP BY ANO

UNION ALL

SELECT 
    ANO,
    'Pessoal' AS Indicador,
    SUM(Pessoal)/SUM(VATD) * 100 AS Valor
FROM dva_contas
GROUP BY ANO

UNION ALL

SELECT 
    ANO,
    'RCP' AS Indicador,
    SUM(RCP)/SUM(VATD) * 100 AS Valor
FROM dva_contas
GROUP BY ANO

UNION ALL

SELECT 
    ANO,
    'RCT' AS Indicador,
    SUM(RCT)/SUM(VATD) * 100 AS Valor
FROM dva_contas
GROUP BY ANO

ORDER BY ANO;
