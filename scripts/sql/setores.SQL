SELECT 
    d.ANO,
    s.SETOR,
    d.SETOR_ATIV,
    d.Receitas,
    d.Governo,
    d.Pessoal,
    d.RCT AS Credores,
    d.RCP AS Acionistas
FROM dva_igpdi AS d
LEFT JOIN setores AS s
ON s.SETOR_ATIV = d.SETOR_ATIV;


SELECT 
    d.ANO,
    s.SETOR,
    ROUND(SUM(d.Receitas)) Receitas,
    ROUND(SUM(d.VAB)) VAB,
    ROUND(SUM(d.VATD)) VATD,
    ROUND(SUM(d.Governo)) Governo,
    ROUND(SUM(d.Pessoal)) Pessoal,
    ROUND(SUM(d.RCT)) Credores,
    ROUND(SUM(d.RCP)) Acionistas
FROM dva_igpdi d
RIGHT JOIN setores s
ON s.SETOR_ATIV = d.SETOR_ATIV
WHERE ANO in ('2009','2016','2023')

GROUP BY ANO, SETOR;


SELECT 
    d.ANO,
    s.SETOR,
    ROUND(SUM(d.Receitas)) Receitas,
    ROUND(SUM(d.VAB)) VAB,
    ROUND(SUM(d.VATD)) VATD,
    ROUND(SUM(d.Governo)) Governo,
    ROUND(SUM(d.Pessoal)) Pessoal,
    ROUND(SUM(d.RCT)) Credores,
    ROUND(SUM(d.RCP)) Acionistas
FROM dva_igpdi d
RIGHT JOIN setores s
ON s.SETOR_ATIV = d.SETOR_ATIV
WHERE ANO in ('2009','2016','2023')

GROUP BY ANO;

SELECT
    
    ANO Tempo,
    (CD_CVM || "-" || ANO) Individuos,
    Receitas * 0.000000001 Receitas,
    Pessoal * 0.000000001 Pessoal,
    Governo * 0.000000001 Governo,
    RCT * 0.000000001 Credores,
    RCP * 0.000000001 Acionistas

FROM dva_contas;