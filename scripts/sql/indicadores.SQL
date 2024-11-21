SELECT
    d.ANO,
    s.SETOR,
    d.SETOR_ATIV,
    round(sum(d.Receitas)) as Receitas
FROM dva_igpdi d
JOIN setores s on d.SETOR_ATIV = s.SETOR_ATIV
WHERE d.ANO IN (2009, 2023)
GROUP BY d.ANO, s.SETOR, d.SETOR_ATIV;