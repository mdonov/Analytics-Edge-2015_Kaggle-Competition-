--What is the city and state of the store which had the greatest increase in average daily revenue  from November to December?

SELECT sub.store, sub.MonthlyChange, str.city, str.state

FROM

(SELECT
         tr.store,   
         COUNT (DISTINCT
         (Case WHEN EXTRACT(MONTH from tr.saledate)=11 then tr.saledate END)) nDaysNov,
         COUNT(DISTINCT
         (Case WHEN EXTRACT(MONTH from tr.saledate)=12 then tr.saledate END)) nDaysDec,

         SUM (case WHEN EXTRACT(MONTH from tr.saledate)=11 then tr.amt END) as NovRev,
         SUM (case WHEN EXTRACT(MONTH from tr.saledate)=12 then tr.amt END) as DecRev,
         NovRev/nDaysNov as avgNov,
         DecRev/nDaysDec as avgDec,
         (avgDec -avgNov) as MonthlyChange
FROM trnsact tr
WHERE tr.stype='P' and NOT(EXTRACT(MONTH from tr.saledate)=8 and EXTRACT(year from tr.saledate)=2005)
GROUP BY tr.store
HAVING nDaysDec>=20 and nDaysNov>=20) as sub

JOIN strinfo as str
ON str.store=sub.store
GROUP BY sub.store, sub.MonthlyChange, str.city, str.state
ORDER BY sub.MonthlyChange DESC;




--Compare the average daily revenue of the store with the highest msa_income and the store with the lowest median 
--msa_income (according to the msa_income field). In what city and state were these two stores, and which store had 
-- a higher average daily revenue? 

SELECT SUM(rads.tRevenue)/SUM(rads.ndays) as DayAverg,store_msa.msa_income Med_Income, store_msa.state, store_msa.city

FROM 
   (SELECT
    store,
    EXTRACT(month from saledate) as m,
    EXTRACT(year from saledate) y,
    COUNT(DISTINCT saledate) nDays,
    SUM(amt) tRevenue,
    tRevenue/nDays AVGdailyRev
    FROM trnsact
    WHERE stype='P' and not (m=8 and y=2005)
    GROUP BY  store,m,y
    HAVING ndays>20) as Rads

JOIN store_msa
ON store_msa.store= rads.store

WHERE store_msa.msa_income IN ((SELECT MAX(msa_income) FROM store_msa), (SELECT MIN(msa_income)FROM store_msa))

GROUP BY Med_Income, store_msa.state,store_msa.city







