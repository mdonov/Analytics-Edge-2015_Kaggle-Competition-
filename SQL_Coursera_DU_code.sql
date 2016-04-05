
--What is the city and state of the store which had the greatest increase in average daily revenue  from November to December?

SELECT sub.store, sub.MonthlyChange, str.city, str.state

FROM

(SELECT
         tr.store,   
         COUNT (distinct
         (Case WHEN extract(MONTH from tr.saledate)=11 then tr.saledate END)) nDaysNov,
         count(distinct
         (Case WHEN extract(MONTH from tr.saledate)=12 then tr.saledate END)) nDaysDec,

         SUM (case WHEN extract(MONTH from tr.saledate)=11 then tr.amt END) as NovRev,
         SUM (case WHEN extract(MONTH from tr.saledate)=12 then tr.amt END) as DecRev,
         NovRev/nDaysNov as avgNov,
         DecRev/nDaysDec as avgDec,
         (avgDec -avgNov) as MonthlyChange

FROM trnsact tr

WHERE tr.stype='P' and NOT(extract(MONTH from tr.saledate)=8 and extract(year from tr.saledate)=2005)
group by tr.store
having nDaysDec>=20 and nDaysNov>=20) as sub

JOIN strinfo as str
on str.store=sub.store
group by sub.store, sub.MonthlyChange, str.city, str.state
order by sub.MonthlyChange desc
