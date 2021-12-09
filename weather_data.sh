## ftp://ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt
cd ~/NSO/projects/preanalytical/card/lot_analysis/data/
rm -r weather
mkdir weather
cd weather
# ## toronto intl A  StationID 51459
# ## Barrie Oro 42183

for year in `seq 2014 2019`
do 
    wget --content-disposition "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=42183&Year=${year}&Month=1&Day=14&timeframe=2&submit=Download+Data" 
done

rm temp.csv
for var in *.csv
do
    #    sed -n '/^"Date/,$p' "$var" | tail -n +2 >> temp.csv
    tail -n +2  "$var" >> temp.csv
done
