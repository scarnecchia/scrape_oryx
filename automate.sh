#!/usr/bin/env Rscript

export DAY=`date +'%Y-%m-%d'`

echo "Ensuring we have the latest version of the program"
git pull --no-ff origin master
git checkout website
git pull --no-ff origin website

echo "Running scraper"
Rscript scrape_oryx.R

echo "Publishing Website"
git add --all
git commit -m "Oryx Updates for ${DAY}"
git push origin website

echo "Publishing Datasets"
git checkout data
git add --all
git commit -m "Oryx Updates for ${DAY}"
git push origin data

echo "Done"